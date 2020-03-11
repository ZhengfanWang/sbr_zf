library(rstan)

#sub_validationoutput.R
fit <- readRDS(file = "rdsoutput/new/tun1.rds")
standata <- readRDS(file = "output/stan_data/hs_nval.rds")

array <- extract(fit)

#####################################################
#          USE Greg's code
#####################################################
# create dataset
country_code <- standata$getc_i
region_code <- standata$getr_c[standata$getc_i]
year <- standata$gett_i + 1999
source <- standata$getj_i
y <- standata$Y

array <- rstan::extract(fit)

predictor <- matrix(NA,ncol = standata$numcov, nrow = standata$N)
for(cov in 1:standata$numcov){
  for(k in 1:standata$ntrain){
    predictor[k,cov] <- standata$covar_array[cov,
                                             standata$getc_i[standata$getitrain_k[k]],
                                             standata$gett_i[standata$getitrain_k[k]]]
  }}
predictor <- as.data.frame(predictor)
colnames(predictor) <- c("gni_sm","nmr","lbw_sm","anc4_sm","mean_edu_f_sm",
                         "gini_sm","urban","gfr","sab","anc1_sm","abr_sm",
                         "csec_sm","pab_sm","pfpr","gdp","mmr")
total_sd <- apply(array$sigma_i,2,median)
percent <- t(apply(array$prep,2,quantile,probs = c(0.025,0.1,0.5,0.9,0.975)))

rv_data_raw <- data.frame(country_code = standata$getc_i,
                          region_code = standata$getr_c[standata$getc_i],
                          year = standata$gett_i + 1999,
                          source = standata$getj_i,
                          y = standata$Y,
                          total_sd = apply(array$sigma_i,2,median))
rv_data <- cbind(rv_data_raw,predictor,percent)


rv_data %>% head() 
residuals(data = rv_data,
          y = "y",
          yhat = "50%",
          total_standard_error = "total_sd") 

residuals(data = rv_data,
          y = "y",
          yhat = "50%",
          total_standard_error = "total_sd",
          subset = "source") 

residuals_autoplot(
  data = rv_data,
  y = "y",
  yhat = "50%"
)$lbw_sm

# there might be some bugs here. 
coverage(data = rv_data,
         y = "y",
         lower = "2.5%",
         upper = "97.5%")

# rewrite the function here 
 rv_data %>%
  dplyr::mutate(out = as.numeric(y < lower | y > upper)) %>% 
  dplyr::select(source, out) %>% 
  dplyr::summarise("coverage" = 1 - (sum(out)/nrow(.)))

 
rv_data %>%
   dplyr::mutate(out = as.numeric(y < lower | y > upper)) %>% 
   dplyr::select(source, out) %>% 
   dplyr::group_by(source) %>% 
   dplyr::summarise("coverage" = 1 - (sum(out)/nrow(.))) 
 
################ use loo package########

library("rstan")
library("rstanarm")
library("bayesplot")
library("loo")
fit <- readRDS(file = "rdsoutput/new/base.rds")
fit2 <- readRDS(file = "rdsoutput/new/tun1.rds")
loo <- loo(fit, save_psis = TRUE, cores = 4)
loo2 <- loo(fit2, save_psis = TRUE, cores = 4)
print(loo)
plot(loo)

array <- rstan::extract(fit2)
yrep <- array$prep
psis <- loo$psis_object
lw <- weights(psis)
y <- standata$Y
color_scheme_set("orange")
ppc_loo_pit_overlay(y, yrep, lw = lw)
loo_compare(list(loo, loo2))


