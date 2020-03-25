library(rstan)
rds.name <- "validation/hs_val_ran20_res202000319.rds"
data.name <- "validation/hs_val_ran20.rds"
fit <- readRDS(file = paste0("rdsoutput/",rds.name))
stan.data <- readRDS(file = paste0("output/stan_data/",data.name))

array <- rstan::extract(fit)

# create dataset
predictor <- matrix(NA,ncol = stan.data$numcov, nrow = stan.data$N)
for(cov in 1:stan.data$numcov){
  for(k in 1:stan.data$ntrain){
    predictor[k,cov] <- stan.data$covar_array[cov,
                                             stan.data$getc_i[stan.data$getitrain_k[k]],
                                             stan.data$gett_i[stan.data$getitrain_k[k]]]
  }}
predictor <- as.data.frame(predictor)
colnames(predictor) <- c("gni_sm","nmr","lbw_sm","anc4_sm","mean_edu_f_sm",
                         "gini_sm","urban","gfr","sab","anc1_sm","abr_sm",
                         "csec_sm","pab_sm","pfpr","gdp","mmr")

percent <- t(apply(array$prep,2,quantile,probs = c(0.025,0.1,0.5,0.9,0.975)))
colnames(percent) <- c("ci5","ci10","yhat","ci90","ci97.5")
rv_data_raw <- data.frame(country_idx = stan.data$getc_i,
                          region_code = stan.data$getr_c[stan.data$getc_i],
                          year = stan.data$gett_i + 1999,
                          source = stan.data$getj_i,
                          y = stan.data$Y,
                          stdev = apply(array$sigma_i,2,median))
rv_data <- cbind(rv_data_raw,predictor,percent) %>%
           merge(countryRegionList, by="country_idx") %>% 
           mutate(residual = y - yhat,
                  standardized_residual = residual/stdev)

# indices of test set
getitest <- setdiff(seq(1,stan.data$N), stan.data$getitrain_k)
ntest <- length(getitest)

# PIT
pit.j <- rep(NA, ntest)
for (j in 1:ntest){
  i <- getitest[j]
  yrepi.s <- array$prep[,i]
  pit.j[j] <- mean(yrepi.s <= stan.data$Y[i])
} 

#create test set 
rv_data_test <- cbind(rv_data[getitest,],pit.j) 


rv_data_test %>% summarise(mean_residual = mean(residual),
                           median_residual = median(residual),
                           abs_error = mean(abs(residual)),
                           mean_standardize_residual = mean(standardized_residual)
                           )


hist(pit.j, freq = F, xlab = "PIT-values", main = "Predicting last obs")  # should look uniform
abline(h=1)  

p <- 0.1
qbinom(c(0.1, 0.9), size = ntest, prob = p)/ntest # 80% PI for prop of left out obs in one bin of PIT values with range of p 
qbinom(c(0.25, 0.75), size = ntest, prob = p)/ntest # 50% PI for prop of left out obs in one bin of PIT values with range of p 
p <- 0.25
qbinom(c(0.1, 0.9), size = ntest, prob = p)/ntest # 80% PI for prop of left out obs in one bin of PIT values with range of p 
qbinom(c(0.25, 0.75), size = ntest, prob = p)/ntest # 50% PI for prop of left out obs in one bin of PIT values with range of p 

# coverage follows from pit
mean(pit.j < 0.025) # % below 95% PI
mean(pit.j < 0.05) # % below 90% PI
mean(pit.j < 0.1)
mean(pit.j > 0.975) # % above 95% PI 
mean(pit.j > 0.95) 
mean(pit.j > 0.9)


#####################################################
#          USE Greg's code
#####################################################

# create dataset
country_code <- stan.data$getc_i
region_code <- stan.data$getr_c[stan.data$getc_i]
year <- stan.data$gett_i + 1999
source <- stan.data$getj_i
y <- stan.data$Y


predictor <- matrix(NA,ncol = stan.data$numcov, nrow = stan.data$N)
for(cov in 1:stan.data$numcov){
  for(k in 1:stan.data$ntrain){
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

rv_data_raw <- data.frame(country_code = stan.data$getc_i,
                          region_code = stan.data$getr_c[stan.data$getc_i],
                          year = stan.data$gett_i + 1999,
                          source = stan.data$getj_i,
                          y = stan.data$Y,
                          total_sd = apply(array$sigma_i,2,median))
rv_data <- cbind(rv_data_raw,predictor,percent)


rv_data_test <- rv_data[getitest,] 
rv_data_test %>% head()
residuals(data = rv_data_test,
          y = "y",
          yhat = "50%",
          total_standard_error = "total_sd") 

residuals(data = rv_data,
          y = "y",
          yhat = "50%",
          total_standard_error = "total_sd",
          subset = "source") 

residuals_autoplot(
  data = rv_data_test,
  y = "y",
  yhat = "50%"
)$lbw_sm



1-sum(as.numeric(y[getitest] < rv_data_test$`2.5%` | y[getitest] > rv_data_test$`97.5%`))/(stan.data$N-stan.data$ntrain)
 
 

 lower <- rv_data$`2.5%`
 upper <- rv_data$`97.5%`

 data<- rv_data %>%
   dplyr::mutate(out = as.numeric(y < lower | y > upper)) %>% 
   dplyr::select(source, out) %>% 
   dplyr::summarise("coverage" = 1 - (sum(out)/nrow(.)))

 
