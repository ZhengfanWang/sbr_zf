library(rstan)

#sub_validationoutput.R
hs <- readRDS(file = "rdsoutput/new/tun1.rds")
standata <- readRDS(file = "output/stan_data/hs_nval.rds")

array <- extract(fit)
# indices of test set
getitest <- setdiff(seq(1,stan.data$N), stan.data$getitrain_k)

# errors
pred <- apply(array$prep,2,median)
errors <- stan.data$Y[getitest] - pred[getitest]
hist(errors)
# summarize
mean(errors)  
median(errors)  
mean(abs(errors))
median(abs(errors))


ntest <- length(getitest)
ntest
# PIT
pit.j <- rep(NA, ntest)
for (j in 1:ntest){
  i <- getitest[j]
  yrepi.s <- array$prep[,i]
  pit.j[j] <- mean(yrepi.s <= stan.data$Y[i])
} 
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
country_code <- standata$getc_i
region_code <- standata$getr_c[standata$getc_i]
year <- standata$gett_i + 1999
source <- standata$getj_i
y <- standata$Y

array1 <- rstan::extract(base)
array2 <- rstan::extract(hs)
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
total_sd <- apply(array2$sigma_i,2,median)
percent <- t(apply(array2$prep,2,quantile,probs = c(0.025,0.1,0.5,0.9,0.975)))

rv_data_raw <- data.frame(country_code = standata$getc_i,
                          region_code = standata$getr_c[standata$getc_i],
                          year = standata$gett_i + 1999,
                          source = standata$getj_i,
                          y = standata$Y,
                          total_sd = apply(array2$sigma_i,2,median))
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

coverage(data = rv_data,
         y = "y",
         lower = "2.5%",
         upper = "97.5%",
         subset = "source")
table(standata$getj_i)

1-sum(as.numeric(y < rv_data$`2.5%` | y > rv_data$`97.5%`))/standata$N
 
 

 lower <- rv_data$`2.5%`
 upper <- rv_data$`97.5%`

 data<- rv_data %>%
   dplyr::mutate(out = as.numeric(y < lower | y > upper)) %>% 
   dplyr::select(source, out) %>% 
   dplyr::summarise("coverage" = 1 - (sum(out)/nrow(.)))

 
