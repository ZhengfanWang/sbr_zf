standata <- readRDS(file = "output/stan_data/nhs_nval.rds")     #stan data used for fit model
fit <- readRDS(file = "rdsoutput/base_nval.rds")       #stan fit 
mcmc.array <- rstan::extract(fit)
#mcmc.array2 <- rstan::extract(fit2)
print(fit,pars = c("beta","bias_dt","sigma_j","gamma_r"))
#--------------------------------#
#     source type summary table  #
#--------------------------------#
source_type <- c("admin","HMIS","subnat LR","survey")
source_type_bias <- c(0,0,0,median(mcmc.array$bias_dt))
source_type_sd <- sqrt(apply(mcmc.array$var_j,2,median))        
## note: current assumption: 0 bias for admin and subnat LR. estimate bias for HMIS and survey with prior N(0,5^2)T[,0]
##                           estimate source type variance
source_summ <- data.frame(source = source_type, 
                          bias = round(source_type_bias,digits = 3),
                          sd = round(source_type_sd,digits = 3))
source_summ
write.csv(source_summ,"output/result/source summary.csv")

hist(mcmc.array$bias_dt,main = "posterior sample survey bias",xlab = "posterior sample",breaks = 40)

#--------------------------------#
#  summary     covariates table  #
#--------------------------------#
hs <- F
int_cov <- c("gni","nmr","lbw","anc4","mean_edu_f")
if(hs == TRUE){
  int_cov <- c("gni","nmr","lbw","anc4","mean_edu_f",
               "gini","urban","gfr","sab","anc1","abr",
               "csec","pab")
}

betas <- round(apply(mcmc.array$beta,2,median),digits = 3)
sd <- round(apply(mcmc.array$beta,2,sd),digits = 3)
ci <- t(round(apply(mcmc.array$beta,2,quantile,c(0.025,0.975)),digits = 3))
covar_summ <- as.data.frame(cbind(int_cov,betas,sd,ci))
colnames(covar_summ) <- c("covariates","estimates","sd","2.5%","97.%%")
covar_summ

write.csv(covar_summ,"output/result/HS covariates summary.csv")

#--------------------------------#
#  summary   regional intercept  #
#--------------------------------#
print(fit,pars = c("gamma_r"))
gamma_r <-  round(apply(mcmc.array$gamma_r,2,median),digits = 3)
sd_gamma <- round(apply(mcmc.array$gamma_r,2,sd),digits = 3)
ci_gamma <- t(round(apply(mcmc.array$gamma_r,2,quantile,c(0.025,0.975)),digits = 3))
reg_summ <- as.data.frame(cbind(c("1","2","3","4","5","6"),gamma_r,sd_gamma,ci_gamma))
colnames(reg_summ) <- c("sdg region","estimates","sd","2.5%","97.%%")
reg_summ

write.csv(covar_summ,"output/result/HS covariates summary.csv")

#------------------------------------------------#
#   country -year estimates and 95% uncertainity  #
#------------------------------------------------#
mu_ct <- mcmc.array$mu_ct +mcmc.array$delta_ct                                  ### est mean

muhat <-c()
for(c in 1:standata$numcountry){
  for(t in 1:standata$yearLength){
    cache_mu_ct <- quantile(mu_ct[,c,t],c(0.025,0.5,0.975))
    cache_covar <- cache_mu_ct 
    cache_exp_mu_rt <- exp(cache_covar)
    muhat <- rbind(muhat,cache_exp_mu_rt)
  }
}
#muhat
colnames(muhat) <- c("low","muhat","up")

getr_c <- standata$getr_c   
estyears <- standata$estyears
year <- rep(estyears,times=standata$numcountry)

countryiso <- countryRegionList$iso                     #### run "iso.R" to get countryRegionList
yearLength <- length(estyears)
iso <- rep(countryiso,each= yearLength)
muhat <- data.frame(muhat,year,iso)

country.list <- list()
for(c in 1:standata$numcountry){
  start.p <- (c-1)*yearLength+1
  country.list[[c]] <- muhat[start.p:(start.p+yearLength-1),] %>% mutate(country = countryRegionList$country[c])
}

fit_result <- country.list[[1]]
for(c in 2:standata$numcountry){
  fit_result <- rbind(fit_result,country.list[[c]])  
}

fit_result <- fit_result %>% select(country,iso,year,low,muhat,up)

write.csv(fit_result,"output/result/base.csv")
