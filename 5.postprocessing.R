fit <- readRDS("rdsoutput/qi1.rds")
standata <- readRDS("output/stan.qi1.rds")
mcmc.array <- rstan::extract(fit)


#--------------------------------#
#     source type summary table  #
#--------------------------------#
source_type <- c("admin","HMIS","subnat LR","survey")
source_type_bias <- c(0,apply(mcmc.array$bias_dt,2,median))
source_type_sd <- sqrt(c(0.0025,apply(mcmc.array$var_j,2,median)))        
## note: current assumption: add 0.0025 for variance of admin data
source_summ <- data.frame(source = source_type, 
                          bias = round(source_type_bias,digits = 3),
                          sd = round(source_type_sd,digits = 3))
source_summ
#write.csv(source_summ,"table/source summary.csv")

#------------------------------------------------#
#   country -year estimates and 95% uncertainity  #
#------------------------------------------------#
mu_ct <- mcmc.array$mu_ct +mcmc.array$delta_ct                                  ### est mean

muhat <-c()
for(c in 1:195){
  for(t in 1:19){
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

#write.csv(fit_result,"output/qi1.csv")
