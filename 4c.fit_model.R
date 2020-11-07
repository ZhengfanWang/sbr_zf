

#read stan data
#stan.data <- readRDS("output/stan_data/base.rds")

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


if(hs <- F){
fit<- rstan::stan(file= "mod/base.stan",data=stan.data,chains = 4,
                  warmup = 1000, iter = 2000,
                  control=list(adapt_delta=0.99, max_treedepth=15))
}else{
fit<- rstan::stan(file= "mod/hs_tau.stan",data=stan.data,chains = 1,
                  control=list(adapt_delta=0.99, max_treedepth=15))}

#make sure to change the file name to save.
saveRDS(fit,file = "rdsoutput/base_20200907.rds")