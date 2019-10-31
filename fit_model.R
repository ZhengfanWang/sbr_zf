
stan.data<-readRDS("output/stan.qi1.hs.rds")
stan.data$N
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

fit<- rstan::stan(file= "mod/p1_loo.stan",data=stan.data,chains = 1,
                  control=list(adapt_delta=0.99, max_treedepth=15))

fit<- rstan::stan(file= "mod/p1_hs.stan",data=stan.data,chains = 1,
                  control=list(adapt_delta=0.99, max_treedepth=15))

stan.data.loo <- readRDS("output/stan.loo.rds")
fit.loo <- rstan::stan(file= "mod/P1.loo.stan",data=stan.data.loo,chains = 1,
                  control=list(adapt_delta=0.99, max_treedepth=15))

saveRDS(fit,file = "rdsoutput/base_model_standardize.rds")



