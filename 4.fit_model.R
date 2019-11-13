


stan.data<-readRDS("output/stan_data/stan.qi1.hs.rds")
stan.data$ntrain
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

fit<- rstan::stan(file= "mod/q1_hs.stan",data=stan.data,chains = 1,
                  control=list(adapt_delta=0.99, max_treedepth=15))

fit<- rstan::stan(file= "mod/q1.stan",data=stan.data,chains = 1,
                  control=list(adapt_delta=0.99, max_treedepth=15))


saveRDS(fit,file = "rdsoutput/base_model_standardize.rds")



