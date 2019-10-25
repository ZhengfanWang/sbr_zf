
stan.data<-readRDS("output/standata.quad.I1.rds")
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

fit<- rstan::stan(file= "mod/spline1_hs.stan",data=stan.data,chains = 1,
                  control=list(adapt_delta=0.99, max_treedepth=15))

saveRDS(fit,file = "rdsoutput/base_model_standardize.rds")



