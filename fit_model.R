
stan.data<-readRDS("output/stan.quad.I1.5.rds")
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

fit<- rstan::stan(file= "mod/Pspline1.stan",data=stan.data,chains = 1,
                  control=list(adapt_delta=0.99, max_treedepth=15))

saveRDS(fit,file = "rdsoutput/base_model_standardize.rds")

print(fit,pars = c("beta","beta_dt","rho","sigma_ar","sigma_j"))