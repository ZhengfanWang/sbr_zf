

#input stan data list which store in output/stan_data.4 types of data here
# 1. do hs prior with validation "hs_val.rds"
# 2. do hs prior without validation "hs_nval.rds"
# 3. base model with validation "nhs_val.rds"
# 4. base model without validation "nhs_val.rds"

#example
stan.data <- readRDS("input/hs_val.rds")

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

#model store in model file. 4 types model so far.
#1. "base.stan"
#2. "base_t.stan"
#3. "reg_hs.stan"
#4. "reg_hs_t.stan"

# and the setting for the stan model:chains = 4,
# control=list(adapt_delta=0.99, max_treedepth=15
fit<- rstan::stan(file= "mod/reg_hs.stan",data=stan.data,chains = 4,
                  control=list(adapt_delta=0.99, max_treedepth=15))

saveRDS(fit,file = "rdsoutput/reg_hs_val.rds")





