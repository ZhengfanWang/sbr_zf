library(rstan)

fit <- readRDS("rdsoutput/reg_hs_nval.rds")
#fit <- readRDS("rdsoutput/reg_hs_nval_t.rds")

print(fit, pars = c("beta","sigma_j","gamma_r","tau_delta"))
traceplot(fit, pars = c("beta","sigma_j","gamma_r","tau_delta"))
traceplot(fit, pars = c("tau_delta"))
          