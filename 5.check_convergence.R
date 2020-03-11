fit <- readRDS("rdsoutput/new/lambda_df3.rds")

library(rstan)
# check SD parameters
print(fit,pars = c("sigma_c","sigma_r","sigma_j","tau_delta"))
traceplot(fit,pars = c("sigma_c","sigma_r","sigma_j","tau_delta"))

# check HS 
print(fit,pars = c("lambda","tau","caux"))
traceplot(fit,pars = c("lambda","tau","caux"))

# check mean parameter for intercept
print(fit,pars = c("gamma_w","gamma_r"))
traceplot(fit,pars = c("gamma_w","gamma_r"))

# check bates
print(fit, pars = c("beta"))
traceplot(fit, pars = c("beta"))
