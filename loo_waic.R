library("rstanarm")
library("bayesplot")
library("loo")
fit1 <- readRDS("rdsoutput/Pspline1.rds")
fit2 <- readRDS("rdsoutput/Pspline2.rds")
fit3 <- readRDS("rdsoutput/ar1.rds")
loo.p1 <- loo(fit1)
loo.p2 <- loo(fit2)
loo.ar1 <- loo(fit3)
print(loo.p1)
print(loo.p2)
print(loo.ar1)

plot(loo.p1)
plot(loo.p2)
plot(loo.ar1)

loo::compare(loo.p1, loo.ar1) 
#The difference in ELPD is much larger than twice the estimated standard
#error again indicating that the ar1 model is expected to have better predictive performance than the P1 model. 

loglik.p1 <- loo::extract_log_lik(fit1)
loglik.ar1 <- loo::extract_log_lik(fit3)
waic.p1 <- waic(loglik.ar1)
waic.ar1 <- waic(loglik.p1)

print(loo::compare(waic.p1, waic.ar1), digits = 2)
