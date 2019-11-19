library("rstanarm")
library("bayesplot")
library("loo")
fit.ref <- readRDS("rdsoutput/ref_qi1.rds")
fit.add1 <- readRDS("rdsoutput/add1_qi1.rds")
fit.add2 <- readRDS("rdsoutput/add2_qi1.rds")

chain <- extract(fit.ref)
dim(chain$log_lik)
loo.ref <- loo(fit.ref)
loo.add1 <- loo(fit.add1)
loo.add2 <- loo(fit.add2)

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
