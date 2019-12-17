library("rstanarm")
library("bayesplot")
library("loo")
fit.ref <- readRDS("rdsoutput/qi1.hs.rds")
standata <- readRDS("output/stan.qi1.hs.rds")
chain <- rstan::extract(fit.ref)
dim(chain$log_lik)
loo.ref <- loo(fit.ref)
print(loo.ref)
plot(loo.ref)

y <- standata$Y
yrep <- chain$prep
loo1 <- loo(fit.ref, save_psis = TRUE, cores = 4)
psis1 <- loo1$psis_object
lw <- weights(psis1)
color_scheme_set("orange")
ppc_loo_pit_overlay(y, yrep, lw = lw)
