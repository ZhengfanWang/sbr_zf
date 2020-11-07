library("rstanarm")
library("bayesplot")
library("loo")
standata <- readRDS(file = "output/stan_data/base_nval_20200907.rds")     #stan data used for fit model
fit <- readRDS(file = "rdsoutput/new/hs_nval_res20200907.rds")       #stan fit 
chain <- rstan::extract(fit)
log_lik <- extract_log_lik(fit)
dim(log_lik)
loo1 <- loo(log_lik, save_psis = TRUE, cores = 8)
print(loo1)
plot(loo1)
remove(fit)

fit2 <- readRDS(file = "rdsoutput/new/base_20200907.rds") 
chain2 <- rstan::extract(fit2)
log_lik2 <- extract_log_lik(fit2)
loo2 <- loo(log_lik2, save_psis = TRUE, cores = 4)
loo2
remove(fit2)



pdf_name <- paste0("fig/psis.pdf")
pdf(pdf_name, width = 12, height = 7)
plot(loo2,main = "subsetted model")
plot(loo1,main = "HS tau0=1")
dev.off()

loo1 <- loo(log_lik)
loo1
loo2 <- loo(log_lik2)
loo2


compare(loo2,loo1)


print(loo_compare(loo1,loo2),digits=3)

y <- standata$Y
yrep <- chain$prep
psis1 <- loo1$psis_object
lw <- weights(psis1)
color_scheme_set("orange")
ppc_loo_pit_overlay(y, yrep, lw = lw)



