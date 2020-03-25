library("rstanarm")
library("bayesplot")
library("loo")
standata <- readRDS(file = "output/stan_data/hs_nval.rds")     #stan data used for fit model
fit <- readRDS(file = "rdsoutput/new/base.rds")       #stan fit 
chain <- rstan::extract(fit)
log_lik <- extract_log_lik(hs)
dim(hs.array$log_lik)
loo1 <- loo(hs, save_psis = TRUE, cores = 4)
print(loo1)
plot(loo1)

fit2 <- readRDS(file = "rdsoutput/new/lambda_df3.rds") 
chain2 <- rstan::extract(fit2)
loo2 <- loo(fit2, save_psis = TRUE, cores = 4)
log_lik2 <- extract_log_lik(fit2)

fit3 <- readRDS(file = "rdsoutput/new/tun1.rds") 
log_lik3 <- extract_log_lik(fit3)
loo3 <- loo(fit3, save_psis = TRUE, cores = 4)

fit4 <- readRDS(file = "rdsoutput/new/tau_sigma_j1.rds") 
log_lik4 <- extract_log_lik(fit4)
loo4 <- loo(fit4, save_psis = TRUE, cores = 4)

waic(log_lik)
waic(log_lik2)
waic(log_lik3)
waic(log_lik4)

print(loo_compare(loo3,loo2),digits=3)

y <- standata$Y
yrep <- chain$prep
psis1 <- loo1$psis_object
lw <- weights(psis1)
color_scheme_set("orange")
ppc_loo_pit_overlay(y, yrep, lw = lw)


#######
#   breaking LOO results down by source type
#----------------------------------------------------

loo_by_source <- function(source,dat,loo){
i <- dat$getj_i == source
y <- dat$Y[i]
yrep <- chain$prep[,i]
psis1 <- loo$psis_object
lw <- weights(psis1)[,i]
color_scheme_set("orange")
ppc_loo_pit_overlay(y, yrep, lw = lw)
}

loo_by_def <- function(def,dat,loo){
  i <- dat$getj_i == def
  y <- dat$Y[i]
  yrep <- chain$prep[,i]
  psis1 <- loo$psis_object
  lw <- weights(psis1)[,i]
  color_scheme_set("orange")
  ppc_loo_pit_overlay(y, yrep, lw = lw)
}

# admin data
length(which(standata$getj_i == 1))
loo_by_source(source=1,dat = standata,loo=loo1)

# HMIS
length(which(standata$getj_i == 2))
loo_by_source(source=2,dat = standata,loo=loo1)

#subnat LR
length(which(standata$getj_i == 3))
loo_by_source(source=3,dat = standata,loo=loo1)

# survey
length(which(standata$getj_i == 4))
loo_by_source(source=4,dat = standata,loo=loo1)
#--------------------------------------------------------

length(which(standata$getd_i == 1))
loo_by_def(def=1,dat = standata,loo=loo1)

length(which(standata$getd_i == 2))
loo_by_def(def=2,dat = standata,loo=loo1)

length(which(standata$getd_i == 3))
loo_by_def(def=3,dat = standata,loo=loo1)

length(which(standata$getd_i == 4))
loo_by_def(def=4,dat = standata,loo=loo1)

