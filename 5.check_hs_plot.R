

source('R/stan_utility.R')
source('R/plot_utility.R')
source('R/plot_ran_int.R')
library(rstan)

stan.data <- readRDS(file = "output/stan_data/stan.qi1.hs.rds")
fit <- readRDS(file = "rdsoutput/1030rdsoutput/qi1.hs.rds")

beta_true <- trueB <- rep(0,dim(stan.data$covar_array)[1])

para <- extract(fit)
dim(para$beta)

plot_post_quantiles(fit, trueB, "res")

pdf("fig/lap&hs") 

par(mfrow=c(1,1))

util$plot_post_quantiles(fit, trueB, "lap50")
util$plot_post_quantiles(hs50, trueB, "hs50")



util$plot_residual_quantiles(fit, trueB, "lap50")
util$plot_residual_quantiles(hs50, trueB, "hs50")


util$plot_post_quantiles(lap1500, trueB, "lap1500")
util$plot_post_quantiles(hs1500, trueB, "hs1500")


util$plot_residual_quantiles(lap1500, trueB, "lap1500")
util$plot_residual_quantiles(hs1500, trueB, "hs1500")

util$plot_ran_int_quantiles(hs, beta_c, "residual hs with random intercepts")
util$plot_int_post_quantiles(hs, beta_c, "random intercepts posterior")

util$plot_ran_int_quantiles_adj(hs, beta_c, "adjusted random intercepts")

dev.off()

