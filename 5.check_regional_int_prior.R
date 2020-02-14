# how many countries in each region
table(countryRegionList$sdg_name)

library(rstan)
standata <- readRDS(file = "output/stan_data/nhs_nval.rds")     #stan data used for fit model
fit <- readRDS(file = "rdsoutput/regHS_nval_res.rds")       #stan fit 
traceplot(fit,pars = c("gamma_r"))

#traceplot
mcmc.array <- rstan::extract(fit)

gamma_r <-  round(apply(mcmc.array$gamma_r,2,median),digits = 3)
sd_gamma <- round(apply(mcmc.array$gamma_r,2,sd),digits = 3)
ci_gamma <- t(round(apply(mcmc.array$gamma_r,2,quantile,c(0.025,0.975)),digits = 3))
sdgname <- c("Southern Asia",
             "Sub-Saharan Africa",
             "Northern America, Australia and New Zealand, Central Asia and Europe",
             "Western Asia and Northern Africa ",
             "Latin America and the Caribbean",
             "Eastern Asia, South Eastern Asia and Oceania")
reg_summ <- as.data.frame(cbind(sdgname,gamma_r,sd_gamma,ci_gamma))
colnames(reg_summ) <- c("sdg region","estimates","sd","2.5%","97.%%")
#summary of regional intercept
reg_summ

x <- seq(-4, 4, length=100)
hx <- dnorm(x, mean = 2.5, sd = 2)

plot_region <- function(region){
  plot(density(mcmc.array$gamma_r[,region]),main = paste0(sdgname[region]),type="l",lty=3)
  lines(x, hx, type="l", lty=3, xlab="x value",
        ylab="Density", col = "red")
  legend("topright",legend = c("postior","prior"),col=c("black","red"),lty=3)}

# prior and posterior for each regional intercept
par(mfrow=c(1,2))
plot_region(1)
plot_region(2)
plot_region(3)
plot_region(4)
plot_region(5)
plot_region(6)

