#### Purpose of code: Plots of 1) Regional intercept prior/poseterios 
######                         2) Prior/posteriors on beta coefficients
######                         3) Country intercepts distribution around posterior median regional intercept
## Author: Anu Mishra
## Date: 2/18/2020

rm(list=ls())

library(haven)

sdgregion <- read.csv("C:/Users/anmishra/Dropbox/UN IGME Stillbirths/Other/sbr_regions.csv")
sdg_region <- sdgregion %>% select(ISO3Code,sdg.rev1,SDGRegionrev1) %>% 
  dplyr::rename("iso"="ISO3Code","sdg" = "sdg.rev1","sdg_name"="SDGRegionrev1")

data <- read.csv("C:/Users/anmishra/Dropbox/UN IGME Stillbirths/output/20200216/data_for_model_20200216.csv")

#fit <- readRDS("C:/Users/anmishra/Dropbox/UN IGME Stillbirths/output/20200216/regHS_longIter_nval_res20200214.rds") #one level
fit <- readRDS("C:/Users/anmishra/Dropbox/UN IGME Stillbirths/output/20200216/regHS2level_longIter_nval_res20200214.rds") #two-level
mcmc.array <- rstan::extract(fit)


#### Posteriors vs Priors #####
## regional intercepts ##

## for single level model ##
pdf("C:/Users/anmishra/Dropbox/UN IGME Stillbirths/output/20200216/fig/20200216_2levelhs_regionInt_density.pdf",width=12,height=8)
gamma_r <- mcmc.array$gamma_r
gamma_r <- as.data.frame(gamma_r)
names(gamma_r) <- c("region1","region2","region3","region4","region5","region6")
for(i in 1:6){
  gamma_post <- gamma_r[,i]
  d <- density(gamma_post)
  region.name <- unique(sdg_region$sdg_name[sdg_region$sdg==i])
  
  plot(d,xlab="Regional Intercept",ylab="Density",xlim=c(1,3), ylim = c(0,7.5), lwd=2, main = region.name)
  lines(x = seq(-10,10,0.001),y = dnorm(x = seq(-10,10,0.001), mean = 2.5,sd = 2),type = "l",lwd=2,lty=2)
  legend("topleft",c("Prior","Posterior"),lty=c(2,1),lwd=2)
}
dev.off()


### For 2 level model ###
pdf("C:/Users/anmishra/Dropbox/UN IGME Stillbirths/output/20200216/fig/20200216_2levelhs_regionInt_density.pdf",width=12,height=8)
gamma_r <- as.data.frame(mcmc.array$gamma_r)
gamma_w <- as.numeric(mcmc.array$gamma_w)
gamma_w_d <- density(gamma_w)
plot(gamma_w_d,xlab="Regional Intercept Hyperparameter: Mean" ,ylab="Density",xlim=c(1,4), ylim = c(0,7), lwd=2, main = "gamma_w")
lines(x = seq(-10,10,0.001),y = dnorm(x = seq(-10,10,0.001), mean = 2.5,sd = 2),type = "l",lwd=2,lty=2)
legend("topleft",c("Prior","Posterior"),lty=c(2,1),lwd=2)

sigma_r <- as.numeric(mcmc.array$sigma_r)
sigma_r_d <- density(sigma_r)
plot(sigma_r_d,xlab="Regional Intercept Hyperparameter: Standard Deviation" ,ylab="Density",xlim=c(-0.1,1.5), ylim = c(0,7), lwd=2, main = "sigma_r")
lines(x = seq(-10,10,0.001),y = dnorm(x = seq(-10,10,0.001), mean = 0,sd = 1),type = "l",lwd=2,lty=2)
legend("topright",c("Prior","Posterior"),lty=c(2,1),lwd=2)
dev.off()



## coefficients ##
pdf("C:/Users/anmishra/Dropbox/UN IGME Stillbirths/output/20200216/fig/20200216_2levelhs_beta_density.pdf",width=12,height=8)
int_cov <- c("gni_sm","nmr","lbw_sm","anc4_sm","mean_edu_f_sm",
             "gini_sm","urban","gfr","sab","anc1_sm","abr_sm",
             "csec_sm","pab_sm","pfpr","gdp","mmr")
beta <- mcmc.array$beta
for(i in 1:length(int_cov)){
  beta_post <- beta[,i]
  d <- density(beta_post)
  var.name <- int_cov[i]
  
  plot(d,xlab="Beta",ylab="Density",xlim=c(-0.25,0.75), ylim = c(0,47), lwd=2, main = var.name)
  lines(x = seq(-10,10,0.001),y = dnorm(x = seq(-10,10,0.001), mean = 0,sd = 1),type = "l",lwd=2,lty=2)
  legend("topleft",c("Prior","Posterior"),lty=c(2,1),lwd=2)
}
dev.off()

###### Country Intercept w/ Regional Intercept ######
gamma_c <-  mcmc.array$gamma_c
gamma_c.med <- apply(gamma_c,2,median)
gamma_c.upp <- apply(gamma_c,2,quantile,0.95)
gamma_c.low <- apply(gamma_c,2,quantile,0.05)
national_covar <- read_dta("C:/Users/anmishra/Dropbox/UNICEF Stillbirth/Model/SBR 2019/Code/sbr_zf-master/input/covar/national_covariates.dta",encoding='latin1')
countryRegionList <- national_covar[,c(1,2,8)] %>% distinct() %>% 
  dplyr::rename("iso"="iso3") %>% 
  mutate(country_idx=as.numeric(factor(iso)))
countryiso <- countryRegionList$iso 

gamma_c <- data.frame("iso"=countryiso,"med" = gamma_c.med, "low" = gamma_c.low , "upp"  = gamma_c.upp)
gamma_c <- left_join(gamma_c,sdg_region,by="iso")

dataIso <- unique(data$iso)
gamma_c$hasData <- ifelse(gamma_c$iso %in% dataIso,"blue","red")

pdf("C:/Users/anmishra/Dropbox/UN IGME Stillbirths/output/20200216/fig/20200216_2levelhs_countryIntercept.pdf",width=14,height=10)
for(i in 1:6){
  gamma_c_reg <- gamma_c[gamma_c$sdg==i,]
  gamma_c_reg <- gamma_c_reg[order(gamma_c_reg$hasData,decreasing = T),]
  
  gamma_c_reg$iso <- factor(gamma_c_reg$iso, levels = gamma_c_reg$iso)
  x.lim = c(1,3.5)
  region.name <- unique(sdg_region$sdg_name[sdg_region$sdg==i])
  
  plot(0,0,xlim = x.lim, ylim = c(0,6+length(gamma_c_reg$iso)*2),yaxt="n",xlab="Median Country Intercept (90% Uncertainty Bound)",ylab="Country",main=region.name)
  abline(v=median(gamma_r[,i]),lty=2,lwd=2)
  points(gamma_c_reg$med,seq(2,length(gamma_c_reg$iso)*2,2),pch=16,col=gamma_c_reg$hasData)
  arrows(gamma_c_reg$low, seq(2,length(gamma_c_reg$iso)*2,2), gamma_c_reg$upp,seq(2,length(gamma_c_reg$iso)*2,2),
         length=0.05, angle=90, code=3,col=gamma_c_reg$hasData)
  
  axis(side = 2,at = seq(2,length(gamma_c_reg$iso)*2,2),labels = gamma_c_reg$iso,las=2,cex.axis=0.8)
  legend("topright","Median Regional Intercept",lty=2,lwd=2,bty="n")
  legend("topleft",c("Country w/ Data","Country w/o Data"),pch=16,lty=1,col=c("blue","red"))
}
dev.off()


