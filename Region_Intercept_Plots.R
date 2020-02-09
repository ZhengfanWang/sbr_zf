#### Purpose of code: Contribution to Estimate -- try to understand when no data available ###
#### Author: Anu 
#### Date: Feb. 2, 2020
library(haven)
library(dplyr)
library(readxl)

rm(list=ls())

data <- read_xlsx("C:/Users/anmishra/Dropbox/UNICEF Stillbirth/Model/SBR 2020/data_for_plotting_20200121.xlsx")
regions <- read.csv("C:/Users/anmishra/Dropbox/UNICEF Stillbirth/Model/Regions/sbr_regions.csv")
fit <- readRDS(file = "C:/Users/anmishra/Dropbox/UNICEF Stillbirth/Model/SBR 2020/output/VEN_20200127/base_VEN_res_nval.rds")       #stan fit 

## regional intercept 
mcmc.array <- rstan::extract(fit)
gamma_r.med <-  round(apply(mcmc.array$gamma_r,2,median),digits = 3)
gamma_r.mean <-  round(apply(mcmc.array$gamma_r,2,mean),digits = 3)
table(regions$SDGRegionrev1,regions$sdg.rev1)
gamma_r <- data.frame("regions" = c("Southern Asia","Sub-Saharan Africa","Northern America, Australia and New Zealand, Central Asia and Europe",
                                    "Western Asia and Northern Africa","Latin America and the Caribbean","Eastern Asia, South Eastern Asia and Oceania (excl. Australia and New Zealand)"),
                      "gamma_r_med" = gamma_r.med, "gamma_r_mean" = gamma_r.mean)


#### Trend estimates 
mu_ct <- mcmc.array$mu_ct +mcmc.array$delta_ct                                  ### est mean
muhat <-c()
for(c in 1:195){
  for(t in 1:21){
    cache_mu_ct <- quantile(mu_ct[,c,t],c(0.025,0.5,0.975))
    cache_covar <- cache_mu_ct 
    cache_exp_mu_rt <- exp(cache_covar)
    muhat <- rbind(muhat,cache_exp_mu_rt)
  }
}
#muhat
colnames(muhat) <- c("low","muhat","up")
estyears <- seq(2000,2020)
year <- rep(estyears,times=195)

#isos
national_covar <- read_dta("C:/Users/anmishra/Dropbox/UNICEF Stillbirth/Model/SBR 2019/Code/sbr_zf-master/input/covar/national_covariates.dta",encoding='latin1')
countryRegionList <- national_covar[,c(1,2,8)] %>% distinct() %>% 
  dplyr::rename("iso"="iso3") %>% 
  mutate(country_idx=as.numeric(factor(iso)))

countryiso <- countryRegionList$iso 

countryiso <- countryRegionList$iso                     #### run "iso.R" to get countryRegionList
yearLength <- length(estyears)
iso <- rep(countryiso,each= yearLength)
muhat <- data.frame(muhat,year,iso)
rownames(muhat) <- NULL
muhat <- left_join(muhat,regions[,c("ISO3Code","SDGRegionrev1")], by = c("iso"="ISO3Code"))

nmr<-read.csv("C:/Users/anmishra/Dropbox/UN IGME data/2019 Round Estimation/Code/Aggregate results (median) 2019-08-15/Rates & Deaths_Country Summary.csv",stringsAsFactors = F)
nmr<-nmr[nmr$ISO3Code!="LIE",c("ISO3Code",paste0("NMR.",2000:2018))]
names(nmr)<-c("ISO3Code",paste0("NMR_",2000:2018))
nmr<-gather(nmr,tmp,nmr,NMR_2000:NMR_2018)%>%
  separate(tmp,into=c("ind","year"),sep="_")%>%
  mutate(year=as.numeric(year))%>%
  select(-ind) %>% arrange(ISO3Code, year)

muhat <- muhat[order(muhat$iso,muhat$year),]
muhat.nmr <- left_join(muhat,nmr,by=c("iso"="ISO3Code","year"))



#### underlying data
source_type <- c("admin","HMIS","subnat LR","survey")
source_type_bias <- c(0,0,0,median(mcmc.array$bias_dt))

data <- left_join(data,regions[,c("ISO3Code","SDGRegionrev1")], by = c("iso"="ISO3Code"))
data$SBR_adj <- ifelse(data$source=="survey",exp(log(data$SBR) - source_type_bias[4]),data$SBR)

data.inc <- data[data$inclusion=="incl",]
incl.countries <- unique(data.inc$iso)
excl.countries <- setdiff(countryiso,incl.countries)







######## Plots ########
### regional intercept with included data

pdf("C:/Users/anmishra/Dropbox/UNICEF Stillbirth/Analysis/fig/Regional Int with Data.pdf",width = 12,height=8)
for(i in 1:nrow(gamma_r)){
  data.region <- data.inc[data.inc$SDGRegionrev1==as.character(gamma_r$regions[i]),]
  plot(c(0,0),xlim=c(1999.5,2020.5),ylim=c(0,max(data.region$SBR)+3),xlab="Year",ylab="SBR (per 1000 live births)",main=gamma_r$regions[i])
  points(data.region$int.year[data.region$source=="admin"],data.region$SBR_adj[data.region$source=="admin"],pch=16,cex=1.2)
  points(data.region$int.year[data.region$source=="survey"],data.region$SBR_adj[data.region$source=="survey"],pch=17,cex=1.2)
  points(data.region$int.year[data.region$source=="HMIS"],data.region$SBR_adj[data.region$source=="HMIS"],pch=15,cex=1.2)
  points(data.region$int.year[data.region$source=="subnat.LR"],data.region$SBR_adj[data.region$source=="subnat.LR"],pch=3,cex=1.2)
  abline(h=exp(gamma_r$gamma_r_med[i]),col="blue",lwd=2)
  abline(h=exp(gamma_r$gamma_r_mean[i]),col="orange",lty=2,lwd=2)
  legend("topright",c("Admin","Survey","HMIS","Subnat.LR"),pch=c(16,17,15,3),title = "Included Data")
  legend("topleft",c(paste0("Med. Regional Intercept = ",round(exp(gamma_r$gamma_r_med[i]),3)),
                     paste0("Mean. Regional Intercept = ",round(exp(gamma_r$gamma_r_mean[i]),3))),
         col=c("blue","orange"),lty=c(1,2),lwd=2)
  
}
dev.off()




### regional intercept with trend estimtates - included only
library(maptools)
muhat.incl <- muhat[muhat$iso %in% incl.countries,]

pdf("C:/Users/anmishra/Dropbox/UNICEF Stillbirth/Analysis/fig/Regional Int with Trend.pdf",width = 12,height=8)
for(i in 1:nrow(gamma_r)){
  muhat.incl.region <- muhat.incl[muhat.incl$SDGRegionrev1==as.character(gamma_r$regions[i]),]
  plot(c(0,0),xlim=c(1999.5,2020.5),ylim=c(0,max(muhat.incl.region$muhat)+4),xlab="Year",ylab="SBR (per 1000 live births)",main=gamma_r$regions[i])
  
  country.region <- unique(muhat.incl.region$iso)
  for(c in 1:length(country.region)){
    lines(seq(2000,2020,1),muhat.incl.region$muhat[muhat.incl.region$iso==country.region[c]],pch=16,cex=1.2,lwd=2,col="red")
  }
  abline(h=exp(gamma_r$gamma_r_med[i]),col="blue",lwd=2)
  abline(h=exp(gamma_r$gamma_r_mean[i]),col="orange",lty=2,lwd=2)
  legend("topleft",c(paste0("Med. Regional Intercept = ",round(exp(gamma_r$gamma_r_med[i]),3)),
                     paste0("Mean. Regional Intercept = ",round(exp(gamma_r$gamma_r_mean[i]),3))),
         col=c("blue","orange"),lty=c(1,2),lwd=2)
  pointLabel(rep(2000,length(country.region)), y = muhat.incl.region$muhat[muhat.incl.region$year==2000], labels = muhat.incl.region$iso[muhat.incl.region$year==2000], cex =0.7,col="black")
}
dev.off()



### regional intercept with trend estimtates - both data 
muhat$incl.col <- ifelse(muhat$iso %in% incl.countries,"red","gray")

pdf("C:/Users/anmishra/Dropbox/UNICEF Stillbirth/Analysis/fig/Regional Int with Trend - all countries.pdf",width = 12,height=8)
for(i in 1:nrow(gamma_r)){
  muhat.region <- muhat[muhat$SDGRegionrev1==as.character(gamma_r$regions[i]),]
  plot(c(0,0),xlim=c(1999.5,2020.5),ylim=c(0,max(muhat.region$muhat)+4),xlab="Year",ylab="SBR (per 1000 live births)",main=gamma_r$regions[i])
  
  country.region <- unique(muhat.region$iso)
  for(c in 1:length(country.region)){
    lines(seq(2000,2020,1),muhat.region$muhat[muhat.region$iso==country.region[c]],pch=16,cex=1.2,lwd=2,col=unique(muhat.region$incl.col[muhat.region$iso==country.region[c]]))
  }
  abline(h=exp(gamma_r$gamma_r_med[i]),col="blue",lwd=2)
  abline(h=exp(gamma_r$gamma_r_mean[i]),col="orange",lty=2,lwd=2)
  legend("topleft",c(paste0("Med. Regional Intercept = ",round(exp(gamma_r$gamma_r_med[i]),3)),
                     paste0("Mean. Regional Intercept = ",round(exp(gamma_r$gamma_r_mean[i]),3))),
         col=c("blue","orange"),lty=c(1,2),lwd=2)
  legend("topright",c("Country with Data","Country with No Data"),col=c("red","gray"),lwd=2)
  pointLabel(rep(2000,length(country.region)), y = muhat.region$muhat[muhat.region$year==2000], labels = muhat.region$iso[muhat.region$year==2000], cex =0.7,col="black")
}
dev.off()




#### SBR to NMR Ratio #####
muhat.nmr$incl.col <- ifelse(muhat$iso %in% incl.countries,"red","gray")
muhat.nmr$sbr_nmr <- muhat.nmr$muhat/muhat.nmr$nmr

pdf("C:/Users/anmishra/Dropbox/UNICEF Stillbirth/Analysis/fig/SBR NMR Ratio_Regional Int with Trend.pdf",width = 12,height=8)
for(i in 1:nrow(gamma_r)){
  muhat.nmr.region <- muhat.nmr[muhat.nmr$SDGRegionrev1==as.character(gamma_r$regions[i]),]
  plot(c(0,0),xlim=c(1999.5,2019.5),ylim=c(0.25,max(muhat.nmr.region$sbr_nmr,na.rm = 2)+0.5),xlab="Year",ylab="SBR to NMR Ratio",main=gamma_r$regions[i])
  
  country.region <- unique(muhat.nmr.region$iso)
  for(c in 1:length(country.region)){
    lines(seq(2000,2020,1),muhat.nmr.region$sbr_nmr[muhat.nmr.region$iso==country.region[c]],pch=16,cex=1.2,lwd=2,col=unique(muhat.nmr.region$incl.col[muhat.nmr.region$iso==country.region[c]]))
  }
  legend("topright",c("Country with Data","Country with No Data"),col=c("red","gray"),lwd=2)
  pointLabel(rep(2000,length(country.region)), y = muhat.nmr.region$sbr_nmr[muhat.nmr.region$year==2000], labels = muhat.nmr.region$iso[muhat.nmr.region$year==2000], cex =0.7,col="black")
}
dev.off()

