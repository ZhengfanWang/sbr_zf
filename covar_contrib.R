rm(list=ls())

library(haven)
library(dplyr)
library(xlsx)

beta <- read.csv("C:/Users/anmishra/Dropbox/UNICEF Stillbirth/Model/SBR 2020/output/logLBW_20200219/hs2level_covariates.csv")
est <- read.csv("C:/Users/anmishra/Dropbox/UNICEF Stillbirth/Model/SBR 2020/output/logLBW_20200219/muhat.csv")
est.sub <- est %>% filter(year %in% c(2000,2020)) %>% select(iso,year,muhat) %>%
                   spread(year,muhat) %>%
  dplyr::rename("yr2020"="2020","yr2000"="2000") %>%
           mutate(overall.chg = yr2000- yr2020) %>%
           mutate(overall.lchg = log(yr2000) - log(yr2020))



#isos
national_covar <- read_dta("C:/Users/anmishra/Dropbox/UNICEF Stillbirth/Model/SBR 2020/Code/sbr_zf-master/input/covar/national_covariates.dta",encoding='latin1')
countryRegionList <- national_covar[,c(1,2,8)] %>% distinct() %>% 
  dplyr::rename("iso"="iso3") %>% 
  mutate(country_idx=as.numeric(factor(iso)))

covar <- read.csv("C:/Users/anmishra/Dropbox/UN IGME Stillbirths/Covariates/2019/sbr_igme_covariates_20191202.csv")
int_cov <- c("gni_sm","nmr","lbw_sm","anc4_sm","mean_edu_f_sm",
             "gini_sm","urban","gfr","sab","anc1_sm","abr_sm",
             "csec_sm","pab_sm","pfpr","gdp","mmr")
covarset <- covar %>% select(c("iso3","year",int_cov)) %>% 
  dplyr::rename("iso"="iso3") %>% 
  filter(year>=2000) %>% 
  mutate(nmr = log(nmr),
         lbw = log(lbw_sm),
         gni = log(gni_sm)) %>% 
  arrange(iso,year) 
source('C:/Users/anmishra/Dropbox/UNICEF Stillbirth/Model/SBR 2020/Code/sbr_zf-master/R/func_create_standata_for_model.R', echo=TRUE)
#covar_array <- create_covar_array(interest_cov = int_cov,estyears = seq(2000,2020), dataset = covarset)

covarset[,3:19] <- apply(covarset[,3:19],2,standardize)
covarset <- covarset %>% filter(year %in% c(2000,2020)) 


covarChg <- as.data.frame(matrix(NA,nrow=195,ncol=16))
names(covarChg) <- int_cov
covarChg.perc <- as.data.frame(matrix(NA,nrow=195,ncol=16))
names(covarChg.perc) <- int_cov

covarLogChg.perc <- as.data.frame(matrix(NA,nrow=195,ncol=16))
names(covarLogChg.perc) <- int_cov

for(i in 1:16){
  covar.name <- names(covarset)[names(covarset) %in% int_cov[i]]
  beta.cov <- beta$estimates[beta$covariates==covar.name]
  
  
  covar <- covarset[,c("iso","year",covar.name)]
  covar <-  covar %>% spread(year,covar.name) %>%
            dplyr::rename("cov.yr2020"="2020","cov.yr2000"="2000") %>%
    mutate(cov.chg.beta = (cov.yr2000- cov.yr2020)*beta.cov) %>%
    left_join(est.sub[,c("iso","overall.chg","overall.lchg")],by=c("iso"))
  
  covarChg[,i] <- covar$cov.chg.beta
  covarChg.perc[,i] <-  100*covar$cov.chg.beta/covar$overall.chg
  covarLogChg.perc[,i] <-  100*covar$cov.chg.beta/covar$overall.lchg
}

pdf("C:/Users/anmishra/Dropbox/UN IGME Stillbirths/output/covar contrib_20200228/absChg_hist.pdf")
for(i in 1:16){
  hist(covarChg[,i],main=paste("Absolute Change in",toupper(int_cov[i]),"times Beta"),xlab=toupper(int_cov[i]))
}
dev.off()



pdf("C:/Users/anmishra/Dropbox/UN IGME Stillbirths/output/covar contrib_20200228/percChg_SBR_hist.pdf")
for(i in 1:16){
  hist(covarChg.perc[,i],main=paste("Percent Change in SBR due to",toupper(int_cov[i])),xlab=toupper(int_cov[i]))
}
dev.off()


pdf("C:/Users/anmishra/Dropbox/UN IGME Stillbirths/output/covar contrib_20200228/percChg_logSBR_hist.pdf")
for(i in 1:16){
  hist(covarLogChg.perc[,i],main=paste("Percent Change in log(SBR) due to",toupper(int_cov[i])),xlab=toupper(int_cov[i]))
}
dev.off()


##### output tables #####
summ <- function(x){
  res <- c(min(x),quantile(x,0.25),quantile(x,0.5),quantile(x,0.75),mean(x),sd(x),max(x))
  names(res) <- c("min","q25","q50","q75","mean","sd","max")
  return(res)
}
chg <- as.data.frame(t(apply(covarChg,2,summ)))

write.xlsx(chg,"C:/Users/anmishra/Dropbox/UN IGME Stillbirths/output/covar contrib_20200228/summary.xlsx",row.names = TRUE)


percChg <- as.data.frame(t(apply(covarChg.perc,2,summ)))
write.csv(chg,"C:/Users/anmishra/Desktop/xx.csv",row.names = TRUE)

percLogChg <- as.data.frame(t(apply(covarLogChg.perc,2,summ)))
write.csv(percLogChg,"C:/Users/anmishra/Desktop/xx.csv",row.names = TRUE)
