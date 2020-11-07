
rm(list=ls())

library(rstan)
library(dplyr)
library(tidyr)
library(haven)
library(ggnewscale)
#fit <-readRDS(file = "rdsoutput/new/base_cesc.rds")
#fit2 <-readRDS(file = "rdsoutput/new/tau0.001.rds")
fit <- readRDS(file = "rdsoutput/new/hs_nval_res20200907.rds")
#fit4 <- readRDS(file = "C:/Users/anmishra/Dropbox/UNICEF Stillbirth/Model/SBR 2020/output/20200211/regHS2level_nval_res20200210.rds")
old <-readRDS(file = "rdsoutput/new/Base_nval_res202000311.rds")

#isos
#national_covar <- read_dta("C:/Users/anmishra/Dropbox/UNICEF Stillbirth/Model/SBR 2019/Code/sbr_zf-master/input/covar/national_covariates.dta",encoding='latin1')
countryRegionList <- national_covar[,c(1,2,8)] %>% distinct() %>% 
  dplyr::rename("iso"="iso3") %>% 
  mutate(country_idx=as.numeric(factor(iso)))

countryiso <- countryRegionList$iso 

################################################




Pspline1_country_list <- get_chain_result(fit,smooth = T)
Pspline2_country_list <- get_chain_result(fit,smooth = F)
#remove(fit) ## to free up memory
#Pspline3_country_list <- get_chain_result(fit3,smooth = T)
#remove(fit3) ## to free up memory
#Pspline4_country_list <- get_chain_result(fit4,smooth = T)
#Pspline5_country_list <- get_chain_result(old,smooth = T)
#remove(old) ## to free up memory
####################################################################

standata <- readRDS(file ="output/stan_data/base_nval_20200907.rds")

definition_fac <- c("ga28wks","ga22wks","ga24wks","bw1000g","bw500g")
source_fac <- c("admin","HMIS","population study","survey")


sbr2018 <- data.frame(logSBR = standata$unadj_Y )
sbr2018$getj_i <- standata$getj_i
sbr2018$getd_i <- standata$getd_i
sbr2018$year <- standata$gett_i + 1999
sbr2018$country_idx <- standata$getc_i
sbr2018$source_name <- source_fac[sbr2018$getj_i]
sbr2018$definition_name <- definition_fac[sbr2018$getd_i]


df <- rstan::extract(fit)
bias_dt_i <- apply(df$bias_dt_i,2,median)
sd_i <- apply(df$sigma_i,2,median)
var_i <- sd_i^2

sbr2018$var <- var_i
sbr2018$logadjsbr <- standata$Y  - bias_dt_i
train <- rep(0,standata$N)
train[standata$getitrain_k] <- 1
sbr2018$train <- train 
sbr2018 <- merge(sbr2018,countryRegionList,by=c("country_idx"))
#sbr2018$source_name <- as.factor(sbr2018$source_name)  
#levels(sbr2018$source_name) <- 
estyears <- standata$estyears
year <- rep(estyears,times=standata$numcountry)
remove(fit)

###################  add this because we want to see the country name if no data point for some country, AND make legend consistent.
year.f <- rep(2020,5)
logSBR.f <- rep(-50,5)
logsbradj.f <- rep(-50,5)

fake.legend <- data.frame(year=year.f,logSBR=logSBR.f,logadjsbr=logsbradj.f,source_name=c(source_fac,"admin"),definition_name=definition_fac) %>% 
  mutate(country = NA,iso = NA, country_idx = NA, var = rep(0.001,5), low = NA, muhat = NA, up = NA,train = NA) %>% 
  select(country,iso,source_name,definition_name,country_idx,logSBR,logadjsbr, year, var , train)
####################################################################

point.list <- list()
point.list_cache <- list()
for(c in 1:standata$numcountry){
  point.list_cache[[c]] <- filter(sbr2018,country_idx==c) %>% 
    select(country, iso, source_name,definition_name,
           country_idx,logSBR,logadjsbr,year,var,train) 
   # right_join(Pspline1_country_list[[c]],by = c("year","country","iso")) %>% 
   point.list[[c]] <- rbind(fake.legend,point.list_cache[[c]]  )
  }
point.list[[5]]$source_name
point.list[[5]]$definition_name
point.list[[16]]
point.list[[12]]
###################################################################




#example

Pspline1_country_list[[1]]
normal_keep_ex <- compare.plot.list(Pspline1_country_list,Pspline2_country_list,
                                    #Pspline3_country_list,Pspline5_country_list,
                                    name=c("BHTSRM","covariate fit"
                                           #,"3.HS model","4.base_5cov"
                                           ))
#########################################################
#NOTE: changed in database
#normal_keep_ex[[12]][c(20,45),3] = "survey"
#normal_keep_ex[[12]][c(15,40),3] = "HMIS"
###################################################

save(normal_keep_ex, file="output/Model_Opt.RData")
 load("output/Model_Opt.Rdata")

##########################################################################################


normal_keep_ex[[28]]
normal_keep_ex[[46]]$definition_name
pdf_name <- paste0("fig/compar_plot_new.pdf")
pdf(pdf_name, width = 12, height = 7)
normal_keep_ex %>% lapply(compare_plot)
dev.off()






