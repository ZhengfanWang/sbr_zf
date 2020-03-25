####All user-defined functions are in R/comp_plot.R

# load one or two stanmodel output. 
# 1. load one if want to plot covariate-base vs model fit
# 2. load two if want to compare two model results

fit <- readRDS(file = "rdsoutput/reg_hs_nval.rds")
#fit2 <- readRDS(file = "rdsoutput/reg_hs_nval.rds")

################################################

#get estimates: smooth = FALSE means covariates-base; TRUE means model fit.
# 1. the same fit but different smooth option if want to plot covariate-base vs model fit
# 2. different fits and use smooth=TRUE to compare two models,eg.validation results.
country_list1 <- get_chain_result(fit,smooth = TRUE)
country_list2 <- get_chain_result(fit,smooth = FALSE)
####################################################################

# read the standata which fit the model
standata <- readRDS(file = "output/stan_data/hs_nval.rds")
standata <- standata
definition_fac <- c("ga28wks","ga22wks","ga24wks","bw1000g","bw500g")
source_fac <- c("admin","HMIS","subnat LR","survey")
sbr2018 <- data.frame(logSBR = standata$unadj_Y )
sbr2018$getj_i <- standata$getj_i
sbr2018$getd_i <- standata$getd_i
sbr2018$year <- standata$gett_i + 1999
sbr2018$country_idx <- standata$getc_i
sbr2018$source_name <- source_fac[sbr2018$getj_i]
sbr2018$definition_name <- definition_fac[sbr2018$getd_i]
estyears <- seq(2000,2020)

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
 
###  add this because we want to see the country name if 
###  no data point for some country, AND make legend consistent.
year.f <- c(1,2,3,4,5)
logSBR.f <- rep(0,5)
logsbradj.f <- rep(1,5)

fake.legend <- data.frame(year=year.f,logSBR=logSBR.f,logadjsbr=logsbradj.f,source_name=c(source_fac,"admin"),definition_name=definition_fac) %>% 
  mutate(country = NA,iso = NA, country_idx = NA, var = rep(0.001,5), low = NA, muhat = NA, up = NA,train = NA) %>% 
  select(country,iso,source_name,definition_name,country_idx,logSBR,logadjsbr, year, var , train)
####################################################################

point.list <- list()
for(c in 1:standata$numcountry){
  point.list[[c]] <- filter(sbr2018,country_idx==c) %>% 
    select(country, iso, source_name,definition_name,
           country_idx,logSBR,logadjsbr,year,var,train) %>% 
   # right_join(Pspline1_country_list[[c]],by = c("year","country","iso")) %>% 
    rbind(fake.legend)
  }


# Create plot list and add names for two sets of estimates
comp.list <- compare.plot.list(country_list1,country_list2, c("estimates","w/o smooth"))



pdf_name <- paste0("fig/HS_fit&cov.pdf")
pdf(pdf_name, width = 8, height = 5)
comp.list %>% lapply(compare_plot)
dev.off()


