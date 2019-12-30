
hs <- T
do.validation <- F
save.to <- "output/stan_data/hs_nval.rds"
#------------------------------ input -------------------------------------------#
endyear <- 2020
estyears <- seq(2000,endyear)
#--------------------------------------------------------------------------------#

#-------------------------------#
#   covariates set              #
#-------------------------------#
covarset.raw <- read.csv("input/covar/sbr_igme_covariates_20191202.csv")

int_cov <- c("gni_sm","nmr","lbw_sm","anc4_sm","mean_edu_f_sm")
if(hs == TRUE){
  int_cov <- c("gni_sm","nmr","lbw_sm","anc4_sm","mean_edu_f_sm",
               "gini_sm","urban","gfr","sab","anc1_sm","abr_sm",
               "csec_sm","pab_sm","pfpr","gdp","mmr")
}

covarset <- covarset.raw %>% select(c("iso3","year",int_cov)) %>% 
  dplyr::rename("iso"="iso3") %>% 
  merge(countryRegionList,by="iso") %>% 
  filter(year>=2000) %>% 
  mutate(gni = log(gni_sm),
         nmr = log(nmr)) %>% 
  arrange(iso,year) 
covar_array <- create_covar_array(interest_cov = int_cov,estyears = estyears, dataset = covarset)

#-------------------------------#
#   definition adjustment       #
#-------------------------------#
#definition adjustment output
def_adj_output <- readRDS("output/def_adj_res.rds")

#data for model result
sbr2018 <- readRDS("output/data_for_model.rds") 
definition_fac <- c("ge28wks",paste0(unique(def_adj_output$definition_rv[!is.na(def_adj_output$def_bias)])))
sbr2018$definition_rv <- factor(sbr2018$definition_rv, levels = definition_fac)

sbr2018 <- right_join(def_adj_output,sbr2018,by = c("definition_rv","lmic")) 

sbr2018$def_bias <- ifelse(is.na(sbr2018$def_bias),0,sbr2018$def_bias)
sbr2018$def_sd <- ifelse(is.na(sbr2018$def_sd),0,sbr2018$def_sd)

sbr2018$definition_rv <- factor(sbr2018$definition_rv, levels = definition_fac)
getd.i <- as.numeric(sbr2018$definition_rv)

#-------------------------------#
#   source type                 #
#-------------------------------#
sbr2018$source <- droplevels(sbr2018$source)
sbr2018$source2 <- as.numeric(sbr2018$source)
datatype1.i <- ifelse(sbr2018$source2==1,1,0)   # admin
datatype2.i <- ifelse(sbr2018$source2==2,1,0)   # HMIS
datatype3.i <- ifelse(sbr2018$source2==3,1,0)   # subnat.lr
datatype4.i <- ifelse(sbr2018$source2==4,1,0)   # survey
getj.i <- sbr2018$source2

#-------------------------------#
#   Other                       #
#-------------------------------# 
N = dim(sbr2018)[1]
Y = log(sbr2018$SBR) - sbr2018$def_bias
var_i = sbr2018$SE.logsbr^2 + sbr2018$def_sd^2 
getc.i <- sbr2018$country_idx
getr.c <- countryRegionList$sdg
gett.i<- sbr2018$year-estyears[1]+1
yearLength <- length(estyears)

###Basis matrix
#library(splines)
## order=1 means Random walk 1, degree = 3 cubic spline
splines.data <- getSplinesData(yearLength,I=1,order=1, degree = 2)

stan.data<- list(Y = Y, var_i = var_i, unadj_Y = log(sbr2018$SBR),
                 covar_array = covar_array, definition_rv = sbr2018$definition_rv,
                 getj_i = getj.i, getd_i = getd.i, gett_i = gett.i, getc_i = getc.i,getr_c = getr.c,
                 datatype1_i = datatype1.i, datatype2_i = datatype2.i, datatype3_i = datatype3.i,datatype4_i=datatype4.i,
                 N = N, numcountry = max(getc.i), numregion = max(getr.c), estyears = estyears, yearLength = yearLength,
                 numdef = max(getd.i), numcov = length(int_cov), numsource = max(getj.i),
                 B_tk=splines.data$B.tk, K=splines.data$K, D=splines.data$D, Z_th=splines.data$Z.tk,
                 BG_td = splines.data$BG.td,H=splines.data$H)

if (!do.validation){
  # all observations are in the training set
  stan.data$getitrain_k <- seq(1, N)
} else {
  # this is one particular type of validation, 
  # leaving out the most recent data point in all countries with at least 2 observations
  indiceslastobs <- list()
  for (c in 1: 195){
    if (sum(getc.i==c)>2){
      indiceslastobs[[c]] <- which(gett.i==max(gett.i[getc.i==c]) & getc.i==c)   
    }
  }
  stan.data$getitrain_k <- seq(1,N)[!is.element(seq(1,N), unlist(indiceslastobs))]
}
stan.data$ntrain <- length(stan.data$getitrain_k)
saveRDS(stan.data,file = save.to)
