library(tidyverse)
library(haven)
covarMatrix <-function (covar,
                        estyears=seq(2000,2018),
                        dataset=covarset){
  yearLength <- length(estyears)
  countryRegionList <- dataset[,c(1,9,10)] %>% distinct()
  numcoun<- length(countryRegionList$iso)
  cMatrix<- matrix(ncol=yearLength,nrow=numcoun)
  for (i in 1:numcoun){
    for ( j in 1: yearLength){
      cMatrix[i,j] <- as.numeric(dataset[dataset$iso == countryRegionList$iso[i] & dataset$year== estyears[j],covar])
    }
  }
  return (cMatrix)
}

standardize <- function(x){
  return((x-mean(x))/sd(x))
}



###########################################function ###################################


sbr2018 <- readRDS("output/dataformodel.rds")

unique(sbr2018[sbr2018$source=="subnat.admin",]$country)

sbr2018 <- sbr2018 %>% filter(source != "subnat.admin")

sbr2018$source <- droplevels(as.factor(sbr2018$source))
sbr2018$source2 <- as.numeric(sbr2018$source)


#definition type

table(sbr2018$definition)

### definition type

definition_fac <- c("ge28wks","ge1000g","ge22wks","ge500g")

sbr2018_cleaned <- sbr2018 %>% filter(definition %in% definition_fac) %>% 
  filter(!is.na(SE))

dim(sbr2018_cleaned)

sbr2018_cleaned$definition <- droplevels(sbr2018_cleaned$definition)
sbr2018_cleaned$definition <- factor(sbr2018_cleaned$definition, levels = definition_fac)

dim(sbr2018_cleaned)
length(sbr2018_cleaned$definition)


###################bias and variance
definition_bias <- c(0,-0.07,0.38,0.27)
definition_var <-  c(0,0.09,0.15,0.12)

### input region grouping list
national_covar <- read_dta("input/covar/national_covariates.dta",encoding='latin1')
countryRegionList <- national_covar[,c(1,2,8)] %>% distinct() %>% 
  dplyr::rename("iso"="iso3") %>% 
  mutate(country_idx=as.numeric(factor(iso))) 

countryRegionList

sbr2018_cleaned <- merge(sbr2018_cleaned,countryRegionList,by="iso")
N = dim(sbr2018_cleaned)[1]

getd.i <- as.numeric(sbr2018_cleaned$definition)

#nu <- rep(NA,length=N)
#for(i in 1:N){
#nu[i] <- getdf(sbr2018_cleaned$rSN_UN[i],0.33,global_expected = 0.96)
#}

deftype1.i <- ifelse(getd.i==1,1,0)
deftype2.i <- ifelse(getd.i==2,1,0)
deftype3.i <- ifelse(getd.i==3,1,0)
deftype4.i <- ifelse(getd.i==4,1,0)

getc.i <- sbr2018_cleaned$country_idx
getr.c <- countryRegionList$shmdg2

estyears<-seq(2000,2018)
yearLength <- length(estyears)

###Basis matrix
library(splines)

source("R/GetSplines.R")
## order=1 means Random walk 1, degree = 3 cubic spline
splines.data <- getSplinesData(yearLength,I=1,order=1, degree = 3)


gett.i<- sbr2018_cleaned$int.year-estyears[1]+1


### input covariates
covarset <- read_dta("input/covar/mcee_covariates_20190625.dta",encoding='latin1')%>% 
  select("iso3","year","gni","nmr","lbw","anc4","mean_edu") %>% 
  dplyr::rename("iso"="iso3") %>% 
  merge(countryRegionList,by="iso")



X1 <- log(covarMatrix("gni"))
X2 <- log(covarMatrix("nmr"))
X3 <- covarMatrix("lbw")
X4 <- covarMatrix("anc4")
X5 <- covarMatrix("mean_edu")



### source type

datatype2.i <- ifelse(sbr2018_cleaned$source2==2,1,0)
datatype3.i <- ifelse(sbr2018_cleaned$source2==3,1,0)
datatype4.i <- ifelse(sbr2018_cleaned$source2==4,1,0)

getj.i <- sbr2018_cleaned$source2

stan.data<- list(Y = log(sbr2018_cleaned$SBR), var_i = sbr2018_cleaned$SE^2, 
                          X1 = X1, X2=X2, X3=X3, X4=X4, X5=X5, 
                       Z1= standardize(X1),Z2 =standardize(X2),Z3 =standardize(X3),Z4 =standardize(X4),Z5 =standardize(X5),
                  getj_i = getj.i, getd_i = getd.i, gett_i = round(gett.i), getc_i = getc.i,getr_c = getr.c,
                  eta_d = definition_bias, phi_d = definition_var,
                  datatype2_i = datatype2.i, datatype3_i = datatype3.i,datatype4_i=datatype4.i,
                  deftype1_i=deftype1.i,deftype2_i=deftype2.i,deftype3_i=deftype3.i,deftype4_i=deftype4.i,
                  N = N, numcountry = 195, numregion = max(getr.c), estyears = estyears,
                  yearLength = yearLength, d=5 , 
                  B_tk=splines.data$B.tk, K=splines.data$K, D=splines.data$D, Z_th=splines.data$Z.tk,
                  BG_td = splines.data$BG.td,H=splines.data$H)



length(stan.data$Y)
length(stan.data$nu)
saveRDS(stan.data,file = "output/standata.cubic.I1.rds")




