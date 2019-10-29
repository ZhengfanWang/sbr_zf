 
sbr2018 <- readRDS("output/data_for_model.rds")
dim(sbr2018)


########################################################
table(sbr2018$definition_rv)
dim(sbr2018)
### input covariates
int_cov <- c("gni","nmr","lbw","anc4","mean_edu","exbf","gfr","gini","hdi","imr","u5mr","literacy_fem","ors","anc1","sab",
                  "underweight","stunting","u5pop","urban","dtp3","mcv","bcg","pab","hib3","rota_last","pcv3","sanitation","water")

int_cov <- c("gni","nmr","lbw","anc4","mean_edu")

covarset <- read_dta("input/covar/mcee_covariates_20190625.dta",encoding='latin1')%>% 
  select(c("iso3","year",int_cov)) %>% 
  dplyr::rename("iso"="iso3") %>% 
  merge(countryRegionList,by="iso") %>% 
  filter(year>=2000) %>% 
  mutate(gni = log(gni),
         nmr = log(nmr)) %>% 
  arrange(iso,year) 

### definition type
definition_fac <- c("ge28wks","ge1000g","ge22wks","ge500g")

###################bias and variance
definition_bias <- c(0,-0.07,0.38,0.27)
definition_var <-  c(0,0.09,0.15,0.12)

#sbr2018$definition_rv <-  fct_collapse(sbr2018$definition_rv,
#                             ge1000g = c("ge1000g","ge1000gANDge28wks","ge1000gORge28wks"),
#                             ge500g =  c("ge500gORge22wks","ge500g"),
#                             ge22wks = c("ge22wks"),
#                             ge20wks = c("ge20wks","ge400gORge20wks","ge500gORge20wks"),
#                             ge24wks = c("ge24wks"),
#                             ge26wks = c("ge1000gORge26wks"),
#                             ge28wks = c("ge28wks","s40wksANDge28wks"))

sbr2018_cleaned <- sbr2018 %>% filter(definition_rv %in% definition_fac) %>% 
                               filter(!is.na(SE.logsbr))
names(sbr2018)

model_data_list <- create_list_for_country(sbr2018_cleaned) 
pdf_name <- paste0("fig/exploratory_plot/model_data_clean.pdf")
pdf(pdf_name,width=12)
model_data_list %>% lapply(exploratory_plot)
dev.off()



sbr2018_cleaned$source <- droplevels(as.factor(sbr2018_cleaned$source))
sbr2018_cleaned$source2 <- as.numeric(sbr2018_cleaned$source)


#definition type





sbr2018_cleaned$definition_rv <- factor(sbr2018_cleaned$definition_rv, levels = definition_fac)
sbr2018_cleaned$definition_rv <- droplevels(sbr2018_cleaned$definition_rv)

dim(sbr2018_cleaned)
table(sbr2018_cleaned$definition_rv)


N = dim(sbr2018_cleaned)[1]

getd.i <- as.numeric(sbr2018_cleaned$definition_rv)


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

## order=1 means Random walk 1, degree = 3 cubic spline
splines.data <- getSplinesData(yearLength,I=1,order=1, degree = 2)

gett.i<- sbr2018_cleaned$year-estyears[1]+1

covar_array <- create_covar_array(interest_cov = int_cov)
#X1 <- covarMatrix(int_cov[1])
#X2 <- covarMatrix(int_cov[2])
#X3 <- covarMatrix(int_cov[3])
#X4 <- covarMatrix(int_cov[4])
#X5 <- covarMatrix(int_cov[5])

### source type
datatype1.i <- ifelse(sbr2018_cleaned$source2==1,1,0)
datatype2.i <- ifelse(sbr2018_cleaned$source2==2,1,0)
datatype3.i <- ifelse(sbr2018_cleaned$source2==3,1,0)
datatype4.i <- ifelse(sbr2018_cleaned$source2==4,1,0)
datatype5.i <- ifelse(sbr2018_cleaned$source2==5,1,0)
getj.i <- sbr2018_cleaned$source2

#stan.data<- list(Y = log(sbr2018_cleaned$SBR), var_i = sbr2018_cleaned$SE^2, 
#                          X1 = X1, X2=X2, X3=X3, X4=X4, X5=X5, 
#                       Z1= standardize(X1),Z2 =standardize(X2),Z3 =standardize(X3),Z4 =standardize(X4),Z5 =standardize(X5),
#                  getj_i = getj.i, getd_i = getd.i, gett_i = round(gett.i), getc_i = getc.i,getr_c = getr.c,
#                  eta_d = definition_bias, phi_d = definition_var,
#                  datatype2_i = datatype2.i, datatype3_i = datatype3.i,datatype4_i=datatype4.i,
#                  deftype1_i=deftype1.i,deftype2_i=deftype2.i,deftype3_i=deftype3.i,deftype4_i=deftype4.i,
#                  N = N, numcountry = 195, numregion = max(getr.c), estyears = estyears,
#                  yearLength = yearLength, d=5 , 
#                  B_tk=splines.data$B.tk, K=splines.data$K, D=splines.data$D, Z_th=splines.data$Z.tk,
#                  BG_td = splines.data$BG.td,H=splines.data$H)

stan.data<- list(Y = log(sbr2018_cleaned$SBR), var_i = sbr2018_cleaned$SE.logsbr^2, covar_array = covar_array,
                 getj_i = getj.i, getd_i = getd.i, gett_i = round(gett.i), getc_i = getc.i,getr_c = getr.c,
                 eta_d = definition_bias, phi_d = definition_var,
                 datatype1_i = datatype1.i,datatype2_i = datatype2.i, datatype3_i = datatype3.i,datatype4_i=datatype4.i,datatype5_i=datatype5.i,
                 deftype1_i=deftype1.i,deftype2_i=deftype2.i,deftype3_i=deftype3.i,deftype4_i=deftype4.i,
                 N = N, numcountry = 195, numregion = max(getr.c), estyears = estyears, numcov = length(int_cov),
                 yearLength = yearLength, numdef = length(definition_fac),
                 B_tk=splines.data$B.tk, K=splines.data$K, D=splines.data$D, Z_th=splines.data$Z.tk,
                 BG_td = splines.data$BG.td,H=splines.data$H)
saveRDS(stan.data,file = "output/stan.quad.I1.rds")
do.validation = T
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
stan.data$getitrain_k


saveRDS(stan.data,file = "output/stan.loo.rds")




