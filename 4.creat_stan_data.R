 
sbr2018 <- readRDS("output/data_for_model.rds")
sbr2018$definition_rv <- droplevels(sbr2018$definition_rv)
table(sbr2018$definition_rv)
dim(sbr2018)
names(sbr2018)

########################################################

hs <- FALSE
do.validation <- F
### input covariates

int_cov <- c("gni","nmr","lbw","anc4","mean_edu")
if(hs == TRUE){
int_cov <- c(int_cov,"exbf","gfr","gini","hdi","imr","u5mr","literacy_fem","ors","anc1","sab",
                 "underweight","stunting","u5pop","urban","dtp3","mcv","bcg","pab","hib3","rota_last","pcv3","sanitation","water")
}

covarset.raw <- read_dta("input/covar/mcee_covariates_20190625.dta",encoding='latin1')
#covarset.nmr <- read.csv("input/covar/mcee_covariates_20191113.csv")
 
covarset <- covarset.raw %>% select(c("iso3","year",int_cov)) %>% 
                             dplyr::rename("iso"="iso3") %>% 
                             merge(countryRegionList,by="iso") %>% 
                             filter(year>=2000) %>% 
                             mutate(gni = log(gni),
                             nmr = log(nmr)) %>% 
                             arrange(iso,year) 

#------------------------------------------#
# need updates input from def adj          #
#------------------------------------------#
definition_rv <- c("ge22wks","ge22wks","ge24wks","ge24wks","ge1000g","ge20wks","ge500g","ge1000gANDge28wks")
lmic <- c(0,1,0,1,0,1,0,1)
def_bias <- c(0.3890,0.2157,NA,0.2660,-0.07,NA,0.27,NA)
def_sd <- c(0.17240,0.08412,NA,0.11328,0.3,NA,0.3464,NA)

def_adj_res <- data.frame(definition_rv=definition_rv,
                          lmic=lmic,
                          def_bias=def_bias,
                          def_sd=def_sd)


sbr2018 <- right_join(def_adj_res,sbr2018,by = c("definition_rv","lmic"))

definition_fac <- c("ge28wks",paste0(unique(def_adj_res$definition_rv[!is.na(def_adj_res$def_bias)])))
sbr2018_cleaned <- sbr2018 %>% filter(definition_rv %in% definition_fac) %>% 
                               mutate(def_bias = ifelse(is.na(def_bias),0,def_bias)) %>% 
                               mutate(def_sd = ifelse(is.na(def_sd),0,def_sd))



sbr2018_cleaned$source <- droplevels(as.factor(sbr2018_cleaned$source))
sbr2018_cleaned$source2 <- as.numeric(sbr2018_cleaned$source)

#definition type
sbr2018_cleaned$definition_rv <- factor(sbr2018_cleaned$definition_rv, levels = definition_fac)

table(sbr2018_cleaned$definition_rv)

N = dim(sbr2018_cleaned)[1]

getd.i <- as.numeric(sbr2018_cleaned$definition_rv)

Y = log(sbr2018_cleaned$SBR) - sbr2018_cleaned$def_bias
var_i = sbr2018_cleaned$SE.logsbr^2 + sbr2018_cleaned$def_sd^2 

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
datatype1.i <- ifelse(sbr2018_cleaned$source2==1,1,0)   # admin
datatype2.i <- ifelse(sbr2018_cleaned$source2==2,1,0)   # HMIS
datatype3.i <- ifelse(sbr2018_cleaned$source2==3,1,0)   # subnat.lr
datatype4.i <- ifelse(sbr2018_cleaned$source2==4,1,0)   # survey

getj.i <- sbr2018_cleaned$source2

stan.data<- list(Y = Y, var_i = var_i, unadj_Y = log(sbr2018_cleaned$SBR),
                 covar_array = covar_array, definition_rv = sbr2018_cleaned$definition_rv,
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
saveRDS(stan.data,file = "output/stan.qi1.rds")


#saveRDS(stan.data,file = "output/stan.qi1.loo.rds")



#model_data_list <- create_list_for_country(sbr2018_cleaned) 
#pdf_name <- paste0("fig/exploratory_plot/model_data_clean.pdf")
#pdf(pdf_name,width=12)
#model_data_list %>% lapply(exploratory_plot)
#dev.off()

