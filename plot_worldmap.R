library(rworldmap)
vignette('rworldmap')

library(haven)
library(tidyverse)

#my data

dat <- readRDS("rdsoutput/Pspline1.rds")


source("R/iso.R")

fitchain <- rstan::extract(dat)
df <- fitchain
mu_ct <- df$mu_ct +df$delta_ct  

year <- 2018
t <- year-1999
muhat <-c()
for(c in 1:195){
    cache_mu_ct <- mean(mu_ct[,c,t])
    cache_covar <- cache_mu_ct 
    cache_exp_mu_rt <- exp(cache_covar)
    muhat[c] <- cache_exp_mu_rt
}
SBR <- muhat



#national_covar <- read_dta("input/national_covariates.dta",encoding='latin1')
#countryRegionList <- national_covar[,c(1,2,8)] %>% distinct()  
#CountryRegionList <- mutate(countryRegionList,country_idx=as.numeric(factor(countryRegionList$iso3)))

iso3 <- countryRegionList[,1]


### data frame of my est and iso3 code
countryExData <- data.frame(iso3 =iso3,SBR) %>% mutate(iso3 = iso)


dim(countryExData)
#### 

library(classInt)
library(RColorBrewer)
#getting smallexample data and joining to a map

sPDF <- joinCountryData2Map(countryExData
                            ,joinCode = "ISO3"
                            ,nameJoinColumn = "iso3"
                          )

#getting class intervals
#classInt <- classIntervals( sPDF[["SBR"]]
#                            ,n=5, style = "jenks")

#catMethod = round(classInt[["brks"]])

catMethod = c(1,4,8,13,20,50)
#getting colours
colourPalette <- brewer.pal(5,'RdPu')

#plot map
mapDevice() #create world map shaped window
mapParams <- mapCountryData(sPDF
                            ,nameColumnToPlot="SBR"
                            ,addLegend=FALSE
                            ,catMethod = catMethod
                            ,mapTitle = "Stillbirth in 2018"
                            ,colourPalette=colourPalette )

do.call(addMapLegend
        ,c(mapParams
           ,legendLabels="all"
           ,legendWidth=0.5
           ,legendIntervals="data"
           ,legendMar = 2))

