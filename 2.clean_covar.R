###################################
# clean covariates set for hs prior variable selection
################################

interest_cov <- c("gni","nmr","lbw","anc4","mean_edu","exbf","gfr","gini","hdi","imr","u5mr","literacy_fem","ors","pfpr","anc1","sab",
                  "underweight","stunting","u5pop","urban","dtp3","mcv","bcg","pab","hib3","rota_last","pcv3","sanitation","water")

covarset <- read_dta("input/covar/mcee_covariates_20190625.dta",encoding='latin1')%>% 
                 select(c("iso3","year",interest_cov)) %>% 
                 dplyr::rename("iso"="iso3") %>% 
                 merge(countryRegionList,by="iso") %>% 
                 filter(year>=2000) %>% 
                 arrange(iso,year) 
  
names(covarset)
saveRDS(covarset,"output/covar_0625.rds")
