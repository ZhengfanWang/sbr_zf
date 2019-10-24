covarset <- read_dta("input/covar/mcee_covariates_20190625.dta",encoding='latin1')%>% 
  select("iso3","year","gni","nmr","lbw","anc4","mean_edu") %>% 
  dplyr::rename("iso"="iso3") %>% 
  merge(countryRegionList,by="iso") %>% 
  filter(year>=2000) %>% 
  arrange(iso,year) 
  
covarset
saveRDS(covarset,"output/covar_0625.rds")
