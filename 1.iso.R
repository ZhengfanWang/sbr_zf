



national_covar <- read_dta("input/covar/national_covariates.dta",encoding='latin1')
countryRegionList <- national_covar[,c(1,2,8)] %>% distinct() %>% 
                     dplyr::rename("iso"="iso3") %>% 
                     mutate(country_idx=as.numeric(factor(iso))) 
                      

inregion <- openxlsx::read.xlsx("input/Regional Groupings.xlsx", sheet = 1)
income_region <- inregion %>% select(ISOCode,WorldBank_IncomeGroup_June2017) %>% 
                              dplyr::rename("iso" = "ISOCode", "icgroup" = "WorldBank_IncomeGroup_June2017")
sdgregion <- read.csv("input/sbr_regions.csv")
sdg_region <- sdgregion %>% select(ISO3Code,sdg.rev1) %>% 
                            dplyr::rename("iso"="ISO3Code","sdg" = "sdg.rev1")
countryRegionList <- countryRegionList %>% inner_join(income_region,by="iso") %>% 
                                           mutate(lmic = ifelse(icgroup=="High income",0,1)) %>% 
                                           select(iso,country,shmdg2,icgroup,lmic,country_idx) %>% 
                                           inner_join(sdg_region,by = "iso")
#countryRegionList


