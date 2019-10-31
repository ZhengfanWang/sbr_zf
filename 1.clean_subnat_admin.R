# Clean & subnational admin
# Last modified: Oct22, 2019


##read subnational admin
subnat_admin.ori <- openxlsx::read.xlsx("input/2019_09_10_Stillbirth database. SubNat. To use_Admin_SB_input_FINAL.xlsx",
                                        sheet =  1,startRow = 2) # read in subnation data 7786 obs

sub.admin.full <- subnat_admin.ori %>%  dplyr::rename("country"="Country",
                                              "year"="Reference_year",
                                              "NMR"="nmr",
                                              "region"="Region_name_eng",
                                              "nLB"="lb",
                                              "nSB"="sb") %>% 
  mutate(source="subnat.admin",nTB=nLB+nSB,definition=as.factor(Definition_SB),SBR=as.numeric(paste(sbr)),
         context=as.factor(Context)) %>% 
  merge(countryRegionList,by = "country") %>% 
  mutate(exclusion_notes = replace(exclusion_notes,is.na(SBR),"missing SBR")) %>% 
  select("country","iso","year","definition","context","SBR","NMR","nSB","nTB","nLB","source","region","exclusion_notes") # 7665

#levels(sub.admin.full$definition)
levels(sub.admin.full$definition) <- c("ge1000g","ge12wks","ge20wks","ge22wks","ge500gORge22wks","ge500gORge22wks","ge26wks","ge28wks",
                                   "ge500g","ge500gORge22wks","ge20wks","ge28wks","ge800gORge26wks","any","any","not defined")

levels(sub.admin.full$context) <- c("HMIS-DHIS2","VR","VR")

sub.admin.full <- sub.admin.full %>% arrange(iso, year,region) %>% 
                                     mutate(rSN = SBR/NMR) %>%  
                                     mutate(definition = replace(definition,is.na(definition)&country=="Bhutan","ge28wks")) %>% 
                                     mutate(definition_rv=definition,adj_sbr_unknown=NA,prop_unknown=NA,
                                            nNM = NA, rSN_UN = NA, notes = NA, WPP_LB = NA, UN_NMR = NA ) %>% 
                                     select("country","iso","region","year","source","context","definition","definition_rv",
                                    "SBR","adj_sbr_unknown","prop_unknown","nSB","nTB","nLB", "WPP_LB",
                                    "nNM","NMR","UN_NMR","rSN","rSN_UN","notes","exclusion_notes") 

saveRDS(sub.admin.full, "output/sub.admin.full.rds")
#write.csv(table(region.cleaned),file = "subnational_admin_Notes.on.Place.in.Country.csv")

