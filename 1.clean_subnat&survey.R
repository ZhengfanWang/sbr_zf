# Clean subnational lit review & survey data
# Last modified: Oct.22.2019

##read subnational lit review 
subnat_lit_rv.ori <- openxlsx::read.xlsx("input/2019_subnational_lit_rv_2019-11-22.xlsx", sheet =  3, startRow = 2) # read in subnation data

#save(subnat_lit_rv.ori,file = "input/rdata/subnat_lit_rv.ori.rdata")
subnat.full <- subnat_lit_rv.ori %>% dplyr::rename("iso"="isocode",
                                            "year"="reference_year",
                                            "SBR"="sbr",
                                            "exclude"="exclude_2019",
                                            "country"="Country",
                                            "Context.grouped"="context",
                                            "nSB"="sb",
                                            "nLB"="lb",
                                            "nTB"="tb",
                                            "nNM"="nnd",
                                            "notes"="comments",
                                            "source_name"="source_1",
                                            "UN_NMR"="UNIGME2019_NMR") %>%
                      mutate(NMR = as.numeric(paste(nmr)),
                             definition = as.factor(def_revised),
                             context=as.factor(Context.grouped),
                             region = NA,
                             source="subnat.LR",
                             exclusion_notes=NA) %>% 
                      mutate(exclusion_notes = replace(exclusion_notes,year < 2000,"prior to 2000")) %>% 
                      mutate(exclusion_notes = replace(exclusion_notes,!is.na(exclude),"exclude by exclude col")) %>%
                      mutate(exclude_col = ifelse(!is.na(exclude),"exclude by exclude col",NA)) %>% 
                      mutate(exclusion_notes = replace(exclusion_notes,is.na(SBR),"missing SBR")) %>% 
                      mutate(NMR = ifelse(is.na(NMR),nNM/nLB*1000,NMR)) %>% 
                      mutate(rSN = SBR/NMR,
                             rSN_UN = SBR/UN_NMR) %>%
                      select("uniqueID","country","iso","region","year","source","context","definition","SBR",
                             "nSB","nTB","nLB","nNM","NMR","rSN","rSN_UN","UN_NMR",
                             "notes","source_name","exclusion_notes","exclude_col") # now 164 obs


# definitions                
#levels(subnat.full$definition)
levels(subnat.full$definition) <- c("ge28wks","ge1000g","ge1000gANDge28wks","ge1000gORge28wks","ge20wks",
                                    "ge20wks","ge500gORge20wks","ge22wks","ge22wks","ge22wks",
                                    "ge500gORge22wks","ge24wks","ge24wks","ge1000gORge26wks","s40wksANDge28wks",
                                    "ge28wks","ge28wks","ge1000gORge28wks","ge500g","ge500gORge22wks",
                                    "ge24wks","ge28wks","ge28wks","any","not defined",
                                    "not defined")



############################
#revalue code does not work for "â¥" 
if(FALSE){
definition.cleaned2 <- plyr::revalue(subnat.full$definition,
                                 c("â¥ 28 weeks"="ge28wks",
                                 "â¥1000g"="ge1000g",
                                 "â¥1000g and â¥28 weeks"="ge1000gANDge28wks",
                                 "â¥1000g or â¥28wks"="ge1000gORge28wks",
                                 "â¥20 weeks"="ge20wks",
                                 "â¥20wks"="ge20wks",
                                 "â¥20wks or â¥500g"="ge500gORge20wks",
                                 "â¥22weeks"="ge22wks",
                                 "â¥22wks or â¥500g"="ge500gORge22wks",
                                 "â¥24 weeks"="ge24wks",
                                 "â¥24wks"="ge24wks",
                                 "â¥26wks or â¥1000g"="ge1000gORge26wks",
                                 "â¥28 and <40wks"="le40wksANDge28",
                                 "â¥28 weeks"="ge28wks",
                                 "â¥28wks"="ge28wks",
                                 "â¥28wks or â¥1000g"="ge1000gORge28wks",
                                 "â¥500g"="ge500g",
                                 "â¥500g or â¥22weeks"="ge500gORge22wks",
                                 "â¥6 months"="ge24wks",
                                 "â¥7 months"="ge28wks",
                                 "any gest"="any",
                                 "other or not defined"="not defined")
                                 )
}

#levels(subnat.full$context)
levels(subnat.full$context) <- c("HF min bias","pop based","pop based")

# exclude the rest of not defined
subnat.full <- subnat.full %>% mutate(adj_sbr_unknown = NA, prop_unknown = NA, definition_rv = definition, WPP_LB = NA) %>% 
                               select("uniqueID","country","iso","region","year","source","context","definition","definition_rv",
                                      "SBR","adj_sbr_unknown","prop_unknown","nSB","nTB","nLB","WPP_LB","nNM","NMR",
                                      "UN_NMR","rSN","rSN_UN","notes","exclusion_notes","exclude_col") %>%
                               arrange(iso,year) # now 141 obs

saveRDS(subnat.full, "output/subnat.lit.rv.full.rds")
###############################

#-------------#
#   survey    #
#-------------#
##read survey data
survey.ori <- openxlsx::read.xlsx("input/Survey_Stillbirth_database_2019-12-11.xlsx", sheet = 1) # read in survey

survey.full <- survey.ori %>% dplyr::rename("country"="Country","iso"="ISO3Code",
                                "year"="ReferenceDate", 
                                "context"="Surveytype", 
                                "notes"="DataCollection", 
                                "NMR"="ES_NMR",
                                "UN_NMR"="UNIGME2019_NMR") %>%
                              mutate(definition = ifelse(is.na(ES_7pMonths),"ge24wks","ge28wks"),
                                     SE.sbr = ifelse(is.na(ES_7pMonths),SE_6pMonths,SE_7pMonths),
                                     nSB = ifelse(is.na(ES_7pMonths),ES_NSB_6pMonths,ES_NSB_7pMonths),
                                     nTB = ifelse(is.na(ES_7pMonths),ES_Totpreg6m,ES_Totpreg7m),
                                     SBR = ifelse(is.na(ES_7pMonths),ES_6pMonths,ES_7pMonths)) %>% 
                              mutate(source="survey", definition_rv = definition, nNM=NA, rSN_UN= SBR/UN_NMR, exclusion_notes = NA,exclusion_ratio = NA,
                                     adj_sbr_unknown=NA,prop_unknown=NA, region = NA, nLB = nTB - nSB, rSN = SBR/NMR,
                                     WPP_LB = NA) %>%
                              mutate(exclusion_notes = replace(exclusion_notes,year < 2000,"prior to 2000")) %>% 
                              mutate(exclusion_notes = replace(exclusion_notes,is.na(SBR),"missing SBR")) %>% 
                              mutate(exclusion_notes = replace(exclusion_notes,model_include==0,"U5MR model exclude")) %>% 
                              mutate(exclude_col = ifelse(model_include==0,"U5MR model exclude",NA)) %>%
                              select("uniqueID","country","iso","region","year","source","context","definition","definition_rv",
                                     "SBR","adj_sbr_unknown","prop_unknown","nSB","nTB","nLB","WPP_LB","nNM","NMR","UN_NMR","rSN","rSN_UN",
                                     "notes","exclusion_notes","exclude_col") 

saveRDS(survey.full, "output/survey.full.rds")



