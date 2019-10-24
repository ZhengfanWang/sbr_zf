# Clean subnational lit review & survey data
# Last modified: Oct.22.2019

##read subnational lit review 
subnat_lit_rv.ori <- openxlsx::read.xlsx("input/2019_subnational_lit_rv_FINAL_091019.xlsx", sheet =  3, startRow = 2) # read in subnation data

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
                                            "source_name"="source_1") %>%
                      mutate(NMR = as.numeric(paste(nmr)),
                             definition = as.factor(def_revised),
                             context=as.factor(Context.grouped),
                             SE.sbr = sqrt(1000*SBR/nTB),
                             SE.logsbr = SE.sbr/SBR,
                             rSN_UN = NA,
                             region = NA,
                             source="subnat.LR",
                             exclusion_notes=NA) %>% 
                      mutate(exclusion_notes = replace(exclusion_notes,year < 2000,"prior to 2000")) %>% 
                      mutate(exclusion_notes = replace(exclusion_notes,!is.na(exclude),"exclude by exclude col")) %>% 
                      mutate(exclusion_notes = replace(exclusion_notes,is.na(SBR),"missing SBR")) %>% 
                      mutate(NMR = ifelse(is.na(NMR),nNM/nLB*1000,NMR)) %>% 
                      mutate(rSN = SBR/NMR) %>%
                      select("country","iso","region","year","source","context","definition",
                             "SBR","SE.logsbr","SE.sbr","nSB","nTB","nLB","nNM","NMR","rSN","rSN_UN",
                             "notes","source_name","exclusion_notes") # now 164 obs


# definitions                


levels(subnat.full$definition)
levels(subnat.full$definition) <- c("ge28wks","ge1000g","ge1000gANDge28wks","ge1000gORge28wks","ge20wks","ge20wks",
                                    "ge500gORge20wks","ge22wks","ge22wks","ge500gORge22wks","ge24wks","ge24wks",
                                    "ge1000gORge26wks","s40wksANDge28wks","ge28wks","ge28wks","ge1000gORge28wks","ge500g",
                                    "ge500gORge22wks","ge24wks","ge28wks","any","not defined","not defined")

#recode for factor does not work for ZF
############################
#definition.cleaned2 <- dplyr::recode(definition.cleaned,
#                               `≥ 28 weeks`="ge28wks",
#                                 "≥1000g"="ge1000g",
#                                 "≥1000g and ≥28 weeks"="ge1000gANDge28wks",
#                                 "≥1000g or ≥28wks"="ge1000gORge28wks",
#                                 "≥20 weeks"="ge20wks",
#                                 "≥20wks"="ge20wks",
#                                 "≥20wks or ≥500g"="ge500gORge20wks",
#                                 "≥22weeks"="ge22wks",
#                                 "≥22wks or ≥500g"="ge500gORge22wks",
#                                 "≥24 weeks"="ge24wks",
#                                 "≥24wks"="ge24wks",
#                                 "≥26wks or ≥1000g"="ge1000gORge26wks",
#                                 "≥28 and <40wks"="le40wksANDge28",
#                                 "≥28 weeks"="ge28wks",
#                                 "≥28wks"="ge28wks",
#                                 "≥28wks or ≥1000g"="ge1000gORge28wks",
#                                 "≥500g"="ge500g",
#                                 "≥500g or ≥22weeks"="ge500gORge22wks",
#                                 "≥6 months"="ge24wks",
#                                 "≥7 months"="ge28wks",
#                                 "any gest"="any",
#                                 "other or not defined"="not defined"
#                                 )







levels(subnat.full$context) <- c("HF min bias","pop based","pop based")


# exclude the rest of not defined
subnat.full <- subnat.full %>% mutate(exclusion_notes = replace(exclusion_notes,definition == "not defined","not defined definition"),
                                       adj_sbr_unknown = NA, prop_unknown = NA, definition_rv = definition) %>% 
                               select("country","iso","region","year","source","context","definition","definition_rv",
                                      "SBR","adj_sbr_unknown","prop_unknown","SE.logsbr","SE.sbr","nSB","nTB","nLB","nNM","NMR","rSN","rSN_UN","notes","exclusion_notes") %>%
                               arrange(iso,year) # now 141 obs


saveRDS(subnat.full, "output/subnat.lit.rv.full.rds")


###############################

#-------------#
#   survey    #
#-------------#


##read survey data
survey.ori <- openxlsx::read.xlsx("input/Survey_Stillbirth_database_2019-10-22.xlsx", sheet = 1) # read in survey

names(survey.ori)


survey.full <- survey.ori %>% dplyr::rename("country"="Country","iso"="ISO3Code",
                                "year"="ReferenceDate", 
                                "context"="Surveytype", 
                                "notes"="DataCollection", 
                                "NMR"="ES_NMR") %>%
                              mutate(definition = ifelse(is.na(ES_7pMonths),"ge24wks","ge28wks"),
                                     SE.sbr = ifelse(is.na(ES_7pMonths),SE_6pMonths,SE_7pMonths),
                                     nSB = ifelse(is.na(ES_7pMonths),ES_NSB_6pMonths,ES_NSB_7pMonths),
                                     nTB = ifelse(is.na(ES_7pMonths),ES_Totpreg6m,ES_Totpreg7m),
                                     SBR = ifelse(is.na(ES_7pMonths),ES_6pMonths,ES_7pMonths)) %>% 
                              mutate(source="survey", definition_rv = definition, nNM=NA, rSN_UN=NA, exclusion_notes = NA,exclusion_ratio = NA,
                                     adj_sbr_unknown=NA,prop_unknown=NA, region = NA, SE.logsbr = SE.sbr/SBR, nLB = nTB - nSB, rSN = SBR/NMR ) %>%
                              mutate(exclusion_notes = replace(exclusion_notes,year < 2000,"prior to 2000")) %>% 
                              mutate(exclusion_notes = replace(exclusion_notes,is.na(SBR),"missing SBR")) %>% 
                              select("country","iso","region","year","source","context","definition","definition_rv",
                                     "SBR","adj_sbr_unknown","prop_unknown","SE.logsbr","SE.sbr","nSB","nTB","nLB","nNM","NMR","rSN","rSN_UN",
                                     "notes","exclusion_notes") 

saveRDS(survey.full, "output/survey.full.rds")


