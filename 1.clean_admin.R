# Clean national admin data 
#   according to latest specifications and exclusion criteria
# Last modified: Oct.22, 2019

#---------------#
#   load data   #
#---------------#

admin.ori <- openxlsx::read.xlsx("input/Admin_Stillbirth_database_20200204.xlsx", sheet = 1, startRow = 2) # read in admin data


#-----------------------------#
#   fix known issues first    #
#-----------------------------#


# fill in missing nLB when we have SBR >0 and nSB
i.miss <- which(is.na(admin.ori$lb) & !is.na(admin.ori$sb) & admin.ori$sbr_rec>0)
admin.ori$lb[i.miss] <- admin.ori$sb[i.miss]*(1000-admin.ori$sbr_rec[i.miss])/admin.ori$sbr_rec[i.miss]

# fill in missing nLB when we have SBR >0 and nTB but no nSB
i.miss2 <- which(is.na(admin.ori$lb) & is.na(admin.ori$sb) & !is.na(admin.ori$tb) & admin.ori$sbr_rec>0)
admin.ori$lb[i.miss2] <- admin.ori$tb[i.miss2]*(1000-admin.ori$sbr_rec[i.miss2])/1000

# fill in missing SBR -- for now (includes some unknown def)
# not sure if we also need to change adj_sbr_unknown?
i.miss3 <- which(is.na(admin.ori$sbr_rec) & 
                   !is.na(admin.ori$sb) & !is.na(admin.ori$tb))
admin.ori$sbr_rec[i.miss3] <- admin.ori$sb[i.miss3]/admin.ori$tb[i.miss3]*1000

i.miss4 <- which(is.na(admin.ori$sbr_rec) & 
                   !is.na(admin.ori$sb) & !is.na(admin.ori$lb))
admin.ori$sbr_rec[i.miss4] <- admin.ori$sb[i.miss4]/(admin.ori$sb[i.miss4]+admin.ori$lb[i.miss4])*1000

#-----------------------------------------------#
#   first pass = exclude:                       #
#   - prior to year 2000                        #
#   - PAHO/WHO                                  #
#   - dq_flag in (3,9,12)                       #
#   - missing SBR with no way to calculate it   #
#   - a couple duplicate obs                    #
#                                               #
#   also exclude defn-obs that we don't need    #
#   - x500g and x28wks                          #
#-----------------------------------------------#
admin.full <- admin.ori %>% dplyr::rename("country"="Country",
                              "iso"="iso3","definition"="definition_rv",
                              "year"="Reference_year", "context"="Context",
                              "SBR"="sbr_rec",
                              "nSB"="sb", 
                              "nTB"="tb",
                              "nLB"="lb", 
                              "WPP_LB"="wpp_lb",
                              "NMR"="nmr_rec",
                              "nNM"="neo",
                              "UN_NMR"="UNIGME2019_NMR",
                              "dq_flag"="dq_flag2019",
                              "notes"="Coverage",
                              "source_name"="Source_1_name",
                              "inclusion.U5MR"="Inclusion.VR.U5MR") %>% 
                            mutate(exclusion_notes_full = NA) %>%
                            mutate(source="admin", LB_frac = nLB/WPP_LB, rSN = SBR/NMR, rSN_UN = SBR/UN_NMR,exclusion_notes=NA) %>%
                            mutate(source = replace(source,context=="HMIS-DHIS2","HMIS")) %>% 
                            mutate(exclusion_notes = replace(exclusion_notes, 
                                                             country=="Cyprus" & notes == "national public sector only",
                                                             "Cyprus public sector only")) %>%
                            mutate(exclusion_notes_full = ifelse(country %in% "Cyprus" & notes %in% "national public sector only",
                                                                 "Cyprus public sector only",exclusion_notes_full)) %>%
                            mutate(exclusion_notes = replace(exclusion_notes, inclusion.U5MR == 0, "excluded by U5MR")) %>%
                            mutate(exclusion_notes_full = ifelse(inclusion.U5MR==0 & !is.na(inclusion.U5MR), 
                                                                  paste(exclusion_notes_full,"excluded by U5MR",sep = ";"),
                                                                 exclusion_notes_full)) %>%   
                            mutate(exclude_col = ifelse(inclusion.U5MR==0 & !is.na(inclusion.U5MR), "excluded by U5MR",NA)) %>%
                            mutate(exclusion_notes = replace(exclusion_notes,
                                                            (source=="admin" & !(LB_frac >= 0.8 | WPP_LB <= 30000 | country=="Serbia" | context=="Sample Vital Registation")),
                                                            "low coverage")) %>%
                            mutate(exclusion_notes_full = ifelse(source=="admin" &  
                                                                   (!is.na(LB_frac) & !is.na(WPP_LB)) &
                                                                   (!(LB_frac >= 0.8 | WPP_LB <= 30000 | country=="Serbia" | context=="Sample Vital Registation")),
                                                                  paste(exclusion_notes_full,"low coverage",sep = ";"),exclusion_notes_full)) %>%
                            mutate(exclusion_notes = replace(exclusion_notes,
                                                             (source=="HMIS" & !(LB_frac >= 0.7 | WPP_LB <= 30000 | country=="Serbia" | context=="Sample Vital Registation")),
                                                             "low coverage")) %>%
                            mutate(exclusion_notes_full = ifelse(source=="HMIS" & (!(LB_frac >= 0.7 | WPP_LB <= 30000 | country=="Serbia" | context=="Sample Vital Registation")) & 
                                                                   (!is.na(LB_frac) & !is.na(WPP_LB)),
                                                                 paste(exclusion_notes_full,"low coverage",sep = ";"),exclusion_notes_full)) %>%
                            mutate(exclusion_notes = replace(exclusion_notes,
                                                             source_name == "PAHO/WHO (2018). Health Situation in the Americas: Core Indicators 2018.",
                                                             "PAHO/WHO data")) %>%
                            mutate(exclusion_notes_full = ifelse(source_name %in% "PAHO/WHO (2018). Health Situation in the Americas: Core Indicators 2018." , 
                                                                 paste(exclusion_notes_full,"PAHO/WHO data",sep = ";"),
                                                                 exclusion_notes_full)) %>% 
                            mutate(exclusion_notes = replace(exclusion_notes,dq_flag %in% c(3,9,12),"dq_flag in 3,9,12")) %>%
                            mutate(exclusion_notes_full = ifelse(dq_flag %in% c(3,9,12),
                                                                 paste(exclusion_notes_full,"dq_flag in 3,9,12",sep = ";"),
                                                                 exclusion_notes_full)) %>%
                            mutate(exclusion_notes = replace(exclusion_notes,dq_flag %in% 15,"Duplicate, use different system")) %>%
                            mutate(exclusion_notes_full = ifelse(dq_flag %in% 15,
                                                          paste(exclusion_notes_full,"Duplicate, use different system",sep = ";"),
                                                          exclusion_notes_full)) %>%
                            mutate(exclusion_notes = replace(exclusion_notes,is.na(SBR),"missing SBR")) %>%
                            mutate(exclusion_notes_full = ifelse(is.na(SBR), 
                                                                 paste(exclusion_notes_full,"missing SBR",sep = ";"),
                                                                 exclusion_notes_full)) %>%
                            mutate(exclusion_notes = replace(exclusion_notes, prop_unknown > 0.5 & !is.na(prop_unknown), "prop of unknown sb > 0.5")) %>%
                            mutate(exclusion_notes_full = ifelse(prop_unknown > 0.5 & !is.na(prop_unknown), 
                                                                 paste(exclusion_notes_full,"prop of unknown sb > 0.5",sep = ";"),
                                                                 exclusion_notes_full)) %>%
                            mutate(exclusion_notes = replace(exclusion_notes,year < 2000,"prior to 2000")) %>%
                            mutate(exclusion_notes_full = ifelse(year < 2000, 
                                                                 paste(exclusion_notes_full,"prior to 2000",sep = ";"),
                                                                 exclusion_notes_full)) %>% 
                            mutate(nSB_adj_unknown = ifelse(is.na(sb_adj_unknown),nSB,sb_adj_unknown)) %>%
                             select("uniqueID","iso","country","year","source","source_name","context","definition",
                                     "SBR","adj_sbr_unknown","nSB_adj_unknown", "prop_unknown","nSB","nTB","nLB","WPP_LB","LB_frac",
                                    "nNM","NMR","UN_NMR","rSN","rSN_UN","notes","exclusion_notes","exclude_col","exclusion_notes_full") 

#length(which(is.na(admin.full$exclusion_notes))) 
### number of obs 3670


#-----------------#
#   definitions   #
#-----------------#

#table(admin.full$definition)

#-----------------#
####definition.cleaned:   combine definitions with the same meaning
definition.cleaned <-  fct_collapse(admin.full$definition,
                                    any = c("any gest age","any gestational age","any weight"),
                                    unknownBWT = c("unknown birthweight","unknown bwt"),
                                    unknownGA=c("unknown GA","unknown gestational age"),
                                    ge20wks = c("x20wks","x20wks including unknown GA","x5mths"),
                                    ge22wks = c("x22wks"),
                                    ge28wks = c("x28wks","x7mths"),
                                    ge24wks = c("x24wks","180 days")
                                    )
#table(definition.cleaned)
#-----------------#
#### definition.cleaned2: rename definition in same scale.

### AM: Lucia said that 100g
definition.cleaned2 <- plyr::revalue(definition.cleaned,
                              c(">30cm"="ge30cm",
                                "x29wks"="ge29wks",
                                "x1000g"="ge1000g",
                                "x1000g and x28wks"="ge1000gANDge28wks",
                                "x1000g or x28wks"="ge1000gORge28wks",
                                "x22wks or x500g"="ge500gORge22wks",
                                "x26wks"="ge26wks",
                                "x28wks or x500g"="ge500gORge28wks",
                                "x35cm or x28 weeks"="ge35cmORge28wks",
                                "x37wks"="ge37wks",
                                "x400g or x20wks"="ge400gORge20wks",
                                "x500g"="ge500g",
                                "x500g or x22wks"="ge500gORge22wks",
                                "x500g and x28wks"="ge500gANDge28wks")
                              )
#table(definition.cleaned2)
admin.full <- admin.full %>% mutate(definition = definition.cleaned2,definition_rv=definition.cleaned2)


#-----------------#
#   Deal with "not defined" observation   #
#1)     If data from same source available for other years and time trend consistent 
#           - use definition given for other years
#2)     If no previous data from the same source from country, or change in time trend (i.e. evidence 
#                    to suggest that the definition used has changed) then:  
#  a.      For HMIS-DHIS2 - assume using 28 week definition
#-----------------#
levels(admin.full$definition_rv)

admin.full <- admin.full %>% merge(countryRegionList[!names(countryRegionList) %in% "country"],by=c("iso"))
nd.data <- admin.full %>%  filter(definition=="not defined")

notdefine.list <- unique(nd.data$country_idx)

#-----------------#
### FROM ANU: I COMMENTED THIS SECTION OUT SINCE I'VE IMPLEMENTED THESE IN THE DATABASE
###hard code 
####### ALB Albania
#admin.full %>% filter(country_idx==notdefine.list[1])%>%
#  arrange(iso,year)
#admin.full[which(admin.full$iso=="ALB"&admin.full$definition=="not defined"&admin.full$year==2010),]$definition_rv <- "ge28wks"

####### ARM armenia
#admin.full %>% filter(country_idx==notdefine.list[2])%>%
#  arrange(iso,year)
#admin.full[which(admin.full$iso=="ARM"&admin.full$definition=="not defined"),]$definition_rv <- "ge500gORge22wks"

####### Benin BEN
#admin.full %>% filter(country_idx==notdefine.list[3])%>%
#  arrange(iso,year)
#admin.full[which(admin.full$iso=="BEN"&admin.full$definition=="not defined"),]$definition_rv <- "ge28wks"

####### Burkina BFA
#admin.full %>% filter(country_idx==notdefine.list[4])%>%
#  arrange(iso,year)
#admin.full[which(admin.full$iso=="BFA"&admin.full$definition=="not defined"),]$definition_rv <- "ge22wks"

####### Bangladesh BGD
#admin.full %>% filter(country_idx==notdefine.list[5])%>%
#  arrange(iso,year)
#admin.full[which(admin.full$iso=="BGD"&admin.full$definition=="not defined"),]$definition_rv <- "ge28wks"

####### Belize BLZ
#admin.full %>% filter(country_idx==notdefine.list[6])%>%
#  arrange(iso,year)
#admin.full[which(admin.full$iso=="BLZ"&admin.full$definition=="not defined"),]$definition_rv <- "ge22wks"
####### Bhutan BTN missing SBR
#admin.full %>% filter(country_idx==notdefine.list[7])%>%
#  arrange(iso,year)
#admin.full[which(admin.full$iso=="BTN"&admin.full$definition=="not defined"),]$definition_rv <- "ge28wks"

####### Germany DEU
#admin.full %>% filter(country_idx==notdefine.list[8])%>%
#  arrange(iso,year)
#admin.full[which(admin.full$iso=="DEU"&admin.full$definition=="not defined"),]$definition_rv <- "ge28wks"

####### Djibouti DJI
#admin.full %>% filter(country_idx==notdefine.list[9])%>%
#  arrange(iso,year)
#admin.full[which(admin.full$iso=="DJI"&admin.full$definition=="not defined"),]$definition_rv <- "ge28wks"

####### Iraq IRQ do not need to change. It has 28wks definition already
#admin.full %>% filter(country_idx==notdefine.list[10])%>%
#  arrange(iso,year)

####### Kenya KEN
#admin.full %>% filter(country_idx==notdefine.list[11])%>%
#  arrange(iso,year)
#admin.full[which(admin.full$iso=="KEN"&admin.full$definition=="not defined"),]$definition_rv <- "ge1000gORge28wks"

####### LCA Saint
#admin.full %>% filter(country_idx==notdefine.list[12])%>%
#  arrange(iso,year)
#admin.full[which(admin.full$iso=="LCA"&admin.full$definition=="not defined"),]$definition_rv <- "ge1000gORge28wks"

####### LKA Sri Lanka
#admin.full %>% filter(country_idx==notdefine.list[13])%>%
#  arrange(iso,year)
#admin.full[which(admin.full$iso=="LKA"&admin.full$definition=="not defined"),]$definition_rv <- "ge28wks"

####### LTU Lithuania 
#admin.full %>% filter(country_idx==notdefine.list[14])%>%
#  arrange(iso,year)

####### MAR Morocco
#admin.full %>% filter(country_idx==notdefine.list[15])%>%
#  arrange(iso,year)
#admin.full[which(admin.full$iso=="MAR"&admin.full$definition=="not defined"),]$definition_rv <- "ge28wks"

####### MCO  Monaco: small country, missing source, not change definition
#admin.full %>% filter(country_idx==notdefine.list[16])%>%
#  arrange(iso,year)

####### MEX  Mexico :excluded because of old data, no need to change.
#admin.full %>% filter(country_idx==notdefine.list[17])%>%
#  arrange(iso,year)

####### RUS Russian: missing SBR, no need to change
#admin.full %>% filter(country_idx==notdefine.list[18])%>%
#  arrange(iso,year)

####### SAU Saudi Arabia
#admin.full %>% filter(country_idx==notdefine.list[19])%>%
#  arrange(iso,year)
#admin.full[which(admin.full$iso=="SAU"&admin.full$definition=="not defined"),]$definition_rv <- "ge28wks"

####### SDN   Sudan
#admin.full %>% filter(country_idx==notdefine.list[20])%>%
#  arrange(iso,year)
#admin.full[which(admin.full$iso=="SDN"&admin.full$definition=="not defined"),]$definition_rv <- "ge28wks"

####### SEN Senegal
#admin.full %>% filter(country_idx==notdefine.list[21])%>%
#  arrange(iso,year)

####### SWZ Swaziland
#admin.full %>% filter(country_idx==notdefine.list[22])%>%
#  arrange(iso,year)
#admin.full[which(admin.full$iso=="SWZ"&admin.full$definition=="not defined"),]$definition_rv <- "ge28wks"

####### TCD    Chad
#admin.full %>% filter(country_idx==notdefine.list[23])%>%
#  arrange(iso,year)
#admin.full[which(admin.full$iso=="TCD"&admin.full$definition=="not defined"),]$definition_rv <- "ge28wks"

####### TGO    Togo
#admin.full %>% filter(country_idx==notdefine.list[24])%>%
#  arrange(iso,year)
#admin.full[which(admin.full$iso=="TGO"&admin.full$definition=="not defined"),]$definition_rv <- "ge28wks"

####### TKM Turkmenistan
#admin.full %>% filter(country_idx==notdefine.list[25])%>%
#  arrange(iso,year)

####### TUN Tunisia
#admin.full %>% filter(country_idx==notdefine.list[26])%>%
#  arrange(iso,year)

####### TUR  Turkey
#admin.full %>% filter(country_idx==notdefine.list[27])%>%
#  arrange(iso,year)
#admin.full[which(admin.full$iso=="TUR"&admin.full$definition=="not defined"),]$definition_rv <- "ge500gORge22wks"

####### URY Uruguay
#admin.full %>% filter(country_idx==notdefine.list[28])%>%
#  arrange(iso,year)

####### UZB Uzbekistan
#admin.full %>% filter(country_idx==notdefine.list[29])%>%
#  arrange(iso,year)

####### ZWE Zimbabwe
#admin.full %>% filter(country_idx==notdefine.list[30])%>%
#  arrange(iso,year)
#admin.full[which(admin.full$iso=="ZWE"&admin.full$definition=="not defined"),]$definition_rv <- "ge28wks"
##### ARM Armenia: Two time serises available: ge1000gORge28wks and ge500gORge22wks. Observations with not define def
# are close to ge500gORge22wks. Change "not defined" to ge500gORge22wks.
##### Bangladesh: The differences between not defined(source:HMIS-DHIS2) and ge28wks(Sample VR) are large.
##### Belize: small country,VR data, multiple time trend for different defitions. Observations with 
#      "not defined" definition are close to 22wks.
#### Saint: small country, no other definition available. Keep "not defined"
#### Lithuania: obsevation with not defined are old observations. No need to change.
####### MCO  Monaco: small country, missing source, not change definition
####### SEN Senegal: obs from 2003 to 2008 are excluded according to dq_flag in 3,9,12.
#                    two observation with not defined in 2009 are HMIS-DHIS2 data 
#                    but not national data(1.Maternity hospitals in Louga region;
#                   2. Maternity hosptials in Ziguinchor region);
#                   Recommend to exclude observation in 2009
####### TKM Turkmenistan:   not defined, VA data from 1991 to 2016, no other definition. keep not defined.
####### TUN Tunisia:not defined, VA data from 2002 to 2012, no other definition. keep not defined.
####### URY Uruguay: No need to change observations with not defined definition. Other definitions available.
####### UZB Uzbekistan: all observations are excluded by dq_flag in 2,9,12



#----------------------#
#   finialize admin    #
#----------------------#


#admin.full <- admin.full %>% mutate(region=NA,
#                                    iso = as.factor(iso)) %>% 
#                            mutate(exclusion_notes = replace(exclusion_notes,
#                                                             context == "Vital Registration"&year %in% c(2011,2014)&iso=="POL",
#                                                             "duplicates, use BDR data")) %>% 
#                            #mutate(exclusion_notes = replace(exclusion_notes,
#                            #                                context == "Vital Registration"&year==2001&iso=="HRV",
#                            #                                "duplicates, use BDR data")) %>% 
#                            mutate(exclusion_notes = replace(exclusion_notes,
##                                                             context == "Vital Registration"&year%in% 2005:2017&iso=="GEO",
#                                                            "duplicates, use BDR data")) %>% 
#                           mutate(exclusion_notes = replace(exclusion_notes,
#                                                            context == "Vital Registration"&year%in% 2000:2015&iso=="NLD",
#                                                            "duplicates, use BDR data")) %>% 
#                           mutate(exclusion_notes = replace(exclusion_notes,
#                                                            year%in% 2000:2016 & iso=="SRB" & definition_rv == "ge1000g",
#                                                            "duplicates, use ge500gANDge28wks def")) %>% 
#                           #mutate(exclusion_notes = replace(exclusion_notes,
#                           #                                  year%in% 2000:2016 & iso=="CHE" & definition_rv != "ge1000gORge28wks",
#                           #                                  "duplicates, use ge1000gORge28wks def")) %>%
#                            mutate(exclusion_notes = replace(exclusion_notes,
#                                                             iso=="OMN" & definition != "ge1000g",
#                                                             "duplicates, use def 1000g")) %>% 
#                           mutate(exclusion_notes = replace(exclusion_notes,
#                                                            iso == "SRB" & definition == "ge500gORge28wks",
#                                                            "duplicates, use def 500g")) %>% 
#                           mutate(exclusion_notes = replace(exclusion_notes, 
#                                                           iso=="DEU" & year==2014 & definition_rv == "ge28wks" & 
#                                                           context == "Vital Registration", 
#                                                           "duplicates, use BDR data")) %>% 
#                           #dupcliate peristat data
#                           mutate(exclusion_notes = replace(exclusion_notes, 
#                                                    iso=="ROU" & year==2010 & definition_rv == "ge1000g" & 
#                                                      source_name == "EURO-PERISTAT project", 
#                                                    "duplicate Peristat, use VR data")) %>% 
#                           mutate(exclusion_notes = replace(exclusion_notes, 
#                                                    iso=="NOR" & year==2015 & definition_rv == "ge1000g" & 
#                                                      source_name == "EURO-PERISTAT project", 
#                                                    "duplicate Peristat, use VR data")) %>%
#                           mutate(exclusion_notes = replace(exclusion_notes, 
#                                                    iso=="IRL" & year==2010 & 
#                                                      source_name == "EURO-PERISTAT project", 
#                                                    "duplicate Peristat, use VR data")) %>%
#                           mutate(exclusion_notes_full = ifelse(grepl("duplicates",exclusion_notes),
#                                                                 paste(exclusion_notes_full,exclusion_notes,sep=";"),exclusion_notes_full)) %>%
#                            arrange(iso, year) %>% 
#                            select("uniqueID","country","iso","region","year","source","source_name","context","definition","definition_rv",
#                                   "SBR","adj_sbr_unknown","nSB_adj_unknown", "prop_unknown","nSB","nTB","nLB","WPP_LB",
#                                   "nNM","NMR","UN_NMR","rSN","rSN_UN","notes","exclusion_notes","exclude_col","exclusion_notes_full") 


saveRDS(admin.full, "output/admin.full.rds")


#########################   1. go to admin_plot.R to get exploratory plot     #############################
#admin_data_list <- create_list_for_country(admin.full)
#pdf_name <- paste0("fig/exploratory_plot/exploratory_admin_wo_exclusion.pdf")
#pdf(pdf_name,width=12)
#admin_data_list %>% lapply(exploratory_plot)
#dev.off()
#########################    go to 1.clean_xxx to clean other datasets      #############################





###################
#----------------------------- OLD CODE -----------------------------#
#
# admin$definition <- droplevels(admin$definition)
# table(admin$definition)
# definition.cleaned <-  fct_collapse(admin$definition,
#                                     any = c("any gest age","any gestational age","any weight"),
#                                     # ge20wks = c("x20wks","x5mths"), #?
#                                     ge28wks = c("x28wks","x7mths"),
#                                     ge500gORge22wks = c("x22wks or x500g","x500g or x22wks")
# )
# levels(definition.cleaned)
# # note: x5mths is only Panama (?), which also has x7mths definition
# # doesn't seem worthwhile to mix the 5mths in with 20wks or 22wks (other countries)?
# library(plyr)
# definition.cleaned2 <- revalue(definition.cleaned,
#                                c(">30cm"="ge30cm",
#                                  "180 days"="ge180days",
#                                  "x1000g"="ge1000g",
#                                  "x1000g and x28wks"="ge1000gANDge28wks",
#                                  "x1000g or x28wks"="ge1000gORge28wks",
#                                  "x20wks"="ge20wks",
#                                  "x22wks"="ge22wks",
#                                  "x24wks"="ge24wks",
#                                  # "x35cm or x28 weeks"="ge35cmORge28wks",
#                                  "x37wks"="ge37wks",
#                                  "x400g or x20wks"="ge400gORge20wks",
#                                  "x500g"="ge500g",
#                                  "x500g and x28wks"="ge500gANDge28wks",
#                                  "x5mths"="ge5mths")
# )
# levels(definition.cleaned2)
# # detach plyr manually on MF's computer
# admin <- admin %>% mutate(definition = definition.cleaned2)
# table(admin$definition)
# table(admin$context)
# table(admin$definition, admin$context)
# 
# # for HMIS-DHIS2 & "not defined", assume ge28wks
# admin$definition[admin$definition=="not defined" & admin$context=="HMIS-DHIS2"] <- "ge28wks"
# # exclude the rest of "not defined"
# admin <- admin %>% filter(definition != "not defined") # now 3612 obs
# admin$definition <- droplevels(admin$definition)
# table(admin$definition)
# 
# #---------------#
# #   coverage    #
# #---------------#
# # The rules:
# # If LB_frac < 0.8, exclude UNLESS
# # Serbia
# # Sample VR
# # small country (use different criteria)
# ##### Cyprus: remove `national public sector only'
# # also exclude if nLB=NA
# 
# c.small <- unique(admin$country[which(admin$WPP_LB <= 30000)]) # could be 20000 with same result
# admin_clean <- admin %>% filter(LB_frac >= 0.8 | 
#                                   country %in% c.small | 
#                                   country=="Serbia" |
#                                   context=="Sample Vital Registation") %>%
#   filter(!(country=="Cyprus" & notes=="national public sector only")) %>%
#   filter(!is.na(nLB)) %>%
#   mutate(SE = sqrt(1000*SBR/nTB)) %>%
#   select("country","iso","year","source","context","definition",
#          "SBR","SE","nSB","nTB","nLB",#"WPP_LB","LB_frac","dq_flag",
#          "nNM","NMR",#"UN_NMR",
#          "rSN","rSN_UN","notes") %>%
#   arrange(iso, year) # now 3220 obs
# 
# table(admin_clean$context)
# admin_clean$context <- droplevels(admin_clean$context)
# levels(admin_clean$context)[3] <- "Sample VR"
# # could also collapse B/D & V registry if we want?
# table(admin_clean$context)
# 
# # plot(rank(admin_clean$LB_frac), admin_clean$LB_frac)
# # test <- admin_clean %>% filter(LB_frac < 0.4) # just the sample VRs
# 
# #-------------------#
# #   SBR:NMR ratio   # FOR NOW
# #-------------------#
# 
# # just using the previous cutoff for now
# admin_clean <- admin_clean %>% filter(!(pmax(rSN,rSN_UN,na.rm=T) < 0.33)) # now 3164
