#  GEt full data set
# Last modified: Oct.22, 2019


#---------------#
#   load data   #
#---------------#
admin <- readRDS("output/admin.full.rds")
subnat.lr <- readRDS("output/subnat.lit.rv.full.rds")
subnat.lr$nSB_adj_unknown <- subnat.lr$nSB
subnat.lr$exclusion_notes_full <- subnat.lr$exclusion_notes
subnat.lr$source_name <- NA

subnat.admin <- readRDS("output/sub.admin.full.rds")
subnat.admin$exclude_col  <- NA
subnat.admin$exclusion_notes_full <- subnat.admin$exclusion_notes
subnat.admin$nSB_adj_unknown <- subnat.admin$nSB
subnat.admin$source_name <- NA

survey <- readRDS("output/survey.full.rds")
survey$nSB_adj_unknown <- survey$nSB
survey$exclusion_notes_full <- survey$exclusion_notes
survey$source_name <- NA

#-------------#
#   merge     # 
#-------------#
SBR.full.ori <- rbind(admin, subnat.lr, survey,subnat.admin) # 13789 obs

#################################################

# fill in missing nLB when we have nTB and nSB
i.miss1 <- which(is.na(SBR.full.ori$nLB)& (!is.na(SBR.full.ori$nTB-SBR.full.ori$nSB)))
SBR.full.ori$nLB[i.miss1] <- (SBR.full.ori$nTB-SBR.full.ori$nSB)[i.miss1]
# fill in missing nTB when we have nLB and nSB
i.miss2 <- which(is.na(SBR.full.ori$nTB)& (!is.na(SBR.full.ori$nSB+SBR.full.ori$nLB)))
SBR.full.ori$nTB[i.miss2] <- (SBR.full.ori$nSB+SBR.full.ori$nLB)[i.miss2]

# fill in missing nLB when we have SBR >0 and nSB
i.miss3 <- which(is.na(SBR.full.ori$nLB) & !is.na(SBR.full.ori$nSB) & SBR.full.ori$SBR>0)
SBR.full.ori$nLB[i.miss3] <- SBR.full.ori$nSB[i.miss3]*(1000-SBR.full.ori$SBR[i.miss3])/SBR.full.ori$SBR[i.miss3]
SBR.full.ori$nTB[i.miss3] <- SBR.full.ori$nLB[i.miss3] + SBR.full.ori$nSB[i.miss3]

# fill in missing nLB when we have SBR >0 and nTB but no nSB
i.miss4 <- which(is.na(SBR.full.ori$nLB) & is.na(SBR.full.ori$nSB) & !is.na(SBR.full.ori$nTB) & SBR.full.ori$SBR>0)
SBR.full.ori$nLB[i.miss4] <- SBR.full.ori$nTB[i.miss4]*(1000-SBR.full.ori$SBR[i.miss4])/1000

# fill in missing nSB when we have SBR >0 and nLB but no nTB 
i.miss7 <- which(is.na(SBR.full.ori$nSB) & is.na(SBR.full.ori$nTB)& !is.na(SBR.full.ori$nLB) & !is.na(SBR.full.ori$SBR) & SBR.full.ori$SBR>0)
i.miss7.replace <- SBR.full.ori$nLB * (SBR.full.ori$SBR/1000)/(1-(SBR.full.ori$SBR/1000))
SBR.full.ori$nSB[i.miss7] <- i.miss7.replace[i.miss7]
SBR.full.ori$nTB[i.miss7] <- SBR.full.ori$nSB[i.miss7] + SBR.full.ori$nLB[i.miss7]

# also use wpp_LB when missing - but only for survey and admin 
i.miss7b <- which(is.na(SBR.full.ori$nSB) & is.na(SBR.full.ori$nTB)& is.na(SBR.full.ori$nLB) & !is.na(SBR.full.ori$SBR) & SBR.full.ori$SBR>0 & !is.na(SBR.full.ori$WPP_LB)) & SBR.full.ori$source %in% c("survey","admin")
i.miss7b.replace <- SBR.full.ori$WPP_LB * (SBR.full.ori$SBR/1000)/(1-(SBR.full.ori$SBR/1000))
SBR.full.ori$nSB[i.miss7b] <- i.miss7b.replace[i.miss7b]


# fill in missing SBR -- for now (includes some unknown def)
# not sure if we also need to change adj_sbr_unknown?
i.miss5 <- which(is.na(SBR.full.ori$SBR) & 
                   !is.na(SBR.full.ori$nSB) & !is.na(SBR.full.ori$nTB))
SBR.full.ori$SBR[i.miss5] <- SBR.full.ori$nSB[i.miss5]/SBR.full.ori$nTB[i.miss5]*1000

i.miss6 <- which(is.na(SBR.full.ori$SBR) & 
                   !is.na(SBR.full.ori$nSB) & !is.na(SBR.full.ori$nLB))
SBR.full.ori$SBR[i.miss6] <- SBR.full.ori$nSB[i.miss6]/(SBR.full.ori$nSB[i.miss6]+SBR.full.ori$nLB[i.miss6])*1000

#-------------------------------------#
# if nSB=0  add 0.5, ntb+0.5          #
#-------------------------------------#
i.noSB <- which(SBR.full.ori$nSB==0)
SBR.full.ori$nSB[i.noSB] <- SBR.full.ori$nSB[i.noSB] + 0.5
SBR.full.ori$nTB[i.noSB] <- SBR.full.ori$nTB[i.noSB] + 0.5
SBR.full.ori$SBR[i.noSB] <- 1000*SBR.full.ori$nSB[i.noSB]/SBR.full.ori$nTB[i.noSB] 
SBR.full.ori$adj_sbr_unknown[i.noSB] <- 1000*SBR.full.ori$nSB[i.noSB]/SBR.full.ori$nTB[i.noSB] 
############################################
SBR.full2 <- SBR.full.ori %>% merge(countryRegionList[,!names(countryRegionList) %in% "country"], by = c("iso")) %>% 
                              select(-notes) %>% 
                             mutate(definition_rv = replace(definition_rv, definition_rv == "s40wksANDge28wks", "ge28wks")) %>% 
                             mutate(adj_sbr_unknown = ifelse(is.na(adj_sbr_unknown),SBR,adj_sbr_unknown),
                                    year = floor(year),
                                    SE.sbr = sqrt(1000*adj_sbr_unknown/nTB),
                                    rSN = adj_sbr_unknown/NMR,
                                    rSN_UN = adj_sbr_unknown/UN_NMR,
                                    definition_rv2 = as.character(definition_rv),
                                    definition_raw = definition_rv) %>% 
                             mutate(SE.sbr = ifelse(is.na(SE.sbr),sqrt(1000*adj_sbr_unknown/(nSB+WPP_LB)),SE.sbr)) %>% 
                             mutate(exclusion_notes =  replace(exclusion_notes, 
                                                              is.na(SBR), 
                                                              "missing sbr"))

SE.sbr.max <- sapply(SBR.full2$source,function(x) max(SBR.full2$SE.sbr[SBR.full2$source==x],na.rm = T))

SBR.full <- SBR.full2 %>%    mutate(SE.sbr = ifelse(is.na(SE.sbr),SE.sbr.max,SE.sbr),
                                    SE.logsbr = SE.sbr / adj_sbr_unknown) %>% 
                             mutate(definition_rv2 = replace(definition_rv2, definition_raw == "ge500gANDge28wks", "ge28wks.m"),
                                    definition_rv2 = replace(definition_rv2, definition_raw == "ge500gORge22wks", "ge500g.m"),
                                    definition_rv2 = replace(definition_rv2, definition_raw == "ge1000gORge28wks", "ge1000g.m"),
                                    definition_rv2 = replace(definition_rv2, definition_raw == "ge500gORge28wks", "ge500g.m")) %>% 
                             mutate(definition_rv = replace(definition_rv, definition_raw == "ge500gANDge28wks", "ge28wks"),
                                    definition_rv = replace(definition_rv, definition_raw == "ge500gORge22wks", "ge500g"),
                                    definition_rv = replace(definition_rv, definition_raw == "ge1000gORge28wks", "ge1000g"),
                                    definition_rv = replace(definition_rv, definition_raw == "ge500gORge28wks", "ge500g")) %>% 
                             mutate(exclusion_ratio = NA) %>% 
                             mutate(definition_rv2 = replace(definition_rv2, definition_rv == "ge1000g" & icgroup %in% c("Low income","Lower middle income"), "ge28wks.m")) %>%
                             mutate(definition_rv2 = replace(definition_rv2, definition_rv == "ge500g" & icgroup %in% c("Low income","Lower middle income"), "ge22wks.m"))  %>%
                             mutate(definition_rv = replace(definition_rv, definition_rv == "ge1000g" & icgroup %in% c("Low income","Lower middle income"), "ge28wks")) %>%
                             mutate(definition_rv = replace(definition_rv, definition_rv == "ge500g" & icgroup %in% c("Low income","Lower middle income"), "ge22wks")) %>%
                             #mutate(definition_rv2 = replace(definition_rv2, definition_rv == "ge1000g" & lmic == 1, "ge28wks.m")) %>%
                             #mutate(definition_rv2 = replace(definition_rv2, definition_rv == "ge500g" & lmic == 1, "ge22wks.m"))  %>%
                             #mutate(definition_rv = replace(definition_rv, definition_rv == "ge1000g" & lmic == 1, "ge28wks")) %>%
                             #mutate(definition_rv = replace(definition_rv, definition_rv == "ge500g" & lmic == 1, "ge22wks")) %>%

                             select(uniqueID,iso,country,region,year,source,source_name,context,definition_rv,definition_rv2,definition_raw,
                                    definition,SBR,nSB_adj_unknown,adj_sbr_unknown,nSB_adj_unknown,SE.sbr,SE.logsbr, prop_unknown,nSB,nTB,nLB,WPP_LB,nNM,NMR,
                                    UN_NMR,rSN,rSN_UN,shmdg2,icgroup,lmic,country_idx,exclusion_notes,exclusion_ratio,exclude_col,
                                   exclusion_notes_full)

i.miss5 <- which(is.na(SBR.full$nLB) & !is.na(SBR.full$adj_sbr_unknown * SBR.full$nTB))
SBR.full$nLB[i.miss5] <- (SBR.full$adj_sbr_unknown * SBR.full$nTB/1000)[i.miss5]

#--------------------------------------------------------#
#   full set                                             #  
#--------------------------------------------------------#
saveRDS(SBR.full,"output/sbr.full.rds")


#--------------------------------------#
#   exploratory plot for full data     # 
#--------------------------------------#
full_data_list <- create_list_for_country(SBR.full)
pdf_name <- paste0("fig/exploratory_plot/exploratory_fulldata.pdf")
pdf(pdf_name,width=12)
full_data_list %>% lapply(exploratory_plot)
dev.off()



