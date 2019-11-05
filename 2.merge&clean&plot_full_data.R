#  GEt full data set
# Last modified: Oct.22, 2019


#---------------#
#   load data   #
#---------------#
admin <- readRDS("output/admin.full.rds")
subnat.lr <- readRDS("output/subnat.lit.rv.full.rds")
subnat.admin <- readRDS("output/sub.admin.full.rds")
survey <- readRDS("output/survey.full.rds")


#-------------#
#   merge     # 
#-------------#
SBR.full.ori <- rbind(admin, subnat.lr, survey,subnat.admin) # 13789 obs

#################################################
i.miss3 <- which(is.na(SBR.full.ori$nLB)& (!is.na(SBR.full.ori$nTB-SBR.full.ori$nSB)))
SBR.full.ori$nLB[i.miss3] <- (SBR.full.ori$nTB-SBR.full.ori$nSB)[i.miss3]
i.miss4 <- which(is.na(SBR.full.ori$nTB)& (!is.na(SBR.full.ori$nSB+SBR.full.ori$nLB)))
SBR.full.ori$nTB[i.miss4] <- (SBR.full.ori$nSB+SBR.full.ori$nLB)[i.miss4]


SBR.full <- SBR.full.ori %>% merge(countryRegionList, by = c("iso","country")) %>% 
                             select(-c(context,definition,notes)) %>% 
                             mutate(definition_rv = replace(definition_rv, definition_rv == "s40wksANDge28wks", "ge28wks")) %>% 
                             mutate(adj_sbr_unknown = ifelse(is.na(adj_sbr_unknown),SBR,adj_sbr_unknown),
                                    year = round(year),
                                    SE.sbr = sqrt(1000*adj_sbr_unknown/nTB),
                                    SE.logsbr = SE.sbr / adj_sbr_unknown,
                                    rSN = adj_sbr_unknown/NMR,
                                    rSN_UN = adj_sbr_unknown/UN_NMR,
                                    definition_rv2 = as.character(definition_rv)) %>% 
                             mutate(definition_rv2 = replace(definition_rv2, definition_rv == "ge500gANDge28wks", "ge28wks.m"),
                                    definition_rv2 = replace(definition_rv2, definition_rv == "ge500gORge22wks", "ge22wks.m"),
                                    definition_rv2 = replace(definition_rv2, definition_rv == "ge1000gORge28wks", "ge1000g.m")) %>% 
                             mutate(definition_rv = replace(definition_rv, definition_rv == "ge500gANDge28wks", "ge28wks"),
                                    definition_rv = replace(definition_rv, definition_rv == "ge500gORge22wks", "ge22wks"),
                                    definition_rv = replace(definition_rv, definition_rv == "ge1000gORge28wks", "ge1000g")) %>% 
                             mutate(exclusion_notes = replace(exclusion_notes, is.na(SE.logsbr), 
                                                              "missing info to cal se.logsbr")) %>% 
                             mutate(exclusion_ratio = NA)

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



