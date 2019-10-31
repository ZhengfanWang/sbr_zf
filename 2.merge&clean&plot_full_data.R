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

SBR.full <- SBR.full.ori %>% merge(countryRegionList, by = c("iso","country")) %>% 
                             select(-c(context,definition,notes)) %>% 
                             mutate(adj_sbr_unknown = ifelse(is.na(adj_sbr_unknown),SBR,adj_sbr_unknown),
                                    year = round(year),
                                    SE.sbr = sqrt(1000*adj_sbr_unknown/nTB),
                                    SE.logsbr = SE.sbr / adj_sbr_unknown,
                                    rSN = adj_sbr_unknown/NMR,
                                    rSN_UN = adj_sbr_unknown/UN_NMR) %>% 
                             mutate(definition_rv = replace(definition_rv, definition_rv == "ge500gANDge28wks", "ge28wks"),
                                    definition_rv = replace(definition_rv, definition_rv == "ge500gORge22wks", "ge22wks"),
                                    definition_rv = replace(definition_rv, definition_rv == "s40wksANDge28wks", "ge28wks"),
                                    definition_rv = replace(definition_rv, definition_rv == "ge1000gORge28wks", "ge1000g")) %>% 
                             mutate(exclusion_notes = replace(exclusion_notes, is.na(SE.logsbr), 
                                                              "missing info to cal se.logsbr")) %>% 
                             mutate(exclusion_ratio = NA)
#--------------------------------------------------------#
#   full set                                             #  
#--------------------------------------------------------#
saveRDS(SBR.full,"output/sbr.full.rds")


#--------------------------------------#
#   exploratory plot for full data     # 
#--------------------------------------#
full_data_list <- create_list_for_country(SBR.full)
pdf_name <- paste0("fig/exploratory_plot/exploratory_fulldata_wo_exclusion.pdf")
pdf(pdf_name,width=12)
full_data_list %>% lapply(exploratory_plot)
dev.off()



