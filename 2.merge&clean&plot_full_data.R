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

SBR.full <- SBR.full.ori %>% merge(countryRegionList, by = c("iso","country")) %>% 
                             select(-c(context,definition,notes)) %>% 
                             mutate(adj_sbr_unknown = ifelse(is.na(adj_sbr_unknown),SBR,adj_sbr_unknown),
                                    year = round(year)) %>% 
                             mutate(definition_rv = replace(definition_rv, definition_rv == "ge500gANDge28wks", "ge28wks"),
                                    definition_rv = replace(definition_rv, definition_rv == "ge500gORge22wks", "ge22wks"),
                                    definition_rv = replace(definition_rv, definition_rv == "s40wksANDge28wks", "ge28wks"))


#--------------------------------------------------------#
#   SBR:NMR ratio     05 lmic, 0.33 hic                  #  oct 22
#--------------------------------------------------------#
names(SBR.full)
exclusion_ratio <- rep(NA,dim(SBR.full)[1])
i.miss.nmr <- which(is.na(SBR.full$NMR))
exclusion_ratio[i.miss.nmr] <- "missing NMR"
i.ratio.lmic <- which(SBR.full$rSN < 0.5 & SBR.full$lmic ==1)
exclusion_ratio[i.ratio.lmic] <- "lmic ratio <0.5"
i.ratio.hic <- which(SBR.full$rSN < 0.33 & SBR.full$lmic ==0)
exclusion_ratio[i.ratio.hic] <- "hic ratio <0.33"

SBR.full <- SBR.full %>% mutate(exclusion_ratio = exclusion_ratio)




if(FALSE){
#--------------------------------------------------------#
#   SBR:NMR ratio     use 0.5 as upper bound             #  oct 22
#--------------------------------------------------------#
ftb <- SBR.full$nLB + SBR.full$nSB
fsbr <- SBR.full$adj_sbr_unknown
flb <- SBR.full$nLB
fnmr <- SBR.full$NMR
fn <- length(ftb)
S <- 1000
fratio_s <- matrix(NA,ncol=S,nrow=fn)
for(i in 1:fn){
  sb_s <- rbinom(S,ftb[i],fsbr[i]/1000)
  nm_s <- rbinom(S,flb[i],fnmr[i]/1000)
  fratio_s[i,] <- sb_s/ftb[i]*1/(nm_s/flb[i])
}
fper_upper <- apply(fratio_s,1,quantile,na.rm = TRUE,c(0.975))
exclusion_ratio <- rep(NA,fn)
i.exclude <- which(fper_upper < 0.5)
exclusion_ratio[i.exclude] <- "upper bound of SBR:NMR < 0.5"
i.miss <- which(is.na(fper_upper))
exclusion_ratio[i.miss] <- "missing info to calculate CI of ratio"
SBR.full <- SBR.full %>% mutate(exclusion_ratio = exclusion_ratio) 
}

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


#-------------

