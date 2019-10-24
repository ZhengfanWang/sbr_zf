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
                             select(-c(context,definition,notes,country_idx)) %>% 
                             mutate(adj_sbr_unknown = ifelse(is.na(adj_sbr_unknown),SBR,adj_sbr_unknown),
                                    year = round(year))

names(SBR.full2)


#--------------------------------------#
#   exploratory plot for full data     # 
#--------------------------------------#
#full_data_list <- create_list_for_country(SBR.full)
#pdf_name <- paste0("fig/exploratory_plot/exploratory_fulldata_wo_exclusion.pdf")
#pdf(pdf_name,width=12)
#full_data_list %>% lapply(exploratory_plot)
#dev.off()

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
fn # There are 2141 obs after first excluded.

summary(ftb)
summary(flb)
summary(fsbr)
summary(fnmr)

fper_upper <- apply(fratio_s,1,quantile,na.rm = TRUE,c(0.975))

exclusion_ratio <- rep(NA,fn)
i.exclude <- which(fper_upper < 0.5)
exclusion_ratio[i.exclude] <- "upper bound of SBR:NMR < 0.5"
i.miss <- which(is.na(fper_upper))
exclusion_ratio[i.miss] <- "missing info to calculate CI of ratio"


SBR.full2 <- SBR.full %>% mutate(exclusion_ratio = exclusion_ratio) 

#--------------------------------------------------------#
#   data for model                                       #  
#--------------------------------------------------------#

SBR.model <- SBR.full2 %>% filter(is.na(exclusion_ratio),is.na(exclusion_notes))

saveRDS(SBR.full2,"output/sbr.full.rds")
names(SBR.full2)
saveRDS(SBR.model,"output/modeldata_newSBR:NMR_cutoff.rds")

#inclusion_data_list <- create_list_for_country(SBR.model)
#pdf_name <- paste0("fig/exploratory_plot/exploratory_data_w_exlucison.pdf")
#pdf(pdf_name,width=12)
#inclusion_data_list %>% lapply(exploratory_plot)
#dev.off()


#-------------

