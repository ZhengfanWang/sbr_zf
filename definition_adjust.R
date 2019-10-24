
##-------------------------------------------------------##
## create data list to adjust definition to 28wks        ##
##-------------------------------------------------------##
SBR_data$definition2 <- droplevels(SBR_data$definition2)
table(SBR_data$definition2)

def.28wks <- SBR_data %>% filter(definition2 == "ge28wks") %>% 
                          rename("SBR28"="SBR","nSB28"="nSB") %>% 
                          select(iso,adj.year,region,SBR28,nSB28) 

def.other <- SBR_data %>% filter(definition2 != "ge28wks") %>% 
                          merge(covar_MNR_data,by = c("iso","year"))

def.other$definition2 <- droplevels(def.other$definition2)

definition_adj_data_list <- list()
for(i in 1:length(levels(def.other$definition2))){
  cache <- def.other%>%
    filter(definition2 == levels(def.other$definition2)[i]) 
    definition_adj_data_list[[i]] <- merge(cache,def.28wks,by=c("iso","adj.year","region"))
}


##-------------------------------------------------------##
## table to check number of paired obs                   ##
##-------------------------------------------------------##

nobs <- definition_adj_data_list %>% sapply(dim)

ncountry <- c()
for(i in 1:length(levels(def.other$definition2))){
  ncountry[i] <- length(unique(definition_adj_data_list[[i]]$iso))}

colnames(nobs) <- levels(def.other$definition2)
sumtab <- rbind(nobs[1,],table(def.other$definition2),ncountry)
rownames(sumtab) <- c("overlap","nobs","ncountry")


write.csv(sumtab,"tab/definition.csv")

##-------------------------------------------------------##
##                         definition plot               ##
##-------------------------------------------------------##


#pdf_name <- paste0("fig/logratioSBR28.pdf")
#pdf(pdf_name,width=12)
#definition_adj_data_list[sapply(definition_adj_data_list,nrow)>0] %>% lapply(definition_log_ratio_aginist_sbr28_plot)
#dev.off()

#pdf_name <- paste0("fig/logratioNMR.pdf")
#pdf(pdf_name,width=12)
#definition_adj_data_list[sapply(definition_adj_data_list,nrow)>0] %>% lapply(definition_log_ratio_aginist_nmr_plot)
#dev.off()

#pdf_name <- paste0("fig/logratioREGION.pdf")
#pdf(pdf_name,width=12)
#definition_adj_data_list[sapply(definition_adj_data_list,nrow)>0] %>% lapply(definition_log_ratio_aginist_region_plot)
#dev.off()


