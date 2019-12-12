#load data
SBR.full.ori <- readRDS("output/fullset.rds")
SBR.full.ori$lmic[SBR.full.ori$iso == "COK"] <- 1

SBR.full <- SBR.full.ori %>% mutate(data_for_model = ifelse(is.na(exclusion_notes) & 
                                           is.na(exclusion_ratio),1,0)) %>%
  mutate(definition_rv2 = replace(definition_rv2, definition_rv == "ge1000g" & lmic == 1, "ge28wks.m")) %>% 
  mutate(definition_rv2 = replace(definition_rv2, definition_rv == "ge500g" & lmic == 1, "ge22wks.m"))  %>% 
  mutate(definition_rv = replace(definition_rv, definition_rv == "ge1000g" & lmic == 1, "ge28wks")) %>% 
  mutate(definition_rv = replace(definition_rv, definition_rv == "ge500g" & lmic == 1, "ge22wks"))

#---------------------------------------------------------#
# Overview for num paired obs and obs where need to adj   # 
#---------------------------------------------------------#


SBR.full <- SBR.full  %>% 
  mutate(data_for_model = replace(data_for_model,
                                  (definition_rv %in% c("any","not defined","unknownGA")),0)) %>% 
  mutate(data_for_model = replace(data_for_model,source == "subnat.admin",0)) %>% 
  select(definition_rv,lmic,def_bias,def_sd,uniqueID,iso,country,region,year,source,context,definition_rv2,definition_raw,
         definition,SBR,nSB_adj_unknown,adj_sbr_unknown,SE.sbr,SE.logsbr, prop_unknown,nSB,nTB,nLB,WPP_LB,nNM,NMR,
         UN_NMR,rSN,rSN_UN,shmdg2,icgroup,country_idx,exclusion_notes,exclusion_ratio,exclude_col,
         exclusion_notes_full,
         defadj_sbr, defadj_sbr_se,
         data_for_model) %>%
  ## moved this below 
  #select(uniqueID,iso,country,year,source,shmdg2,lmic,definition_rv,definition_rv2,definition_raw,definition,
  #       adj_sbr_unknown,country_idx,SE.logsbr) %>% 
  mutate(definition_rv2 = as.factor(definition_rv2),
         source = as.factor(source),
         duplicates = NA)
  SBR.full$definition_rv2 <- droplevels(SBR.full$definition_rv2)
table(SBR.full$definition_rv2)

## clean up full exclusion notes column a little
SBR.full$exclusion_notes_full <- gsub(pattern = "NA;",replacement = "",SBR.full$exclusion_notes_full)
SBR.full$exclusion_notes_full <- gsub(pattern = ";;",replacement = ";",SBR.full$exclusion_notes_full)

#### the following vector is decided by where we can do def adj(priority.for.adj) and where we need adj(SBR.model$definition_rv2)
priority.for.adj_vec <- c("ge28wks","ge28wks.m","ge22wks","ge22wks.m","ge1000g",
                          "ge1000g.m","ge24wks","ge500g")

SBR.full <- SBR.full %>% mutate(data_for_model=replace(data_for_model,
                                                       !definition_rv2 %in% priority.for.adj_vec,0))

SBR.noModel <- SBR.full  %>% filter(data_for_model==0)
SBR.model <- SBR.full %>% filter(data_for_model==1)
SBR.model$definition_rv2 <- droplevels(SBR.model$definition_rv2)
table(SBR.model$definition_rv2)
#---------------------------------------------------------------------#
###important: reorder factor level as priority 
SBR.model$definition_rv2 <- factor(SBR.model$definition_rv2,levels = priority.for.adj_vec)
#---------------------------------------------------------------------#

def <- levels(droplevels(SBR.model$definition_rv2))
source <- levels(droplevels(SBR.model$source))
record <- 0
for(c in 1:195){
  for(t in 2000:2018){
    for(s in source){
      i<- which(SBR.model$country_idx == c&
                  SBR.model$year == t&
                  SBR.model$source == s)
      len <- length(i)
      dev <- as.numeric(SBR.model[i,"definition_rv2"])
      if(len > 1)  {
        exclude <- i[-which(dev == min(dev))]
        SBR.model$data_for_model[exclude] <- 0
        record <- c(record,exclude)
      }    
      for(d in def){
        i2 <- which(SBR.model$country_idx == c&
                      SBR.model$year == t&
                      SBR.model$definition_rv2 == d&
                      SBR.model$source == s)
        len2 <- length(i2)
        if(len2 > 1) (SBR.model[i2,"duplicates"] <- 1:len2)    
      }
    }
  }
}
SBR.full.f <- rbind(SBR.model,SBR.noModel)
SBR.full.f <- SBR.full.f %>% mutate(SBR_raw = SBR,
                     nSB_raw = nSB,
                     SBR = adj_sbr_unknown,
                     nSB = nSB_adj_unknown) %>%
              arrange(country,year,source,definition_rv)

  
write.csv(SBR.full.f,"output/fullset.csv")
saveRDS(SBR.full.f,"output/fullset.rds")

SBR.model.f<- SBR.model %>% filter(data_for_model==1) %>%
                            select(uniqueID,iso,country,year,source,shmdg2,lmic,
                                  definition_rv,definition_rv2,definition_raw,definition,
                                  adj_sbr_unknown,country_idx,SE.logsbr) %>% 
                            mutate(SBR = adj_sbr_unknown)
## apply new exclusion rule, there is one observation with def "ge500gORge20wks" in Brazil 2010 from subnat lit Rv database. 
# In condition that we already have time-series HMIS data in that country-year. I think we can exclude the obs with def "ge500gORge20wks".

saveRDS(SBR.model.f,"output/data_for_model.rds")
write.csv(SBR.model.f,"output/data_for_model.csv")
SBR.need.adj <- SBR.model.f %>% filter(definition_rv != "ge28wks" )


#-------------------------------------------------#
#summary duplicates obs                           #
#-------------------------------------------------#
duplicates_obs <- SBR.model.f %>% filter(duplicates >=1) %>% filter(source == "admin")
if(nrow(duplicates_obs)>0){
  write.csv(duplicates_obs,"table/duplicates.csv")
}

##############################################################
#  plot
############################################################
model_data_list <- create_list_for_country(SBR.model.f) 
pdf_name <- paste0("fig/exploratory_plot/model_data.pdf")
pdf(pdf_name,width=12)
model_data_list %>% lapply(exploratory_plot)
dev.off()
