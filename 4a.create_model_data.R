#load data
SBR.full.ori <- readRDS("output/sbr2018_com_exclusion.rds")
SBR.full.ori$lmic[SBR.full.ori$iso == "COK"] <- 1

SBR.full <- SBR.full.ori %>% filter(is.na(exclusion_notes)) %>% 
                             filter(is.na(exclusion_ratio)) %>%
  mutate(definition_rv2 = replace(definition_rv2, definition_rv == "ge1000g" & lmic == 1, "ge28wks.m")) %>% 
  mutate(definition_rv2 = replace(definition_rv2, definition_rv == "ge500g" & lmic == 1, "ge22wks.m"))  %>% 
  mutate(definition_rv = replace(definition_rv, definition_rv == "ge1000g" & lmic == 1, "ge28wks")) %>% 
  mutate(definition_rv = replace(definition_rv, definition_rv == "ge500g" & lmic == 1, "ge22wks"))

#---------------------------------------------------------#
# Overview for num paired obs and obs where need to adj   # 
#---------------------------------------------------------#


SBR.model <- SBR.full  %>% filter(!(definition_rv %in% c("any","not defined","unknownGA"))) %>% 
  filter(source != "subnat.admin") %>% 
  select(uniqueID,iso,country,year,source,shmdg2,lmic,definition_rv,definition_rv2,definition_raw,definition,
         adj_sbr_unknown,country_idx,SE.logsbr) %>% 
  mutate(definition_rv2 = as.factor(definition_rv2),
         source = as.factor(source),
         duplicates = NA)
SBR.model$definition_rv2 <- droplevels(SBR.model$definition_rv2)
table(SBR.model$definition_rv2)

#### the following vector is decided by where we can do def adj(priority.for.adj) and where we need adj(SBR.model$definition_rv2)
priority.for.adj_vec <- c("ge28wks","ge28wks.m","ge22wks","ge22wks.m","ge1000g",
                          "ge1000g.m","ge24wks","ge500g")

SBR.model <- SBR.model %>% filter(definition_rv2 %in% priority.for.adj_vec)
SBR.model$definition_rv2 <- droplevels(SBR.model$definition_rv2)

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

SBR.model2 <- SBR.model %>% mutate(SBR = adj_sbr_unknown)
SBR.model.f <- SBR.model2[-record,] 
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
