####
#         create data set exclude other def if def28wks is available
##########################################################################
library(tidyverse)
SBR.full <- readRDS("output/sbr.full.rds")

################ some special exclusion


dim(SBR.full)


### OMAN has ge1000gORge28wks and ge1000g in 8 years(2008-2018), use def 1000g, 
SBR.full <- SBR.full %>% mutate(exclusion_notes=replace(exclusion_notes,
                                country =="Oman" &
                    definition == "ge1000gORge28wks" &
                    is.na(exclusion_notes),
                    "use def ge1000g")) %>% 
                        filter(source != "subnat.admin")

SBR.full %>% filter(country =="Oman" &
                      definition == "ge1000gORge28wks")

######
SBR.full <- SBR.full %>% mutate(exclusion_notes=replace(exclusion_notes,
                                                        country =="Australia" &
                                                          definition == "ge1000gORge28wks" &
                                                          is.na(exclusion_notes),
                                                        "use def ge1000g"))

SBR.full %>% filter(country =="Australia" &
                      definition == "ge28wks" )

cache <-merge(SBR.full,countryRegionList,by = c("iso"))

#table(cache$shmdg2)

ge500g_low <- SBR.full[SBR.full$definition=="ge500g"&SBR.full$shmdg2!=1,]
ge1000g_low <- SBR.full[SBR.full$definition=="ge1000g"&SBR.full$shmdg2!=1,]
dim(merge(ge500g_low,ge1000g_low,by=c("iso","year")))
ge28wks_low <- cache[cache$definition=="ge28wks"&cache$shmdg2==3,]
dim(merge(ge500g_low,ge28wks_low,by=c("iso","year")))

#aaa <- countryRegionList[countryRegionList$country=="Argentina",]
##########################################

SBR.full$int.year <- round(SBR.full$year)

SBR.full.exclude <- SBR.full %>%  filter(is.na(exclusion_notes)) %>% 
                                  filter(is.na(exclusion_ratio))

dim(SBR.full.exclude)
### merge combiation definition
##
table(SBR.full.exclude$definition)
definition2 <-  fct_collapse(SBR.full.exclude$definition,
                                     ge1000g = c("ge1000g","ge1000gANDge28wks","ge1000gORge28wks"),
                                     ge500g =  c("ge500gORge22wks","ge500g"),
                                     ge22wks = c("ge22wks"),
                                     ge20wks = c("ge20wks","ge400gORge20wks","ge500gORge20wks"),
                                     ge24wks = c("ge24wks"),
                                     ge26wks = c("ge1000gORge26wks"),
                                     ge28wks = c("ge28wks","s40wksANDge28wks"))
table(definition2)

SBR.full.exclude$definition <- definition2

### use adj_Sbr_unknown to replace sbr
SBR.cache <- SBR.full.exclude %>% mutate(SBR = ifelse(!is.na(adj_sbr_unknown),adj_sbr_unknown,SBR)) %>% 
             select("iso","int.year","definition","source","SBR","adj_sbr_unknown","country","SE","rSN_UN","exclusion_notes","exclusion_ratio") #6518

########################################################################################
names(SBR.cache)
sbr.28wks <- SBR.cache %>% filter(definition == "ge28wks") %>%  
                           mutate(sourceid = 1)                         ### 1229

only.other <- merge(SBR.cache,sbr.28wks,by = c("iso","int.year","source"),all.x=TRUE)  %>%    # 7360
               filter(is.na(sourceid)) %>%     
               dplyr::rename("definition"="definition.x",
                      "SBR"="SBR.x",
                      "country"="country.x",
                      "SE"="SE.x",
                      "exclusion_notes"="exclusion_notes.x",
                      "exclusion_ratio"="exclusion_ratio.x",
                      "rSN_UN"="rSN_UN.x",
                      "adj_sbr_unknown"="adj_sbr_unknown.x") %>% 
               select("iso","int.year","definition","source","SBR","adj_sbr_unknown","country","SE","rSN_UN","exclusion_notes","exclusion_ratio")

table(only.other$definition)

dim(only.other)
only.other

sbr.28wks <- sbr.28wks %>% select(-sourceid)
names(sbr.28wks)
names(only.other)
###################################################################################        finish 28wks select

sbr.1000g <- only.other %>%  filter(definition == "ge1000g") %>% 
                             mutate(sourceid = 1)
dim(sbr.1000g)

only.500g_22wks <- merge(only.other,sbr.1000g,by = c("iso","int.year","source"),all.x=TRUE) %>% 
                   filter(is.na(sourceid)) %>% 
                   dplyr::rename("definition"="definition.x","SBR"="SBR.x","country"="country.x","SE"="SE.x","exclusion_notes"="exclusion_notes.x",
                          "exclusion_ratio"="exclusion_ratio.x","rSN_UN"="rSN_UN.x","adj_sbr_unknown"="adj_sbr_unknown.x") %>% 
                   select("iso","int.year","definition","source","SBR","adj_sbr_unknown","country","SE","rSN_UN","exclusion_notes","exclusion_ratio")
sbr.1000g <- sbr.1000g %>% select(-sourceid)
####################################################################################       finish 1000g select

sbr.22wks <- only.500g_22wks %>%  filter(definition == "ge22wks") %>% 
                                  mutate(sourceid = 1)
dim(sbr.22wks)


only.500g <- merge(only.500g_22wks,sbr.22wks,by = c("iso","int.year","source"),all.x=TRUE) %>%
             filter(is.na(sourceid)) %>% 
             dplyr::rename("definition"="definition.x","SBR"="SBR.x","country"="country.x","SE"="SE.x","exclusion_notes"="exclusion_notes.x",
                    "exclusion_ratio"="exclusion_ratio.x","rSN_UN"="rSN_UN.x","adj_sbr_unknown"="adj_sbr_unknown.x") %>% 
             select("iso","int.year","definition","source","SBR","adj_sbr_unknown","country","SE","rSN_UN","exclusion_notes","exclusion_ratio")
  
sbr.22wks<- sbr.22wks %>% select(-sourceid)

names(only.500g)
SBR.plot <- rbind(sbr.28wks,sbr.1000g,sbr.22wks,only.500g)

dim(SBR.plot)



#SBR.plot$iso <- factor(SBR.plot$iso, levels=sort(levels(SBR.plot$iso)))
#SBR.plot$iso <- droplevels(SBR.plot$iso)
SBR.plot <- SBR.plot %>% arrange(iso, int.year)
summary(SBR.plot)

definition_fac <- c("ge28wks","ge1000g","ge22wks","ge500g")
SBR.plot <- SBR.plot %>% filter(definition %in% definition_fac) %>% 
                                filter(!is.na(SE))
table(SBR.plot$definition)


dim(SBR.plot)
SBR.plot$definition <- droplevels(SBR.plot$definition)
old.dl <- levels(SBR.plot$definition)
SBR.plot$definition <- factor(SBR.plot$definition,
                              levels=old.dl[c(4,1,3,2)])
levels(SBR.plot$definition)


adj_sbr <- SBR.plot$adj_sbr_unknown
for(i in 1:length(adj_sbr)){
  if(is.na(adj_sbr[i]))
    adj_sbr[i] = SBR.plot$SBR[i]
}

SBR.plot$SBR <- adj_sbr

iso.vec <- levels(SBR.plot$iso)

dat.list <- list()
for(i in 1:length(iso.vec)){
  dat.list[[i]] <- SBR.plot[which(SBR.plot$iso==iso.vec[i]), ]
}



summary_plot_admin <- function(p.dat){
  estyears <- seq(2000,2018)
  plot_title <- unique(p.dat$country)
  sum.plot <- ggplot() +
    theme_bw() +
    geom_point(aes(x = int.year, y = SBR, shape = source, colour = definition), 
               size = 3, data = p.dat) +
    scale_x_continuous(name = 'Year', limits = c(2000,2018),
                       breaks = estyears, minor_breaks = NULL) + 
    scale_y_continuous(name = 'SBR', limits = c(0, 36)) +
    # scale_color_discrete(drop = FALSE) +
    labs(title = plot_title) +
    theme(plot.title = element_text(hjust = 0.5, size = 30, face = 'bold'),
          axis.title.x = element_text(size=20),
          axis.title.y = element_text(size=20),
          legend.position = "bottom",
          legend.title=element_blank())
  return(sum.plot)
}





pdf_name <- paste0("fig/summaryplot_clean.pdf")
pdf(pdf_name,width=12)
dat.list %>% lapply(summary_plot_admin)
dev.off()

model_data <- SBR.plot %>% select(iso,int.year,definition,source,SBR,country,SE) 
dim(model_data)
saveRDS(model_data,"output/dataformodel.rds")
write.csv(model_data,"output/dataformodel.csv")
