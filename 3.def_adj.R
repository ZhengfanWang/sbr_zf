#-------------------------------#
#3. Definition adjust           # Oct22
#-------------------------------#

#load data
SBR.full.ori <- readRDS("output/fullset.rds")
SBR.full <- SBR.full.ori %>% filter(is.na(exclusion_notes)|
                                    exclusion_notes %in% c("duplicates, use def 1000g",
                                                           "duplicates, use VR data",
                                                           "duplicates, use data w/o unknow obs",
                                                           "duplicates, use ge1000gORge28wks def")) %>% 
                             filter(is.na(exclusion_ratio)) %>% 
                             mutate(definition_rv2 = replace(definition_rv2, definition_rv == "ge1000g" & lmic == 1, "ge28wks.m")) %>% 
                             mutate(definition_rv2 = replace(definition_rv2, definition_rv == "ge500g" & lmic == 1, "ge22wks.m"))  %>% 
                             mutate(definition_rv = replace(definition_rv, definition_rv == "ge1000g" & lmic == 1, "ge28wks")) %>% 
                             mutate(definition_rv = replace(definition_rv, definition_rv == "ge500g" & lmic == 1, "ge22wks"))
SBR.full.lmic <- SBR.full %>% filter(lmic == 1)
SBR.full.hic <- SBR.full %>% filter(lmic == 0)

#-----------------------------------------------#
# find combiations for Lmic country             # 
#-----------------------------------------------#

def_adj_lmic_res <- find_comb_def_adj(SBR.full.lmic)
#store each combation as a element of a list
def_adj_list_lmic <- def_adj_lmic_res$dat
def_adj_name_lmic <- def_adj_lmic_res$alter.def
#the alter def name
saveRDS(def_adj_list_lmic,"output/def_adj_data_lmic.rds")

#-----------------------------------------------#
# find combiations for HIC country              # 
#-----------------------------------------------#

def_adj_hic_res <- find_comb_def_adj(SBR.full.hic)
#store each combation as a element of a list
def_adj_list_hic <- def_adj_hic_res$dat
def_adj_name_hic <- def_adj_hic_res$alter.def
#the alter def name
saveRDS(def_adj_list_hic,"output/def_adj_data_hic.rds")

#---------------------------------------------------------#
# Overview for num paired obs and obs where need to adj   # 
#---------------------------------------------------------#

def_adj_res <- find_comb_def_adj(SBR.full)
#store each combation as a element of a list
def_adj_list <- def_adj_res$dat
alter.def <- def_adj_res$alter.def
nobs <- def_adj_list %>% sapply(dim)
num_paired_obs <- as.vector(nobs[1,]) 
N <- length(alter.def)
nobs.lmic <- rep(NA,N)
nobs.hic <- rep(NA,N)
for(i in 1:N){
  nobs.lmic[i] <- sum(def_adj_list[[i]]$lmic)
  nobs.hic[[i]] <- num_paired_obs[i] - nobs.lmic[i]
}

summ <- data.frame(definition = alter.def, num_paired_obs = num_paired_obs , 
                   nobs_paired_lmic = nobs.lmic, nobs_paired_hic = nobs.hic) %>% 
  arrange(desc(num_paired_obs))

#-------------------------------------------------#
#    selecting only 1 def per country-year        # 
#-------------------------------------------------#
SBR.full <- SBR.full %>% filter(is.na(exclusion_notes))
SBR.model <- SBR.full  %>% filter(!(definition_rv %in% c("any","not defined","unknownGA"))) %>% 
                           filter(source != "subnat.admin") %>% 
                           mutate(adj_sbr_unknown = round(adj_sbr_unknown,digits = 5)) %>%
                           select(iso,country,year,source,shmdg2,lmic,definition_rv,definition_rv2,
                                 adj_sbr_unknown,country_idx,SE.logsbr) %>% 
                           mutate(definition_rv2 = as.factor(definition_rv2),
                                  source = as.factor(source),
                                  duplicates = NA)
SBR.model$definition_rv2 <- droplevels(SBR.model$definition_rv2)

priority.for.adj <- summ %>% filter(definition %in% levels(SBR.model$definition_rv))

priority.for.adj_vec <- c("ge28wks","ge28wks.m","ge22wks","ge22wks.m","ge1000g","ge1000g.m","ge500g","ge20wks","ge24wks","ge37wks")

def <- levels(droplevels(SBR.model$definition_rv2))
source <- levels(droplevels(SBR.model$source))
for(c in 1:195){
  for(t in 2000:2018){
    for(d in def){
      for(s in source){
      i<- which(SBR.model$country_idx == c&
                SBR.model$year == t&
                SBR.model$definition_rv2 == d&
                SBR.model$source == s)
      len <- length(i)
      if(len > 1) (SBR.model[i,"duplicates"] <- 1:len)    
      }
    }
  }
}

#-----------------------------
### FOR check
#table(SBR.model$duplicates)
#c = 31
#t = 2012
#d = "ge22wks.m"
#----------------------------

priority_adj_vec <- paste0("adj_sbr_unknown_",priority.for.adj_vec)
priority_se_vec <-  paste0("SE.logsbr_",priority.for.adj_vec)

SBR.wide <-  SBR.model %>% filter(definition_rv2 %in% priority.for.adj_vec) %>% 
                           pivot_wider(names_from = definition_rv2, values_from = c(adj_sbr_unknown,SE.logsbr)) %>% 
                           arrange(iso,year) %>% 
                           select(iso,country,year,source,shmdg2,lmic,country_idx,duplicates,definition_rv,
                                  priority_adj_vec,priority_se_vec)

######
names(SBR.wide)
data <- SBR.wide
def_need_change <- levels(SBR.model$definition_rv)
n_def <- length(def_need_change)
n <- nrow(data)
sbr_loc <-  which(str_sub(names(data),start = 1, end = 3) == "adj")
def_need_adj <- rep(NA,n)
SBR_need_adj <- rep(NA,n)
se.logsbr <- rep(NA,n)

for(i in 1:n){
  loc <- min(which(!is.na(data[i,sbr_loc])))
  def_name <- colnames(data[,sbr_loc])[loc]
  def_need_adj[i] <- str_sub(def_name,start = 17)
  SBR_need_adj[i] <- data[i,def_name]
  se.name <- paste0("SE.logsbr_",str_sub(def_name,start = 17))
  se.logsbr[i] <- data[i,se.name]
}
def_need_adj <- as.factor(def_need_adj)
SBR_need_adj <- unlist(SBR_need_adj)
se.logsbr   <- unlist(se.logsbr)
#-------------------------------------------------#
#                   Need def adj                  # 
#-------------------------------------------------#

SBR.model2 <- SBR.wide %>% mutate(definition_rv2 = as.character(def_need_adj),
                                    SBR = SBR_need_adj,
                                    SE.logsbr = se.logsbr) %>% 
                          select(iso,country,year,source,shmdg2,lmic,definition_rv2,
                                 country_idx,duplicates,SBR,SE.logsbr) %>% 
                          mutate(definition_rv = ifelse(str_sub(definition_rv2,start = -1)=="m",
                                                        str_sub(definition_rv2,end = -3),
                                                        definition_rv2)) %>% 
                          mutate(definition_rv = as.factor(definition_rv),
                                 definition_rv2 = droplevels(as.factor(definition_rv2)))%>% 
                          filter(source != "subnat.admin") 

def <- levels(SBR.model2$definition_rv2)
SBR.model2$definition_rv2 <- factor(SBR.model2$definition_rv2,
                                    levels = c("ge28wks","ge28wks.m","ge22wks","ge22wks.m","ge1000g","ge1000g.m","ge500g",
                                               "ge24wks","ge20wks","ge37wks"))
record <- 0
for(c in 1:195){
  for(t in 2000:2018){
      i<- which(SBR.model2$country_idx == c&
                  SBR.model2$year == t)
      len <- length(i)
      dev <- as.numeric(unlist(SBR.model2[i,"definition_rv2"]))
      if(len > 1)  {
        exclude <- i[-which(dev == min(dev))]
        record <- c(record,exclude)
        }    

  }
}
SBR.model <- SBR.model2[-record,] 

saveRDS(SBR.model,"output/data_for_model.rds")

SBR.need.adj <- SBR.model %>% filter(definition_rv != "ge28wks" )
#-------------------------------------------------#
#                  summary table                  # 
#-------------------------------------------------#
def <- droplevels(SBR.need.adj$definition_rv)
N1 <- length(levels(def))
k <- table(def)
definition <- names(k)
veck <- as.vector(k)

nobs.lmic1 <- rep(NA,N1)
nobs.hic1 <- rep(NA,N1)
for(i in 1:N1){
  nobs.lmic1[i] <- sum(SBR.need.adj$lmic[SBR.need.adj$definition_rv == levels(def)[i]])
  nobs.hic1[i] <- veck[i] - nobs.lmic1[i]
}
nobs.hic1
nobs.lmic1
tab <- data.frame(definition = definition, num_obs = veck, num_lmic = nobs.lmic1,num_hic = nobs.hic1)

summ1 <- summ %>% merge(tab,by = "definition") %>% arrange(desc(num_paired_obs))

write.csv(summ1,"table/def_adj_num_paired_obs_exclustion_strict.csv")

#-------------------------------------------------#
#summary duplicates obs                           #
#-------------------------------------------------#
duplicates_obs <- SBR.model %>% filter(duplicates >=1)
write.csv(duplicates_obs,"output/duplicates.csv")

##############################################################
#  plot
############################################################
model_data_list <- create_list_for_country(SBR.model) 
pdf_name <- paste0("fig/exploratory_plot/model_data.pdf")
pdf(pdf_name,width=12)
model_data_list %>% lapply(exploratory_plot)
dev.off()


need_adj_list <- create_list_for_country(SBR.need.adj)
pdf_name <- paste0("fig/exploratory_plot/need_def_adj.pdf")
pdf(pdf_name,width=12)
need_adj_list %>% lapply(exploratory_plot)
dev.off()










