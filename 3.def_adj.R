#-------------------------------#
#3. Definition adjust           # Oct22
#-------------------------------#

#load data
SBR.full.ori <- readRDS("output/full.ratio.exc.based.28wks.rds")
SBR.full <- SBR.full.ori 
#-------------------------------#
# find combiations              # 
#-------------------------------#

def_adj_list_res <- find_comb_def_adj(SBR.full)
#store each combation as a element of a list
def_adj_list <- def_adj_list_res$dat
#the alter def name
alter.def <- def_adj_list_res$alter.def

saveRDS(def_adj_list,"output/def_adj_data_list.rds")


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

SBR.model <- SBR.full  %>% filter(is.na(exclusion_notes)) %>% 
                           filter(is.na(exclusion_ratio)) %>% 
                           filter(!(definition_rv %in% c("any","not defined","unknownGA"))) %>% 
                           mutate(adj_sbr_unknown = round(adj_sbr_unknown,digits = 4)) %>% 
                           select(iso,country,region,year,source,lmic,definition_rv,adj_sbr_unknown)

SBR.model$definition_rv <- droplevels(SBR.model$definition_rv)
table(SBR.model$definition_rv)


priority.for.adj <- summ %>% filter(definition %in% levels(SBR.model$definition_rv))
priority.for.adj_vec <- priority.for.adj$definition

SBR.wide <- SBR.model %>% pivot_wider(names_from = definition_rv, values_from = adj_sbr_unknown,
                                      values_fn = list(adj_sbr_unknown = median)) %>% 
                                      arrange(iso,year) %>% 
                                      select(c("iso","country","region","year","source","lmic","ge28wks",
                                               paste0(priority.for.adj_vec[priority.for.adj_vec != "ge28wks"])))

### if there is other source type, delete subnat
names(SBR.wide)
data <- SBR.wide

def_need_change <- levels(SBR.model$definition_rv)
n_def <- length(def_need_change)
n <- dim(data)[1]
k <- dim(data)[2]
def_need_adj <- rep(NA,n)
SBR_need_adj <- rep(NA,n)
loc <- which(names(SBR.wide)=="ge28wks")
for(i in 1:n){
def_need_adj[i] <- colnames(data[i,loc:k])[min(which(!is.na(data[i,loc:k])))]
SBR_need_adj[i] <- data[[i,(loc-1+min(which(!is.na(data[i,loc:k]))))]]
}
def_need_adj <- as.factor(def_need_adj)

#-------------------------------------------------#
#                   Need def adj                  # 
#-------------------------------------------------#

SBR.model <- SBR.wide %>% mutate(definition_rv = def_need_adj,
                                    SBR = SBR_need_adj) %>% 
                          select(-c(paste0(priority.for.adj_vec)))


SBR.non.sub.admin <- SBR.model %>% filter(source != "subnat.admin") 
need.adj.subnat.admin <- SBR.model %>% filter(source == "subnat.admin") %>% 
                          anti_join(SBR.non.sub.admin,by = c("iso","country","year","lmic"))


SBR.model <- rbind(SBR.non.sub.admin,need.adj.subnat.admin)
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

write.csv(summ1,"table/def_adj_num_paired_obs.csv")

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










