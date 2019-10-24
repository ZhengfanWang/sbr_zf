#-------------------------------#
#3. Definition adjust           # Oct22
#-------------------------------#

#load data
SBR.full <- readRDS("output/sbr.full.rds")

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
alter.def
summ <- data.frame(definition = alter.def, num_paired_obs = num_paired_obs) %>% 
          arrange(desc(num_paired_obs))

write.csv(summ,"output/def_adj_num_paired_obs.csv")


#-------------------------------------------------#
#    selecting only 1 def per country-year        # 
#-------------------------------------------------#

SBR.model <- readRDS("output/modeldata_newSBR:NMR_cutoff.rds")
SBR.model <- SBR.model %>% filter(is.na(exclusion_notes)) %>% 
                           filter(is.na(exclusion_ratio)) %>% 
                           filter(!(definition_rv %in% c("any","not defined","unknownGA"))) %>% 
                           mutate(adj_sbr_unknown = round(adj_sbr_unknown,digits = 4)) %>% 
                           select(iso,country,region,year,source,definition_rv,adj_sbr_unknown)

SBR.model$definition_rv <- droplevels(SBR.model$definition_rv)
levels(SBR.model$definition_rv)


priority.for.adj <- summ %>% filter(definition %in% levels(SBR.model$definition_rv))
priority.for.adj_vec <- priority.for.adj$definition

SBR.wide <- SBR.model %>% pivot_wider(names_from = definition_rv, values_from = adj_sbr_unknown,
                                      values_fn = list(adj_sbr_unknown = mean)) %>% 
                                      arrange(iso,year) %>% 
                                      select(c("iso","country","region","year","source","ge28wks",paste0(head(priority.for.adj_vec,-1))))

### if there is other source type, delete subnat
names(SBR.wide)
data <- SBR.wide

def_need_change <- levels(SBR.model$definition_rv)
n_def <- length(def_need_change)
n <- dim(data)[1]
k <- dim(data)[2]
def_need_adj <- rep(NA,n)
SBR_need_adj <- rep(NA,n)
for(i in 1:n){
def_need_adj[i] <- colnames(data[i,6:k])[min(which(!is.na(data[i,6:k])))]
SBR_need_adj[i] <- data[[i,(5+min(which(!is.na(data[i,6:k]))))]]
}
def_need_adj <- as.factor(def_need_adj)

#-------------------------------------------------#
#                   Need def adj                  # 
#-------------------------------------------------#

SBR.need.adj <- SBR.wide %>% mutate(definition_rv = def_need_adj,SBR = SBR_need_adj) %>% 
                         filter(def_need_adj != "ge28wks")

need_adj_list <- create_list_for_country(SBR.need.adj)
pdf_name <- paste0("fig/exploratory_plot/need_def_adj.pdf")
pdf(pdf_name,width=12)
need_adj_list %>% lapply(exploratory_plot)
dev.off()

SBR.wide %>% filter(country == "Bangladesh")












