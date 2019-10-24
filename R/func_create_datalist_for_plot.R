#create data list for country. used in exploratory plot
create_list_for_country <- function(data){
  iso.vec <- levels(droplevels(data$iso))
  dat.list <- list()
  for(i in 1:length(iso.vec)){
    dat.list[[i]] <- data[which(data$iso==iso.vec[i]), ]
  }
  return(dat.list)
}

### used for find combiation of definition used in "3.def_adj.R"
find_comb_def_adj <- function(data){
  def.28wks <- data %>% filter(definition_rv == "ge28wks") %>% 
    rename("SBR28"="SBR","nSB28"="nSB") %>% 
    mutate(year = round(year)) %>% 
    select(iso,year,region,SBR28,nSB28) 
  
  def.other <- data %>% filter(definition_rv != "ge28wks") %>% 
                        mutate(year = round(year))
  lev <- levels(def.other$definition_rv)
  n_comb <- length(lev)
  definition_adj_data_list <- list()
  for(i in 1:n_comb){
    cache <- def.other%>%
      filter(definition_rv == levels(def.other$definition_rv)[i]) 
    definition_adj_data_list[[i]] <- merge(cache,def.28wks,by=c("iso","year","region"))
  }
  return(list(dat = definition_adj_data_list,
              alter.def = lev))
}
