### used for find combiation of definition used in "3.def_adj.R"
find_comb_def_adj <- function(data){
  def.28wks <- data %>% filter(definition_rv == "ge28wks") %>% 
    rename("SBR28"="SBR","nSB28"="nSB","ori_def28"="definition_raw") %>% 
    mutate(year = round(year)) %>% 
    select(iso,year,region,SBR28,nSB28,source,ori_def28) 
  
  def.other <- data %>% filter(definition_rv != "ge28wks") %>% 
    mutate(year = round(year))
  lev <- levels(def.other$definition_rv)
  n_comb <- length(lev)
  definition_adj_data_list <- list()
  for(i in 1:n_comb){
    cache <- def.other%>%
      filter(definition_rv == levels(def.other$definition_rv)[i]) 
    definition_adj_data_list[[i]] <- merge(cache,def.28wks,by=c("iso","year","source","region"))
  }
  lev <- lev[lapply(definition_adj_data_list,nrow)>0]
  definition_adj_data_list <- definition_adj_data_list[lapply(definition_adj_data_list,nrow)>0]
  return(list(dat = definition_adj_data_list,
              alter.def = lev))
}




#used to find data set from list to do def adj
find_data_from_list <- function(list,def){
  i=1
  while(unique(list[[i]]$definition_rv) != def){
    i=i+1
  }
  return(list[[i]])
}

# create def adj stan list
create_def_reg_stan_data <- function(dat){
  stan_data <- dat %>% mutate(sei = sqrt(SE.logsbr^2 + SE.logsbr28^2),
                              source2 = as.numeric(as.factor(source))) %>% 
                       select(definition_rv,SBR,NMR,source,country_idx,SBR28,sei,nSB) %>% 
                       filter(!is.na(sei),SBR!=0,SBR28!=0)
  
  getr_c <- countryRegionList$lmic
 
  stan_dat <- list(getc_i = stan_data$country_idx, 
                   getr_c = getr_c, 
                   getj_i = stan_data$source2, 
                   y = log(stan_data$SBR),
                   x = log(stan_data$SBR28), 
                   sbr28 = stan_data$SBR28, 
                   nmr = stan_data$NMR,
                   N=dim(stan_data)[1],
                   sei = stan_data$sei)
  
   return(stan_dat)               
}


### used in 3.comp_cutoff_for_non28wks_def.R

def_adj_func <- function(a,b,def,bias){
  for(i in 1:length(int.def)){
    if(b==def[i]) {r = a + bias[i]} 
  }
  return(r)   
}