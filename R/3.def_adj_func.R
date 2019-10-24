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

