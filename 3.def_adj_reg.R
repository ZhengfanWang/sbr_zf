

SBR.full <- readRDS("output/sbr.full.rds")

##not sure whether we need to exclude data when def adj. Exclude here.
SBR_data <- SBR.full %>% filter(is.na(exclusion_notes)) %>% 
                         filter(is.na(exclusion_ratio)) 

##-------------------------------------------------------##
## create data list to adjust definition to 28wks        ##
##-------------------------------------------------------##
SBR_data$definition_rv <- droplevels(SBR_data$definition_rv)
table(SBR_data$definition_rv)


countryList <- countryRegionList %>%   
                select(iso,country_idx) %>% 
                arrange(country_idx)

SBR_data <- SBR_data %>% merge(countryList,by = "iso") 

def.28wks <- SBR_data %>% filter(definition_rv == "ge28wks") %>% 
  rename("SBR28"="SBR","nSB28"="nSB","NMR28"="NMR","SE.logsbr28"="SE.logsbr","SE.sbr28"="SE.sbr") %>% 
  select(iso,year,region,SBR28,nSB28,NMR28,SE.logsbr28,SE.sbr28) 

def.other <- SBR_data %>% filter(definition_rv != "ge28wks") 

def.other$definition_rv <- droplevels(def.other$definition_rv)

definition_adj_data_list <- list()
for(i in 1:length(levels(def.other$definition_rv))){
  cache <- def.other%>%
    filter(definition_rv == levels(def.other$definition_rv)[i]) 
  definition_adj_data_list[[i]] <- merge(cache,def.28wks,by=c("iso","year","region"))
}

definition_adj_data_reg <- definition_adj_data_list[sapply(definition_adj_data_list,nrow)>0]

###########################################################
##                  1.improtant definition               ##
###########################################################

##-------------------------------------------------------##
##                   ge1000g                             ##
##-------------------------------------------------------##

def.1000g <- find_data_from_list(definition_adj_data_reg,"ge1000g")
names(def.1000g)
definition_log_ratio_aginist_region_plot(def.1000g)

definition_log_ratio_aginist_sbr28_plot(def.1000g)

def.1000g$source <- as.factor(def.1000g$source)

def1000g <- create_def_reg_stan_data(def.1000g)

fit1000g <- stan(file='mod/def_adj_reg_region_bias.stan',chains = 4,
            control=list(adapt_delta=0.99, max_treedepth=15),
            data=def1000g)
saveRDS(fit1000g,file="output/fit1000g.rds")
print(fit1000g,pars = c("alpha[1]","alpha[2]","sigma[1]","sigma[2]"))

def_residual_plot(fit1000g,def1000g,"sbr")

def_residual_plot(fit1000g,def1000g,"nmr")


#fit1000g_country_int <- stan(file='mod/def_adj_reg_random_country.stan',chains = 4,
#                        control=list(adapt_delta=0.99, max_treedepth=15),
#                        data=def1000g)
#saveRDS(fit1000g_country_int,file="output/fit1000g_country_int.rds")
#print(fit1000g_country_int,pars = c("alpha_r[1]","alpha_r[2]","alpha_r[3]","sigma_j[1]","sigma_j[2]"))
##-------------------------------------------------------##
##                   ge20wks                             ##
##-------------------------------------------------------##

def.20wks <- find_data_from_list(definition_adj_data_reg,"ge20wks")

definition_log_ratio_aginist_sbr28_plot(def.20wks)
definition_log_ratio_aginist_region_plot(def.20wks)

def20wks <- create_def_reg_stan_data(def.20wks)
#def.20wks[def20wks$y==-Inf,]
fit20wks <- stan(file='mod/def_adj_reg.stan',chains = 4,
            control=list(adapt_delta=0.99, max_treedepth=15),
            data=def20wks)
saveRDS(fit20wks,file="output/fit20wks.rds")
print(fit20wks,pars = c("alpha_r[1]","alpha_r[2]","sigma_j[1]","sigma_j[2]"))


def_residual_plot(fit20wks,def20wks,"sbr")

def_residual_plot(fit20wks,def20wks,"nmr")

fit20wks_country_int <- stan(file='mod/def_adj_reg_random_country.stan',chains = 4,
                             control=list(adapt_delta=0.99, max_treedepth=15),
                             data=def20wks)
saveRDS(fit20wks_country_int,file="output/fit20wks_country_int.rds")


##-------------------------------------------------------##
##                   ge22wks                             ##
##-------------------------------------------------------##
def.22wks <- definition_adj_data_reg[[8]] %>% filter(log(SBR)-log(SBR28) < 1.2)
def.22wks.notes <- definition_adj_data_reg[[8]] %>% filter(log(SBR)-log(SBR28) >1)

definition_log_ratio_aginist_sbr28_plot(def.22wks)
definition_log_ratio_aginist_region_plot(def.22wks)


def22wks <- create_def_reg_stan_data(def.22wks)

fit22wks <- stan(file='mod/def_adj_reg.stan',chains = 4,
                 control=list(adapt_delta=0.99, max_treedepth=15),
                 data=def22wks)
saveRDS(fit22wks,file="output/fit22wks.rds")

print(fit22wks,pars = c("alpha_r[1]","alpha_r[2]","sigma_j[1]","sigma_j[2]"))

def_residual_plot(fit22wks,def22wks,"sbr")

def_residual_plot(fit22wks,def22wks,"nmr")

fit22wks_country_int <- stan(file='mod/def_adj_reg_random_country.stan',chains = 4,
                             control=list(adapt_delta=0.99, max_treedepth=15),
                             data=def22wks)
saveRDS(fit22wks_country_int,file="output/fit22wks_country_int.rds")
print(fit22wks_country_int,pars = c("alpha_r[1]","alpha_r[2]","sigma_j[1]","sigma_j[2]"))

##-------------------------------------------------------##
##                   ge500gORge22wks                     ##
##-------------------------------------------------------##

definition_log_ratio_aginist_sbr28_plot(definition_adj_data_reg[[9]])
definition_log_ratio_aginist_region_plot(definition_adj_data_reg[[9]])
def.500gOR22wks <- definition_adj_data_reg[[9]]
def500gOR22wks <- create_def_reg_stan_data(def.500gOR22wks)

fit500gOR22wks <- stan(file='mod/def_adj_reg.stan',chains = 4,
                 control=list(adapt_delta=0.99, max_treedepth=15),
                 data=def500gOR22wks)
print(fit500gOR22wks)

##-------------------------------------------------------##
##                   ge500g                              ##
##-------------------------------------------------------##

def.500g <- definition_adj_data_reg[[13]]%>% filter(log(SBR)-log(SBR28) !=0, country != "Andorra",country != "Australia")
definition_log_ratio_aginist_region_plot(def.500g)
definition_log_ratio_aginist_sbr28_plot(def.500g)

def500g <- create_def_reg_stan_data(def.500g)

fit500g <- stan(file='mod/def_adj_reg.stan',chains = 4,
                 control=list(adapt_delta=0.99, max_treedepth=15),
                 data=def500g)
saveRDS(fit500g,file="output/fit500g.rds")
print(fit500g,pars = c("alpha_r[1]","alpha_r[2]","alpha_r[3]","sigma_j[1]","sigma_j[2]"))


def_residual_plot(fit500g,def500g,"sbr")

def_residual_plot(fit500g,def500g,"nmr")

fit500g_country_int <- stan(file='mod/def_adj_reg_random_country.stan',chains = 4,
                             control=list(adapt_delta=0.99, max_treedepth=15),
                             data=def500g)
saveRDS(fit500g_country_int,file="output/fit500g_country_int.rds")
print(fit500g_country_int,pars = c("alpha_r[1]","alpha_r[2]","alpha_r[3]","sigma_j[1]","sigma_j[2]"))


###########################################################
##                  2.less improtant definition          ##
###########################################################

##-------------------------------------------------------##
##                   any_gest_age                        ##
##-------------------------------------------------------##

definition_log_ratio_aginist_region_plot(definition_adj_data_reg[[1]])
def.anygestage <- definition_adj_data_reg[[1]]
defanygestage <- create_def_reg_stan_data(def.anygestage)

fitanygestage <- stan(file='mod/def_adj_reg.stan',chains = 4,
                control=list(adapt_delta=0.99, max_treedepth=15),
                data=defanygestage)
print(fitanygestage)

##-------------------------------------------------------##
##                   any_weight                          ##
##-------------------------------------------------------##

definition_log_ratio_aginist_region_plot(definition_adj_data_reg[[2]])

##-------------------------------------------------------##
##                   not_defined                         ##
##-------------------------------------------------------##

definition_log_ratio_aginist_region_plot(definition_adj_data_reg[[3]])



##-------------------------------------------------------##
##                   ge1000gANDge28wks                   ##
##-------------------------------------------------------##

definition_log_ratio_aginist_region_plot(definition_adj_data_reg[[5]])
definition_log_ratio_aginist_sbr28_plot(definition_adj_data_reg[[5]])
def.ge1000gANDge28wks  <- definition_adj_data_reg[[5]]
defge1000gANDge28wks  <- create_def_reg_stan_data(def.ge1000gANDge28wks )

fitge1000gANDge28wks  <- stan(file='mod/def_adj_reg.stan',chains = 4,
                      control=list(adapt_delta=0.99, max_treedepth=15),
                      data=defge1000gANDge28wks )
print(fitge1000gANDge28wks)

##-------------------------------------------------------##
##                   ge1000gORge28wks                    ##
##-------------------------------------------------------##

definition_log_ratio_aginist_region_plot(definition_adj_data_reg[[6]])
definition_log_ratio_aginist_sbr28_plot(definition_adj_data_reg[[6]])
def.ge1000gORge28wks  <- definition_adj_data_reg[[6]]
defge1000gORge28wks  <- create_def_reg_stan_data(def.ge1000gORge28wks)

fitge1000gORge28wks  <- stan(file='mod/def_adj_reg.stan',chains = 4,
                              control=list(adapt_delta=0.99, max_treedepth=15),
                              data=defge1000gORge28wks)
print(fitge1000gORge28wks)



##-------------------------------------------------------##
##                   ge24wks                             ##
##-------------------------------------------------------##

definition_log_ratio_aginist_region_plot(definition_adj_data_reg[[10]])
definition_log_ratio_aginist_sbr28_plot(definition_adj_data_reg[[10]])

def.ge24wks  <- definition_adj_data_reg[[10]]
defge24wks  <- create_def_reg_stan_data(def.ge24wks)

fitge24wks  <- stan(file='mod/def_adj_reg.stan',chains = 4,
                             control=list(adapt_delta=0.99, max_treedepth=15),
                             data=defge24wks)
print(fitge24wks)


##-------------------------------------------------------##
##                   ge500gANDge28wks                    ##
##-------------------------------------------------------##

definition_log_ratio_aginist_region_plot(definition_adj_data_reg[[11]])
definition_log_ratio_aginist_sbr28_plot(definition_adj_data_reg[[11]])

def.ge500gANDge28wks  <- definition_adj_data_reg[[11]]
defge500gANDge28wks  <- create_def_reg_stan_data(def.ge500gANDge28wks)

fitge500gANDge28wks  <- stan(file='mod/def_adj_reg.stan',chains = 4,
                    control=list(adapt_delta=0.99, max_treedepth=15),
                    data=defge500gANDge28wks)
print(fitge500gANDge28wks)

##-------------------------------------------------------##
##                   ge37wks                             ##
##-------------------------------------------------------##

definition_log_ratio_aginist_region_plot(definition_adj_data_reg[[12]])
definition_log_ratio_aginist_sbr28_plot(definition_adj_data_reg[[12]])

def.ge37wks <- definition_adj_data_reg[[12]]
defge37wks  <- create_def_reg_stan_data(def.ge37wks)

fitge37wks <- stan(file='mod/def_adj_reg.stan',chains = 4,
                             control=list(adapt_delta=0.99, max_treedepth=15),
                             data=defge37wks)
print(fitge37wks)


##-------------------------------------------------------##
##                   ge500gORge20wks                     ##
##-------------------------------------------------------##

definition_log_ratio_aginist_region_plot(definition_adj_data_reg[[14]])


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