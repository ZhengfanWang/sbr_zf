#--------
# master.R 
# Oct 2019
#--------

# setup:
# main steps are in this master script
# project specific R functions are saved in the R subfolder 
# subscripts (that eventually are to be turned into R functions) are in sub_....R
# original data are in input subfolder



## load funcs
Rfiles <- list.files(file.path(paste0(getwd(),"/R/")), ".R") #gets names of r files
sapply(paste0(paste0(getwd(),"/R/"), Rfiles), source) #gets functions from file

#----------------------------------------------------------------------------------------#
############                 1. read and clean data                      #################
#----------------------------------------------------------------------------------------#

## Read in countryRegionList info including MDG region, income region
source("1.iso.R")

##admin data
source("1.clean_admin.R")

#subnation admin data
source("1.clean_subnat_admin.R")

#subnation lit review data and survey data
source("1.clean_subnat&survey.R")


#----------------------------------------------------------------------------------------#
#                       2. merge data& Country summary plot                              #
#----------------------------------------------------------------------------------------#

#get SBR:NMR upper bound.
#source("2.study_SBR_NMR_cutoff_value.R")

### add exclusion_ratio col
source("2.merge&clean&plot_full_data.R")
# exploratory plot for countries in fig/summaryplot.pdf

#########
#still need to re-org for Horse shoe 
source("2.clean_covar.R")
#########

#----------------------------------------------------------------------------------------#
#############          3. definition adjustment                      #####################
#----------------------------------------------------------------------------------------#

#Find combinations for def adj analysis, 
#and Selecting only 1 def per country-year-source,
#and to summarize for which defs we need an adjustment
source("3.def_adj.R")

##### new approach may applied here
source("3.def_adj_reg.R")

#----------------------------------------------------------------------------------------#
#############          4. Process  model  fit                          ###################
#----------------------------------------------------------------------------------------#
source("creat_stan_data.R")
source("fit_model")

#----------------------------------------------------------------------------------------#
#############          5. model check and others                              ############
#----------------------------------------------------------------------------------------#

#country plot
source("plot_country_est.R")

#comparison plot
source("plot_mult_mod_compare.R")

#loo
source("loo_waic.R")

#world map
source("plot_worldmap.R")

