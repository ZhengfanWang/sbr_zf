#--------
# master.R 
# Nov 2020
#--------

# setup:
# main steps are in this "0.master.R" script
# project specific R functions are saved in the R subfolder 
# original data are in input subfolder(ZF20201101: Need to updates by UNICEF.)

## load funcs
Rfiles <- list.files(file.path(paste0(getwd(),"/R/")), ".R") #gets names of r files
sapply(paste0(paste0(getwd(),"/R/"), Rfiles), source) #gets functions from file

source("0.load_packages.R")

#----------------------------------------------------------------------------------------#
############                 1. read and clean data                      #################
#----------------------------------------------------------------------------------------#

## Read in "countryRegionList" info including SDG region, income region
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

### add exclusion_ratio col
source("2.merge&clean&plot_full_data.R")

# get SBR:NMR upper bound.
# !NOTE!:this is done once only, using high quality HMIC data in input folder
# so updated only if those data are updated
 source("2.study_SBR_NMR_cutoff_value.R")

# exclusion based on SBR:NMR ratio 
source("2b.apply_SBR_NMR_cutoff_def28.R")

# exclusion non 28wks definition after 3.def_adj.R getting the defadj result. 
source("2c.apply_SBR_NMR_cutoff_alter_def.R")

#----------------------------------------------------------------------------------------#
#############          3. definition adjustment                      #####################
#----------------------------------------------------------------------------------------#

#Find combinations for def adj analysis, 
#and Selecting only 1 def per country-year-source,
#and to summarize for which defs we need an adjustment
source("3.def_adj.R")

# Definition adjustment model is in JAGS format in "mod/defadj_mod/" folder
# Scripts to process the def_adj is in "def_adj_script" folder

#----------------------------------------------------------------------------------------#
#############          4. Process  model  fit                          ###################
#----------------------------------------------------------------------------------------#
#create data file for the final model.
source("4a.create_model_data.R")

#---------------------------------------#
## Create STAN data for final STAN model
hs <- T    # use HS prior or not
do.validation <- F   # do vaildation or not
laocv <- F           # if do vaildation, do leave the last observation or leave 20% observation out validation
save.to <- "output/stan_data/hs_nval.rds"   # save the STAN data name and path
source("4b.creat_stan_data.R")
#---------------------------------------#

# fit HS model if hs = T, base model if hs = F
source("4c.fit_model")

#-----------------------------------------------------------------------------------------#
#############          5. model check and others                              #############
# For following check plot, please go to each script and select the model output to check #
#-----------------------------------------------------------------------------------------#

#residual plot
source("5.check_res_plot.R")

#country plot and comparison plot
source("5.plot_country_est.R")
source("5.plot_mult_result_comparison.R")
source("5.plot_scattor_plot.R") 

#Check waic using loo package
source("5.check_loo_waic.R")

#world map to show the sbr situation
source("5.plot_worldmap.R")

#check the intercept
source("5.check_regional_int_prior.R")

#check the convergence
source("5.check_convergence.R")

#post processing/create
source("5.postprocessing.R")