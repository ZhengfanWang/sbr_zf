library(rstan)
library(tidyverse)

fit <- readRDS(file = "rdsoutput/new/base_cesc.rds")
standata <- readRDS(file = "output/stan_data/hs_nval.rds")

array <- rstan::extract(fit)

#####################################################
#          residual and coverage
#####################################################
# create dataset first, named as rv_data
rv_data <- create_rv_data(array,standata) 
# where array is the output of loo model, and the prediction is saved as "prep" and the estimate sd for each obs is saved as "sigma_i" in the model
# where standata is the input of loo model, and the observed SBR is saved as "Y".


####  use rv_data to calculate residual and coverage
rv_data %>% head() 
residuals(data = rv_data,
          y = "y",
          yhat = "50%",
          total_standard_error = "total_sd") 

residuals(data = rv_data,
          y = "y",
          yhat = "50%",
          total_standard_error = "total_sd",
          subset = "source") 

residuals_autoplot(
  data = rv_data,
  y = "y",
  yhat = "50%"
)$lbw_sm

coverage(data = rv_data,
         y = "y",
         lower = "2.5%",
         upper = "97.5%")


###################
# The approach to check the PI and calculate the error
###################
#calculate pit values, input are rv_data and standata
pit.j <- cal_pit(rv_data,standata)

# coverage follows from pit
mean(pit.j < 0.025) # % below 95% PI
mean(pit.j < 0.05) # % below 90% PI
mean(pit.j > 0.975) # % above 95% PI 
mean(pit.j > 0.95) # % above 90% PI 





