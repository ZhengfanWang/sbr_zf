rm(list=ls())
library(rstan)
fit <- readRDS(file = "rdsoutput/new/hs_nval_res20200907.rds")
hs <- rstan::extract(fit)
remove(fit)

old <-readRDS(file = "rdsoutput/new/base_nval_res20200907.rds")
base <- rstan::extract(old)
remove(old)

mu_ct_hs <- hs$mu_ct + hs$delta_ct
mu_ct_base <- base$mu_ct + base$delta_ct

dim(mu_ct_hs)
res_hs <- apply(mu_ct_hs,c(2,3),mean)
res_base <- apply(mu_ct_base,c(2,3),mean)
mu_base <- as.vector(res_base)
mu_hs <- as.vector(res_hs)

diff <- mu_base - mu_hs
ave <- 0.5*(mu_base + mu_hs)


diff_exp <- exp(mu_base) - exp(mu_hs)
ave_exp <- 0.5*(exp(mu_base) + exp(mu_hs))

region <- rep(countryRegionList$sdg_name,21)

ggplot()+
  geom_point(aes(x = exp(mu_base), y = exp(mu_hs), col = region)) + 
  scale_x_continuous(name = 'SBR from subsetted model') +
  scale_y_continuous(name = 'SBR from BHTSRM') +
  #labs(title = 'Estimates comparsion between BHTSRM and subsetted model')+
theme(plot.title = element_text(hjust = 0.5, size = 12, face = 'bold'),
      axis.title.x = element_text(size=12),
      axis.title.y = element_text(size=12),
      legend.position = "right",
      legend.title=element_blank(),legend.text = element_text(size=12))

ggplot()+
  geom_point(aes(x = ave, y = diff, col = region)) + 
  scale_x_continuous(name = 'average of estimated log SBR') +
  scale_y_continuous(name = 'the difference log SBR between subsetted model and BHTSRM') +
  #labs(title = 'Estimates comparsion between BHTSRM and subsetted model')+
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = 'bold'),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        legend.position = "right",
        legend.title=element_blank(),legend.text = element_text(size=12))

ggplot()+
  geom_point(aes(x = ave_exp, y = diff_exp, col = region)) + 
  scale_x_continuous(name = 'average of SBR estimates') +
  scale_y_continuous(name = 'SBR estimate subsetted model - SBR estimate BHTSRM') +
 #labs(title = 'Estimates comparsion between BHTSRM and subsetted model')+
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = 'bold'),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        legend.position = "right",
        legend.title=element_blank(),legend.text = element_text(size=12))


