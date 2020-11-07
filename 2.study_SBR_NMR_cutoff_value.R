###  SBR:NMR cutoff value study
# this is done once only, using high quality HMIC data
# so updted only if those data are updated


# load high quality lmic data
hq_data <- openxlsx::read.xlsx("input/high quality LMIC study data/High Quality LMIC data_SBR_NMR.xlsx")

############################# MC method to explore cut off ######################

#-------------------------------#
#     1-step: explore hq data   #
#-------------------------------#
tb <- hq_data$lb + hq_data$sb
sbr <- hq_data$sbr
lb <- hq_data$lb
nmr <- hq_data$nmr
n <- nrow(hq_data)


sd_i <- getsd_logratio(tb, sbr, nmr, lb) # see function in r folder


stan.data.cutoff <- list(n = n, y_i = log(sbr/nmr), sd_i = sd_i)
library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
fit <- stan(file= "mod/study_ratio_cutoff.stan",data=stan.data.cutoff,chains = 4,
            control=list(adapt_delta=0.99, max_treedepth=15))

print(fit,pars = c("mu","sigma"))
fit.chain <- rstan::extract(fit)
mu <- fit.chain$mu                  # posterior sample of overall mean of log ratio
sigma  <- fit.chain$sigma           # posterior sample of variability across settings.

mu.hat <- mean(mu)
delta.hat.sq <- var(mu)
sigma.hat.sq <- median(sigma)^2

saveRDS(list(mu.hat=mu.hat, delta.hat.sq=delta.hat.sq, sigma.hat.sq=sigma.hat.sq), 
        file = "output/results_cutoff_analysis.rds")


