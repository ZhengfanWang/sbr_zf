
# load the values from the cutoff analysis
params_cutoff <- readRDS(file = "output/results_cutoff_analysis.rds")
# params_cutoff is list(mu.hat, delta.hat.sq, sigma.hat.sq)

cutoff_prob <- 0.05
cutoff_bound_zerovar <- exp(qnorm(cutoff_prob,params_cutoff$mu.hat,
    sqrt(params_cutoff$delta.hat.sq + params_cutoff$sigma.hat.sq))


# for observed nmr
ftb <- full_data$nLB + full_data$nSB
fsbr <- full_data$adj_sbr_unknown
flb <- full_data$nLB
fnmr_obs <- full_data$NMR
prob_obs_i <- get_probs_sbrnmr_obs(ftb = ftp, fsbr = fsbr, 
  fnmr = fnmr_obs, flb = flb, params_cutoff)
exclude_sbrnmr_obs_i <- (prob_obs_i < cutoff_prob)

# now same for UN nmr for selected obs
fnmr_un <- full_data$UN_NMR
flb_un <- full_data$nLB # THIS SHOULD BE UPDATED TO ALL LIVE BIRTHS
prob_un_i <- get_probs_sbrnmr_obs(ftb = ftp, fsbr = fsbr, 
  fnmr = fnmr_un, flb = flb_un, params_cutoff)
exclude_sbrnmr_un_i <- (prob_un_i < cutoff_prob)

# some exploratory analysis 
#hist(prob_i_obs,freq = FALSE, breaks = 20)
#hist(prob_i_un,freq = FALSE, breaks = 20)

#mean(prob_i_obs < cutoff_prob,na.rm = T)
#mean(prob_i_un < cutoff_prob,na.rm = T)

# la did not update below yet:
# process is to add prob_i columns to the data set (to have it for analysis) 
# as well as the exclusion outcome (to filter by)

SBR.full.ratio <- full_data %>% rename(exclude_based_on_obssbrnmr_ratio = exclusion_ratio) %>% 
  mutate(prob_based_on_obssbrnmr = round(prob_i_obs,digit=4),
         v_based_on_obssbrnmr = v_i_obs,
         prob_based_on_sbrunnmr = round(prob_i_un,digit=4),
         v_based_on_sbrunnmr = v_i_un,
         exclude_based_on_obssbrnmr_ratio = replace(exclude_based_on_obssbrnmr_ratio,
                                                    prob_i_obs <= 0.05&definition_rv == "ge28wks",
                                                    "TRUE"),
         exclude_based_on_obssbrnmr_ratio = replace(exclude_based_on_obssbrnmr_ratio,
                                                    prob_i_obs > 0.05&definition_rv == "ge28wks",
                                                    "FALSE")
         #,
         #exclude_based_on_obssbrnmr_ratio = replace(exclude_based_on_obssbrnmr_ratio,
         #                                            is.na(prob_i_obs),
         #                                            "missing obs NMR")
  ) %>% 
  mutate(exclude_based_on_sbrUNnmr_ratio = NA) %>% 
  mutate(exclude_based_on_sbrUNnmr_ratio = replace(exclude_based_on_sbrUNnmr_ratio,
                                                   prob_i_un <= 0.05&definition_rv == "ge28wks",
                                                   "TRUE"),
         exclude_based_on_sbrUNnmr_ratio = replace(exclude_based_on_sbrUNnmr_ratio,
                                                   prob_i_un > 0.05&definition_rv == "ge28wks",
                                                   "FALSE")
         #,
         #exclude_based_on_sbrUNnmr_ratio = replace(exclude_based_on_sbrUNnmr_ratio,
         #                                            is.na(prob_i_un),
         #                                            "missing UN NMR")
  ) %>% 
  mutate(exclusion_ratio = NA) %>% 
  mutate(exclusion_ratio = replace(exclusion_ratio,
                                   is.na(prob_i),
                                   "missing nmr and un nmr")) %>% 
  mutate(exclusion_ratio = replace(exclusion_ratio,
                                   prob_i<0.05 & definition_rv == "ge28wks",
                                   "prob < 0.05 and 28wks def")) 
#table(SBR.full.ratio$exclusion_ratio)
write.csv(SBR.full.ratio,"output/fullset.csv")
saveRDS(SBR.full.ratio,"output/fullset.rds")
################################################################################################

#--------------------------------------#
#   exploratory plot after exclusion   # 
#--------------------------------------#
SBR.clean <- SBR.full.ratio %>% filter(is.na(exclusion_notes),is.na(exclusion_ratio))
clean_data_list <- create_list_for_country(SBR.clean)
pdf_name <- paste0("fig/exploratory_plot/exploratory_clean_data.pdf")
pdf(pdf_name,width=12)
clean_data_list %>% lapply(exploratory_plot)
dev.off()
