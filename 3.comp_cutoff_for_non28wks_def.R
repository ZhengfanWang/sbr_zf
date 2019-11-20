
##few code to do def adj



#------------------------------------------#
# need updates input from def adj          #
#------------------------------------------#
definition_rv <- c("ge22wks","ge22wks","ge24wks","ge24wks","ge1000g","ge20wks","ge500g","ge1000gANDge28wks")
lmic <- c(0,1,0,1,0,1,0,1)
def_bias <- c(0.3890,0.2157,NA,0.2660,-0.0710,NA,0.27,NA)
def_sd <- c(0.17240,0.08412,NA,0.11328,0.06896,NA,0.3464,NA)

def_adj_output<- data.frame(definition_rv=definition_rv,
                          lmic=lmic,
                          def_bias=def_bias,
                          def_sd=def_sd)
def_adj_output

## AM: Is the the right file to read in?
## ZW: Sorry, It is a example here.I should have made it clear. I use this to create data for stan model. I think it would be better to read in the output in "2b.apply_SBR_NMR_cutoff_value.R" or 
# the output of "2.merge&clean&plot_full_data.R"
sbr2018 <- readRDS("output/data_for_model.rds")
sbr2018 <- right_join(def_adj_res,sbr2018,by = c("definition_rv","lmic"))

definition_fac <- c("ge28wks",paste0(unique(def_adj_res$definition_rv[!is.na(def_adj_res$def_bias)])))
sbr2018_cleaned <- sbr2018 %>% filter(definition_rv %in% definition_fac) %>% 
                               mutate(def_bias = ifelse(is.na(def_bias),0,def_bias)) %>% 
                               mutate(def_sd = ifelse(is.na(def_sd),0,def_sd))

adj_sbr = exp(log(sbr2018_cleaned$SBR) - sbr2018_cleaned$def_bias)
sd_i = sqrt(sbr2018_cleaned$SE.logsbr^2 + sbr2018_cleaned$def_sd^2) 

##----------------------------------------#
#  Apply SBR:NMR cut-off                  #
#-----------------------------------------#
params_cutoff <- readRDS(file = "output/results_cutoff_analysis.rds")
# params_cutoff is list(mu.hat, delta.hat.sq, sigma.hat.sq)
cutoff_prob <- 0.05

### use observed NMR 
fsbr <- adj_sbr
flb <- sbr2018_cleaned$nLB
fSB <- (fsbr*flb)/(1000 - fsbr)
ftb <- flb + fSB

fnmr_obs <- sbr2018_cleaned$NMR
defadj_prob_obs_i <- get_probs_sbrnmr(ftb = ftb, fsbr = fsbr, 
                               fnmr = fnmr_obs, 
                               flb = flb, 
                               params_cutoff)
defadj_exclude_sbrnmr_obs_i <- (defadj_prob_obs_i < cutoff_prob)



### use UN IGME NMR 
fnmr_un <- sbr2018_cleaned$UN_NMR
flb_un <- sbr2018_cleaned$WPP_LB 
defadj_prob_un_i <- get_probs_sbrnmr(ftb = ftb, 
                              fsbr = fsbr, 
                              fnmr = fnmr_un,
                              flb = flb_un, 
                              params_cutoff)
defadj_exclude_sbrnmr_un_i <- (defadj_prob_un_i < cutoff_prob)

## min probability
defadj_prob_min_i <- map2_dbl(defadj_prob_un_i,defadj_prob_obs_i,min,na.rm=T)
defadj_exclude_sbrnmr_max_i <- (defadj_prob_min_i < cutoff_prob)

###ZW: do we need another column "defadj_exclude_sbrnmr..." or just use "exclude_sbrnmr"? Not sure. And we did not consider the uncertainty of def adj here.
defadj_sbr2018_cleaned <- sbr2018_cleaned %>% mutate(defadj_sbr = adj_sbr) %>%
                                              mutate(defadj_sbr_se = sd_i) %>%
                                              mutate(defadj_exclude_sbrnmr = ifelse(is.na(NMR),defadj_exclude_sbrnmr_un_i,defadj_exclude_sbrnmr_obs_i)) %>%
                                              mutate(defadj_exclude_sbrnmr_max = defadj_exclude_sbrnmr_max_i) %>% 
                                              mutate(defadj_exclude_sbrnmr_obs = defadj_exclude_sbrnmr_obs_i) %>% 
                                              mutate(defadj_exclude_sbrnmr_un = defadj_exclude_sbrnmr_un_i) %>% 
                                              mutate(defadj_prob_min = defadj_prob_min_i) %>% 
                                              mutate(defadj_prob_obs = defadj_prob_obs_i) %>%
                                              mutate(defadj_prob_un = defadj_prob_un_i)

#in these cases apply the small country exclusion, to overwrite these probabilities as "FALSE" 
defadj_sbr2018_cleaned <- defadj_sbr2018_cleaned %>% mutate(defadj_exclude_sbrnmr_max= replace(defadj_exclude_sbrnmr_max,defadj_exclude_sbrnmr_max==TRUE & WPP_LB <= 30000 & (SBR==0 | NMR==0),FALSE),
                                                            defadj_exclude_sbrnmr_obs = replace(defadj_exclude_sbrnmr_obs,defadj_exclude_sbrnmr_obs==TRUE & WPP_LB <= 30000 & (SBR==0 | NMR==0),FALSE),
                                                            defadj_exclude_sbrnmr_un = replace(defadj_exclude_sbrnmr_un,defadj_exclude_sbrnmr_un==TRUE & WPP_LB <= 30000 & (SBR==0 | NMR==0),FALSE),
                                                            defadj_exclude_sbrnmr = replace(defadj_exclude_sbrnmr,defadj_exclude_sbrnmr==TRUE & WPP_LB <= 30000 & (SBR==0 | NMR==0),FALSE))


### AM: I'm not sure if to add it here, but there will need to be a filtering step (and any other manipulation) before
###     putting into Stan model. Added for now so we could see what to filter by
## ZW: The output should be full set with complete "exclude_sbrnmr". Then I use the complete set to select observations for model.

dataformodel <- defadj_sbr2018_cleaned %>% filter(defadj_exclude_sbrnmr==FALSE | is.na(defadj_exclude_sbrnmr)) 

