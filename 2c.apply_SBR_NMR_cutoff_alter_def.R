

##few code to do def adj

### Read in def adjutments
#------------------------------------------#
# need updates input from def adj          #

def_adj_output <- readRDS("output/def_adj_res.rds")
#------------------------------------------#

sbr2018 <- readRDS("output/fullset.rds")


#sbr2018 <- right_join(def_adj_output,sbr2018,by = c("definition_rv","lmic"))
#apply definitional adjustment to UPMIC 
sbr2018$lmic[sbr2018$icgroup %in% c("Upper middle income")] <- 0
#sbr2018$lmic[sbr2018$icgroup %in% c("Upper middle income")] <- 1
sbr2018 <- right_join(def_adj_output,sbr2018,by = c("definition_rv","lmic"))



## AM: Since we are starting with the whole dataset, maybe we should not filter yet, so we can see 
##     exactly what gets excluded at the end? But I'll make that "def_bias" is NA for those with definitions not in
##     and update the exclusion notes, so they get filtered at the end. Does this make sense?

definition_fac <- c("ge28wks",paste0(unique(def_adj_output$definition_rv[!is.na(def_adj_output$def_bias)])))
#sbr2018_cleaned <- sbr2018 %>% filter(definition_rv %in% definition_fac) %>% 
#                               mutate(def_bias = ifelse(is.na(def_bias),0,def_bias)) %>% 
#                               mutate(def_sd = ifelse(is.na(def_sd),0,def_sd))

set_definition_rv <- c("ge22wks","ge24wks","ge1000g","ge20wks","ge500g")
sbr2018 <- sbr2018 %>% mutate(def_bias = ifelse(is.na(def_bias) & definition_rv %in% set_definition_rv,0,def_bias)) %>% 
                               mutate(def_sd = ifelse(is.na(def_sd)  & definition_rv %in% set_definition_rv,0,def_sd))

sbr2018$exclusion_notes <- ifelse(!(sbr2018$definition_rv %in% definition_fac),"not used def",sbr2018$exclusion_notes)
  

adj_sbr = exp(log(sbr2018$SBR) - sbr2018$def_bias)
sd_i = sqrt(sbr2018$SE.logsbr^2 + sbr2018$def_sd^2) 

##----------------------------------------#
#  Apply SBR:NMR cut-off                  #
#-----------------------------------------#
params_cutoff <- readRDS(file = "output/results_cutoff_analysis.rds")
# params_cutoff is list(mu.hat, delta.hat.sq, sigma.hat.sq)
#cutoff_prob <- 0.05
cutoff_prob <- 0.5 #sensitivity analysis

### use observed NMR 
fsbr <- adj_sbr
flb <- sbr2018$nLB
fSB <- (fsbr*flb)/(1000 - fsbr)
ftb <- flb + fSB

fnmr_obs <- sbr2018$NMR
defadj_prob_obs_i <- get_probs_sbrnmr(ftb = ftb, fsbr = fsbr, 
                               fnmr = fnmr_obs, 
                               flb = flb, 
                               params_cutoff)
defadj_exclude_sbrnmr_obs_i <- (defadj_prob_obs_i < cutoff_prob)



### use UN IGME NMR 
fnmr_un <- sbr2018$UN_NMR
flb_un <- sbr2018$WPP_LB 
defadj_prob_un_i <- get_probs_sbrnmr(ftb = ftb, 
                              fsbr = fsbr, 
                              fnmr = fnmr_un,
                              flb = flb_un, 
                              params_cutoff)
defadj_exclude_sbrnmr_un_i <- (defadj_prob_un_i < cutoff_prob)


### AM: Need to come back to this -- something minimum doesn't seem to work for all cases  
## min probability
#defadj_prob_min_i <- map2_dbl(defadj_prob_un_i,defadj_prob_obs_i,min,na.rm=T)
#defadj_exclude_sbrnmr_max_i <- (defadj_prob_min_i < cutoff_prob)

###ZW: do we need another column "defadj_exclude_sbrnmr..." or just use "exclude_sbrnmr"? Not sure. And we did not consider the uncertainty of def adj here.
###AM: Leontine said to ignore uncertainity for now. Let's keep all columns here and then at end I'll create on exclusion_ratio column and drop these


defadj_sbr2018 <- sbr2018 %>% mutate(defadj_sbr = adj_sbr) %>%
                              mutate(defadj_sbr_se = sd_i) %>%
                              mutate(defadj_exclude_sbrnmr = ifelse(is.na(NMR) | source %in% c("HMIS","subnat.LR"),defadj_exclude_sbrnmr_un_i,defadj_exclude_sbrnmr_obs_i)) %>%
                                     #mutate(defadj_exclude_sbrnmr_max = defadj_exclude_sbrnmr_max_i) %>% 
                                     mutate(defadj_exclude_sbrnmr_obs = defadj_exclude_sbrnmr_obs_i) %>% 
                                     mutate(defadj_exclude_sbrnmr_un = defadj_exclude_sbrnmr_un_i) %>% 
                                     #mutate(defadj_prob_min = defadj_prob_min_i) %>% 
                                     mutate(defadj_prob_obs = defadj_prob_obs_i) %>%
                                     mutate(defadj_prob_un = defadj_prob_un_i)

#in these cases apply the small country exclusion, to overwrite these probabilities as "FALSE" 
defadj_sbr2018 <- defadj_sbr2018 %>% mutate(#defadj_exclude_sbrnmr_max= replace(defadj_exclude_sbrnmr_max,defadj_exclude_sbrnmr_max==TRUE & WPP_LB <= 30000 & (nSB < 1 | NMR==0),FALSE),
                                            defadj_exclude_sbrnmr_obs = replace(defadj_exclude_sbrnmr_obs,defadj_exclude_sbrnmr_obs==TRUE & WPP_LB <= 30000 & (nSB < 1 | NMR==0),FALSE),
                                            defadj_exclude_sbrnmr_un = replace(defadj_exclude_sbrnmr_un,defadj_exclude_sbrnmr_un==TRUE & WPP_LB <= 30000 & (nSB < 1 | NMR==0),FALSE),
                                            defadj_exclude_sbrnmr = replace(defadj_exclude_sbrnmr,defadj_exclude_sbrnmr==TRUE & WPP_LB <= 30000 & (nSB < 1 | NMR==0),FALSE))



#### Creating one exclusion_ratio column for 
defadj_sbr2018$exclusion_ratio <- NA
defadj_sbr2018$exclusion_ratio <- ifelse(defadj_sbr2018$exclude_sbrnmr==TRUE & !is.na(defadj_sbr2018$exclude_sbrnmr)& 
                                           defadj_sbr2018$definition_rv=="ge28wks",
                                         "prob < 0.05 & 28 wks def",defadj_sbr2018$exclusion_ratio)

defadj_sbr2018$exclusion_ratio <- ifelse(defadj_sbr2018$defadj_exclude_sbrnmr==TRUE & !is.na(defadj_sbr2018$defadj_exclude_sbrnmr)& 
                                        defadj_sbr2018$definition_rv!="ge28wks",
                                           "prob < 0.05 & non-28 wks def",
                                           defadj_sbr2018$exclusion_ratio)


## AM: Saved at the end of script 4.creat_model_data.R
#write.csv(defadj_sbr2018,"output/fullset.csv")
saveRDS(defadj_sbr2018,"output/fullset.rds")


