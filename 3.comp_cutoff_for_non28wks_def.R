
##few code to do def adj



#------------------------------------------#
# need updates input from def adj          #
#------------------------------------------#
definition_rv <- c("ge22wks","ge22wks","ge24wks","ge24wks","ge1000g","ge20wks","ge500g","ge1000gANDge28wks")
lmic <- c(0,1,0,1,0,1,0,1)
def_bias <- c(0.3890,0.2157,NA,0.2660,-0.07,NA,0.27,NA)
def_sd <- c(0.17240,0.08412,NA,0.11328,0.3,NA,0.3464,NA)

def_adj_res <- data.frame(definition_rv=definition_rv,
                          lmic=lmic,
                          def_bias=def_bias,
                          def_sd=def_sd)
def_adj_res
sbr2018 <- right_join(def_adj_res,sbr2018,by = c("definition_rv","lmic"))

definition_fac <- c("ge28wks",paste0(unique(def_adj_res$definition_rv[!is.na(def_adj_res$def_bias)])))
sbr2018_cleaned <- sbr2018 %>% filter(definition_rv %in% definition_fac) %>% 
                               mutate(def_bias = ifelse(is.na(def_bias),0,def_bias)) %>% 
                               mutate(def_sd = ifelse(is.na(def_sd),0,def_sd))

adj_sbr = exp(log(sbr2018_cleaned$SBR) - sbr2018_cleaned$def_bias)
sd_i = sqrt(sbr2018_cleaned$SE.logsbr^2 + sbr2018_cleaned$def_sd^2) 