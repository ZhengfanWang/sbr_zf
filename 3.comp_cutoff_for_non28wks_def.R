

### definition type
definition_fac <- c("ge28wks","ge1000g","ge22wks","ge500g")

###################bias and variance
definition_bias <- c(0,-0.07,0.38,0.27)
definition_var <-  c(0,0.09,0.15,0.12)

defadj_result_input <- c

definition <- c("ge22wks","ge22wks","ge24wks","ge24wks","ge1000g","ge20wks","ge500g","ge1000gANDge28wks")
income <- c("hic","lmic","hic","lmic","hic","lmic","hic","lmic")
bias <- c(0.3890,0.2157,NA,0.2660,-0.07,NA,0.27,NA)
sd <- c(0.17240,0.08412,NA,0.11328,0.3,NA,0.3464,NA)

def_adj_res <- data.frame(definition=definition,
                          income=income,
                          bias=bias,
                          sd=sd)
def_adj_res

sbr2018 <- readRDS("output/data_for_model.rds")

sbr <- sbr2018$SBR
se_logsbr <- sbr2018$SE.logsbr
def <- sbr2018$definition_rv
source <- sbr2018$source
income <- sbr2018$lmic
defadj <- c(0.3890,0.2157,NA,0.2660,-0.07,NA,0.27,NA)

get_adj_obs <- function(output = "def", 
                        sbr_i, 
                        se_logsbr_i,    # stoch/sampling sd
                        def_i,
                        source_i,
                        income_i,
                        def_adj,
                        sou_adj = NULL){

  getd.i
  if(output == "def"){
     log(sbr_i) - def_adj[]
}

                          
}

levels(droplevels(sbr2018$definition_rv))
