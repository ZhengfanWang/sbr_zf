standata <- readRDS(file = "output/stan.qi1.rds")             ####  data used for fit model

fit <- readRDS(file = "rdsoutput/qi1.rds")                            ### fit result

print(fit, pars = c("beta","bias_dt","sigma_j","tau_delta","gamma_r"))

df <- rstan::extract(fit)
dt_bias <- c(0,round(apply(df$bias_dt,2,mean),digits = 4))             ## get data type bias from fit
dt_variance <- c(0.0025,round((apply(df$var_j,2,mean)),digits = 4))         ## get data type variance from fit
sqrt(dt_variance)
mu_ct <- df$mu_ct +df$delta_ct                                  ### est mean
getr_c <- standata$getr_c                                       

muhat <-c()
for(c in 1:195){
  for(t in 1:19){
    cache_mu_ct <- quantile(mu_ct[,c,t],c(0.025,0.5,0.975))
    cache_covar <- cache_mu_ct 
    cache_exp_mu_rt <- exp(cache_covar)
    muhat <- rbind(muhat,cache_exp_mu_rt)
  }
}
#muhat
colnames(muhat) <- c("low","muhat","up")


estyears <- standata$estyears
year <- rep(estyears,times=standata$numcountry)


countryiso <- countryRegionList$iso                     #### run "iso.R" to get countryRegionList
iso <- rep(countryiso,each=19)
muhat <- data.frame(muhat,year,iso)

class(muhat)
country.list <- list()
for(c in 1:standata$numcountry){
  start.p <- (c-1)*19+1
  country.list[[c]] <- muhat[start.p:(start.p+18),] %>% mutate(country = countryRegionList$country[c])
}

#country.list

fit_result <- country.list[[1]]
for(c in 2:standata$numcountry){
 fit_result <- rbind(fit_result,country.list[[c]])  
}

fit_result <- fit_result %>% select(country,iso,year,low,muhat,up)

#write.csv(fit_result,"output/qi1.csv")

###########################################################################
def <- standata$eta_d
definition_fac <- c("ga28wks","bw1000g","ga22wks","bw500g")
source_fac <- c("admin","HMIS","subnat LR","survey")
sbr2018 <- data.frame(logSBR = standata$Y )
sbr2018$getj_i <- standata$getj_i
sbr2018$getd_i <- standata$getd_i
sbr2018$year <- standata$gett_i + 1999
sbr2018$country_idx <- standata$getc_i
sbr2018$source_name <- source_fac[sbr2018$getj_i]
sbr2018$definition_name <- definition_fac[sbr2018$getd_i]
sbr2018$var <- standata$var_i + standata$phi_d[sbr2018$getd_i] + dt_variance[sbr2018$getj_i]
sbr2018$logadjsbr <- sbr2018$logSBR - def[sbr2018$getd_i] - dt_bias[sbr2018$getj_i]
sbr2018 <- merge(sbr2018,countryRegionList,by=c("country_idx"))
which(sbr2018$var>1)
sbr2018[which(sbr2018$var>10),]
################################################

####################################################

year.f <- c(1,2,3,4)
logSBR.f <- rep(0,4)
logsbradj.f <- rep(1,4)

fake.legend <- data.frame(year=year.f,logSBR=logSBR.f,logadjsbr=logsbradj.f,
                          source_name=source_fac,definition_name=definition_fac) %>% 
  mutate(country = NA,iso = NA, country_idx = NA, var = rep(0.001,4), low = NA, muhat = NA, up = NA) %>% 
  select(country,iso,source_name,definition_name,country_idx,logSBR,logadjsbr, year, var, low, muhat,up)


point.list <- list()
for(c in 1:standata$numcountry){
  point.list[[c]] <- filter(sbr2018,country_idx==c) %>% 
    select(country, iso, source_name,definition_name,
           country_idx,logSBR,logadjsbr,year,var) %>% 
    right_join(country.list[[c]],by = c("year","country","iso")) %>% 
    rbind(fake.legend)
}


#################################################
check <- function(dat.list){
  cis.tq <- dat.list 
  point_dat <- cis.tq %>% dplyr::select(year, logSBR, muhat, up, low , source_name, definition_name,logadjsbr,country,var) %>% 
    drop_na(logSBR)
  ####bias
  se.sbr <- sqrt(point_dat$var)
  # jitter <- runif(dim(point_dat)[1],-0.3,0.3)
  #  point_dat$jitter.year <- point_dat$year+jitter
  point_dat$lowunc <- exp(point_dat$logadjsbr - 1.96*se.sbr)
  point_dat$upunc <- exp(point_dat$logadjsbr + 1.96*se.sbr)
  #### variance
  plot_title <- unique(na.omit(cis.tq[, "country"]))
  est_plot <- ggplot() +
    theme_bw() +
    geom_point(aes(x = year, y = exp(logSBR), colour = definition_name),shape = 1, size =2, data = point_dat) +
    geom_line(aes(x = year, y = muhat), size = 2,data = cis.tq) +
    geom_ribbon(aes(x = year, ymin = low, ymax = up), alpha = 0.3,data = cis.tq) +
    geom_errorbar(aes(x = year, ymin = lowunc, ymax = upunc),width = 0.4 ,col="dark grey",data = point_dat)+
    
    geom_point(aes(x = year, y = exp(logadjsbr), colour = definition_name,shape = source_name), size =3, data = point_dat)+

    
    scale_x_continuous(name = 'Time', breaks = estyears, minor_breaks = NULL, limits = c(2000,2018)) +
    scale_y_continuous(name = 'Stillbirth Rate') +
    labs(title = plot_title) +
    theme(plot.title = element_text(hjust = 0.5, size = 30, face = 'bold'),
          axis.title.x = element_text(size=20),
          axis.title.y = element_text(size=20),
          legend.position = "right",
          legend.title=element_blank())
  
  yupper <- max(point_dat$upunc,na.rm = T)
  ylower <- min(point_dat$lowunc,na.rm = T)
  ave <- mean(cis.tq$muhat,na.rm=T)
 if(yupper-ylower>20){
    est_plot <- est_plot + coord_cartesian(ylim= c(max(0,ave-15),ave+15), xlim = c(1999.5,2018.5),expand = FALSE)
  }
  return(est_plot)
}
pdf_name <- paste0("fig/qi1_v2.pdf")
pdf(pdf_name, width = 10, height = 5)
point.list %>% lapply(check)
dev.off()

