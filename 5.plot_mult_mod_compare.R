

fit <- readRDS(file = "rdsoutput/1121_qi1.rds")
fit2 <- readRDS(file = "rdsoutput/qi1.hs.rds")



################################################

get_chain_result <- function(fit,smooth = TRUE, estyears = seq(2000,2020)){
  df <- rstan::extract(fit)
  yearLength <- length(estyears)
  muhat <-c()
  if(smooth == TRUE){
  mu_ct <- df$mu_ct + df$delta_ct
    for(c in 1:195){
    for(t in 1:yearLength){
      
      muhat <- rbind(muhat,exp(quantile(mu_ct[,c,t],c(.025,.5,.975))))
      
    }
  }}else{
    mu_ct <- df$mu_ct
    gamma_c <- df$gamma_c
    for(c in 1:195){
      for(t in 1:yearLength){
        
        muhat <- rbind(muhat,exp(quantile(mu_ct[,c,t]+gamma_c[,c],c(.025,.5,.975))))
        
      }
    }
  }


  
  colnames(muhat) <- c("low","muhat","up")
  
  year <- rep(estyears,times=195)
  muhat <- as.data.frame(cbind(muhat,year))
  
  class(muhat)
  country.list <- list()
  for(c in 1:195){
    start.p <- (c-1)*yearLength+1
    country.list[[c]] <- muhat[start.p:(start.p+yearLength-1),] %>% mutate(country = countryRegionList$country[c])
  }

  return(country.list)
}


Pspline1_country_list <- get_chain_result(fit,smooth = FALSE)
Pspline2_country_list <- get_chain_result(fit2,smooth = FALSE)


####################################################################

standata <- readRDS(file = "output/stan.qi1.hs.rds")

definition_fac <- c("ga28wks","ga22wks","ga24wks","bw1000g","bw500g")
source_fac <- c("admin","HMIS","subnat LR","survey")
sbr2018 <- data.frame(logSBR = standata$unadj_Y )
sbr2018$getj_i <- standata$getj_i
sbr2018$getd_i <- standata$getd_i
sbr2018$year <- standata$gett_i + 1999
sbr2018$country_idx <- standata$getc_i
sbr2018$source_name <- source_fac[sbr2018$getj_i]
sbr2018$definition_name <- definition_fac[sbr2018$getd_i]
sbr2018$var <- var_i
sbr2018$logadjsbr <- standata$Y  - bias_dt_i
sbr2018 <- merge(sbr2018,countryRegionList,by=c("country_idx"))
train <- rep(0,standata$N)
train[standata$getitrain_k] <- 1
sbr2018$train <- train 

 
###################  add this because we want to see the country name if no data point for some country, AND make legend consistent.
year.f <- c(1,2,3,4,5)
logSBR.f <- rep(0,5)
logsbradj.f <- rep(1,5)

fake.legend <- data.frame(year=year.f,logSBR=logSBR.f,logadjsbr=logsbradj.f,source_name=c(source_fac,"admin"),definition_name=definition_fac) %>% 
  mutate(country = NA,iso = NA, country_idx = NA, var = rep(0.001,5), low = NA, muhat = NA, up = NA,train = NA) %>% 
  select(country,iso,source_name,definition_name,country_idx,logSBR,logadjsbr, year, var , train)
####################################################################

point.list <- list()

for(c in 1:standata$numcountry){
  point.list[[c]] <- filter(sbr2018,country_idx==c) %>% 
    select(country, iso, source_name,definition_name,
           country_idx,logSBR,logadjsbr,year,var,train) %>% 
   # right_join(Pspline1_country_list[[c]],by = c("year","country","iso")) %>% 
    rbind(fake.legend)
  }
dat.list <-point.list[[1]]

###################################################################



compare.plot.list <- function(set1,set2,name){

  plot.list <- list()
  finalplot.list <- list()
  for(c in 1:195){
    set1[[c]] <- set1[[c]] %>% mutate(mod=paste0(name[1]))
    set2[[c]] <- set2[[c]] %>% mutate(mod=paste0(name[2]))
    plot.list[[c]] <- rbind(set1[[c]],set2[[c]]) 
    
      finalplot.list[[c]] <- point.list[[c]] %>% 
        select(country, iso, source_name, definition_name,country_idx,logSBR,logadjsbr,year,var,train) %>% 
        right_join(plot.list[[c]],by = c("year")) %>% 
        select(-country.y) %>% 
        rename(country=country.x) %>% 
        mutate(country = countryRegionList$country[c])
  }
  
  return(finalplot.list)
}
#example
normal_keep_ex <- compare.plot.list(Pspline1_country_list,Pspline2_country_list, c("covbase","covhs"))
normal_keep_ex[[1]]
#########################################################


##########################################################################################
compare_plot <- function(dat.list){
  cis.tq <- dat.list
  point_dat <- cis.tq %>% dplyr::select(year, logSBR,logadjsbr, muhat, up, low , source_name, 
                                        definition_name,country,var,train) %>% 
    drop_na(logSBR)
  se.sbr <- point_dat$SE^2
  point_dat$adj.sbr <- exp(point_dat$logadjsbr)
  exclude_point <- point_dat %>% filter(train == 0)
  include_point <- point_dat %>% filter(train == 1)
   
  plot_title <- unique(na.omit(cis.tq[, "country"]))
  est_plot <- ggplot() +
    theme_bw() +
    geom_line(aes(x = year, y = muhat, colour = mod), size = 2,data = cis.tq) +
    geom_ribbon(aes(x = year, ymin = low, ymax = up,colour = mod), alpha = 0.3,data = cis.tq) +
    #geom_point(aes(x = year, y = adj.sbr, shape = source_name), size =3, data = point_dat)+
    geom_point(aes(x = year, y = adj.sbr, shape = source_name), size =3, data = include_point) +
    geom_point(aes(x = year, y = adj.sbr), col = "red", size =3 ,data = exclude_point)+
    scale_x_continuous(name = 'Time', breaks = estyears, minor_breaks = NULL) +
    scale_y_continuous(name = 'Stillbirth Rate') +
    labs(title = plot_title) +
    theme(plot.title = element_text(hjust = 0.5, size = 30, face = 'bold'),
          axis.title.x = element_text(size=20),
          axis.title.y = element_text(size=20),
          legend.position = "bottom",
          legend.title=element_blank())
  return(est_plot)
}

normal_keep_ex[[1]]

pdf_name <- paste0("fig/HS_fit&cov.pdf")
pdf(pdf_name, width = 8, height = 5)
normal_keep_ex %>% lapply(compare_plot)
dev.off()


