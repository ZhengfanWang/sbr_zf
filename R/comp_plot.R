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
##################################################################################


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

#########################################################################################

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
