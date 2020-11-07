
############################################################################
### get the list country result 
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

###########################################################################################

### get the list used for plot comparison figure
compare.plot.list <- function(set1,set2
                              #,set3,set4
                              ,name
){
  
  plot.list <- list()
  finalplot.list <- list()
  for(c in 1:195){
    set1[[c]] <- set1[[c]] %>% mutate(mod=paste0(name[1]))
    set2[[c]] <- set2[[c]] %>% mutate(mod=paste0(name[2]))
    #set3[[c]] <- set3[[c]] %>% mutate(mod=paste0(name[3]))
    #set4[[c]] <- set4[[c]] %>% mutate(mod=paste0(name[4]))
    
    plot.list[[c]] <- rbind(set1[[c]],set2[[c]]
                            #,set3[[c]],set4[[c]]
    )
    finalplot.list[[c]] <- point.list[[c]] %>% 
      select(country, iso, source_name, definition_name,country_idx,logSBR,logadjsbr,year,var,train) %>% 
      right_join(plot.list[[c]],by = c("year")) %>% 
      select(-country.y) %>% 
      rename(country=country.x) %>% 
      mutate(country = countryRegionList$country[c],
             mod = factor(mod,levels = name))
  }
  
  return(finalplot.list)
}
###################################################################################

### final plot function
compare_plot <- function(dat.list){
  cis.tq <- dat.list %>% filter(year <= 2019)
  point_dat <- dat.list %>% dplyr::select(year, logSBR,logadjsbr, muhat, up, low , source_name, 
                                          definition_name,country,var,train) %>%    drop_na(logSBR)
  
  se.sbr <- sqrt(point_dat$var)
  point_dat$lowunc <- exp(point_dat$logadjsbr - 1.96*se.sbr)
  point_dat$upunc <- exp(point_dat$logadjsbr + 1.96*se.sbr)
  point_dat$adj.sbr <- exp(point_dat$logadjsbr)
  #exclude_point <- point_dat %>% filter(train == 0)
  #include_point <- point_dat %>% filter(train == 1)
  
  plot_title <- unique(na.omit(cis.tq[, "country"]))
  est_plot <- ggplot() +
    theme_bw() +
    geom_line(aes(x = year, y = muhat, col = mod, linetype = mod), size = 1,data = cis.tq) +
    scale_linetype_manual(values = c("solid","dashed")) +
    geom_ribbon(aes(x = year, ymin = low, ymax = up,fill = mod), alpha = 0.1,data = cis.tq) +
    ggnewscale::new_scale_color()+
    geom_point(aes(x = year, y = exp(logSBR), colour = definition_name),shape = 5, size =3, data = point_dat) +
    geom_point(aes(x = year, y = adj.sbr, shape = source_name,col = definition_name), size =5.5, data = point_dat)+
    geom_errorbar(aes(x = year, ymin = lowunc, ymax = upunc),width = 0.4 ,col="dark grey",data = point_dat)+
    
    #geom_point(aes(x = year, y = adj.sbr, shape = source_name,col = definition_name), size =3, data = include_point) +
    #geom_point(aes(x = year, y = adj.sbr), col = "red", size =3 ,data = exclude_point)+
    scale_x_continuous(name = 'Year', breaks = 2000:2019, minor_breaks = NULL) +
    scale_y_continuous(name = 'Stillbirth rate\n(stillbirths per 1,000 total births)') +
    scale_color_manual(values=c("brown","darkgreen","blue","purple","black"),guide = guide_legend(nrow=3,ncol=5)) +
    scale_fill_manual(values=c("red","green"),guide = guide_legend(nrow=3,ncol=5)) +
    scale_shape_manual(values=c(19, 17,15,4))+
    labs(title = plot_title) +
    
    
    
    theme(plot.title = element_text(hjust = 0.5, size = 30, face = 'bold'),
          axis.title.x = element_text(size=12),
          axis.title.y = element_text(size=12),
          legend.position = "bottom",
          legend.title=element_blank(),legend.text = element_text(size=10))
  
  yupper <- max(cis.tq$up,na.rm = T)
  ylower <- min(cis.tq$low,na.rm = T)
  ave <- mean(cis.tq$muhat,na.rm=T)
  
  #if(yupper-ylower<=20){
  est_plot <- est_plot + coord_cartesian(ylim= c(0,yupper+3), xlim = c(1999.5,2019.5),expand = FALSE)
  #}
  # if(yupper-ylower>20){
  # est_plot <- est_plot + coord_cartesian(ylim= c(max(0,ave-15),yupper+5), xlim = c(1999.5,2020.5),expand = FALSE)
  # }
  
  return(est_plot)
}

###################################################################################