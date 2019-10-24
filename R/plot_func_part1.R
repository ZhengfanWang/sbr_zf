
exploratory_plot <- function(p.dat){
  estyears <- seq(2000,2018)
  plot_title <- unique(p.dat$country)
  sum.plot <- ggplot() +
    theme_bw() +
    geom_point(aes(x = year, y = SBR, shape = source, colour = definition_rv), 
               size = 3, data = p.dat) +
    scale_x_continuous(name = 'Year', limits = c(2000,2018),
                       breaks = estyears, minor_breaks = NULL) + 
    scale_y_continuous(name = 'SBR', limits = c(0, 40)) +
    # scale_color_discrete(drop = FALSE) +
    labs(title = plot_title) +
    theme(plot.title = element_text(hjust = 0.5, size = 30, face = 'bold'),
          axis.title.x = element_text(size=20),
          axis.title.y = element_text(size=20),
          legend.position = "bottom",
          legend.title=element_blank())
  return(sum.plot)
}

###  hist plot in "2.study_SBR_NMR_cutoff_value"
histplot <- function(data, breaks = 50, title, xlim, ylim){
  h <- hist(data,plot=FALSE,breaks = breaks)
  h$counts=h$counts/sum(h$counts)
  plot(h,main = title,xlim= xlim, ylim = ylim, ylab = "Relative Frequency",xlab = "SBR:NMR")
}
#######################################################################################

definition_log_ratio_aginist_sbr28_plot <- function(d.dat){
  plot_title <- unique(d.dat$definition_rv)
  sum.plot <- ggplot() +
    theme_bw() +
    geom_point(aes(x = SBR28, y = log(SBR/SBR28), shape = source, colour = country), size =3, data = d.dat) +
    geom_hline(yintercept = 0) +
    scale_x_continuous(name = 'SBR28', minor_breaks = NULL) +
    scale_y_continuous(name = "log(def/def28)") +
    labs(title = plot_title) +
    theme(plot.title = element_text(hjust = 0.5, size = 30, face = 'bold'),
          axis.title.x = element_text(size=20),
          axis.title.y = element_text(size=20),
          legend.position = "bottom",
          legend.title=element_blank())
  return(sum.plot)
}



definition_log_ratio_aginist_nmr_plot <- function(d.dat){
  plot_title <- unique(d.dat$definition2)
  sum.plot <- ggplot() +
    theme_bw() +
    geom_point(aes(x = NMR, y = log(SBR/SBR28), shape = context, colour = country), size =3, data = d.dat) +
    geom_hline(yintercept = 0) +
    scale_x_continuous(name = 'NMR', minor_breaks = NULL) +
    scale_y_continuous(name = "log(def/def28)") +
    labs(title = plot_title) +
    theme(plot.title = element_text(hjust = 0.5, size = 30, face = 'bold'),
          axis.title.x = element_text(size=20),
          axis.title.y = element_text(size=20),
          legend.position = "bottom",
          legend.title=element_blank())
  return(sum.plot)
}

definition_log_ratio_aginist_region_plot <- function(d.dat){
  plot_title <- unique(d.dat$definition_rv)
  sum.plot <- ggplot() +
    theme_bw() +
    geom_point(aes(x = SBR28, y = log(SBR/SBR28), shape = source, 
                   colour = as.factor(lmic)), size =3, data = d.dat) +
    geom_hline(yintercept = 0) +
    scale_x_continuous(name = 'SBR28', minor_breaks = NULL) +
    scale_y_continuous(name = "log(def/def28)") +
    labs(title = plot_title) +
    theme(plot.title = element_text(hjust = 0.5, size = 30, face = 'bold'),
          axis.title.x = element_text(size=20),
          axis.title.y = element_text(size=20),
          legend.position = "bottom",
          legend.title=element_blank())
  return(sum.plot)
}


def_residual_plot <- function(mod,data,var){
  array <- extract(mod)
  res.i <- data$y - colMeans(array$yhat)
  sbr.i <- data$sbr28
  nmr.i <- data$nmr
  source("R/iso.R")
  country= countryRegionList[data$getc_i,]$country
  
  if(var=="sbr"){
    plot <- ggplot()+
      geom_point(aes(x = sbr.i, y = res.i, colour = country), size =2)+
      geom_hline(yintercept = 0)}
  if(var=="nmr"){
    plot <- ggplot()+
      geom_point(aes(x = nmr.i, y = res.i, colour = country), size =2)+
      geom_hline(yintercept = 0)}
  
  return(plot)
}
