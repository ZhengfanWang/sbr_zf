
#######################################definition only for admin data ##################
table(SBR.plot$definition)
SBR.admin <- SBR.plot %>% filter(source=="admin")
dim(SBR.admin)


SBR.admin$definition <- droplevels(SBR.admin$definition)
table(SBR.admin$definition)

SBR.admin.28wks <- SBR.admin %>% filter(definition == "ge28wks")

SBR.admin.defin <- list()
for(i in 1:length(levels(SBR.admin$definition))){
  SBR.admin.defin[[i]] <- SBR.admin %>%
                          filter(definition == levels(SBR.admin$definition)[i]) %>% 
                          merge(SBR.admin.28wks,by = c("iso","year","source","adj.year","country","n"))
}

SBR.admin.defin[[6]]
nobs <- SBR.admin.defin %>% sapply(dim)

ncountry <- c()
for(i in 1:length(levels(SBR.admin$definition))){
ncountry[i] <- length(unique(SBR.admin.defin[[i]]$iso))}

colnames(nobs) <- levels(SBR.admin$definition)
sumtab <- rbind(nobs[1,],table(SBR.admin$definition),ncountry)
rownames(sumtab) <- c("overlap","nobs","ncountry")
sumtab

write.csv(sumtab,"tab/definition.admin.csv")

definition_plot <- function(d.dat){

  plot_title <- unique(d.dat$definition.x)
  sum.plot <- ggplot() +
    theme_bw() +
    geom_point(aes(x = SBR.y, y = SBR.x, shape = context,colour = country), size =3, data = d.dat) +
    geom_abline( slope=1, intercept=0) +
    scale_x_continuous(name = 'ge28wks', minor_breaks = NULL) +
    scale_y_continuous(name = unique(d.dat$definition.x)) +
    labs(title = plot_title) +
    theme(plot.title = element_text(hjust = 0.5, size = 30, face = 'bold'),
          axis.title.x = element_text(size=20),
          axis.title.y = element_text(size=20),
          legend.position = "bottom",
          legend.title=element_blank())
  return(sum.plot)
}

pdf_name <- paste0("fig/def.plot.pdf")
pdf(pdf_name,width=12)
SBR.admin.defin %>% lapply(definition_plot)
dev.off()

######################### definition for all data without subnat admin ########################################

SBR.admin <- SBR.plot %>% filter(source !="subnat.admin")
dim(SBR.admin)

SBR.admin$definition <- droplevels(SBR.admin$definition)
table(SBR.admin$definition)

SBR.admin.28wks <- SBR.admin %>% filter(definition == "ge28wks")

SBR.admin.defin <- list()
for(i in 1:length(levels(SBR.admin$definition))){
  SBR.admin.defin[[i]] <- SBR.admin %>%
    filter(definition == levels(SBR.admin$definition)[i]) %>% 
    merge(SBR.admin.28wks,by = c("iso","year","adj.year","country","n"))
}


nobs <- SBR.admin.defin %>% sapply(dim)


ncountry <- c()
for(i in 1:length(levels(SBR.admin$definition))){
  ncountry[i] <- length(unique(SBR.admin.defin[[i]]$iso))}

colnames(nobs) <- levels(SBR.admin$definition)
sumtab2 <- rbind(nobs[1,],table(SBR.admin$definition),ncountry)
rownames(sumtab2) <- c("overlap","nobs","ncountry")
sumtab2

write.csv(sumtab2,"tab/definition.nosubnatadmin.csv")


definition_ratio_plot <- function(d.dat){
  
  plot_title <- unique(d.dat$definition.x)
  sum.plot <- ggplot() +
    theme_bw() +
    geom_point(aes(x = SBR.y, y = SBR.x/SBR.y, shape = context, colour = country), size =3, data = d.dat) +
    geom_hline(yintercept = 1) +
    scale_x_continuous(name = 'ge28wks', minor_breaks = NULL) +
    scale_y_continuous(name = unique(d.dat$definition.x)) +
    labs(title = plot_title) +
    theme(plot.title = element_text(hjust = 0.5, size = 30, face = 'bold'),
          axis.title.x = element_text(size=20),
          axis.title.y = element_text(size=20),
          legend.position = "bottom",
          legend.title=element_blank())
  return(sum.plot)
}


definition_log_plot <- function(d.dat){
  
  plot_title <- unique(d.dat$definition.x)
  sum.plot <- ggplot() +
    theme_bw() +
    geom_point(aes(x = log(SBR.y), y = log(SBR.x), shape = context, colour = country), size =3, data = d.dat) +
    geom_abline( slope=1, intercept=0) +
    scale_x_continuous(name = 'ge28wks', minor_breaks = NULL) +
    scale_y_continuous(name = unique(d.dat$definition.x)) +
    labs(title = plot_title) +
    theme(plot.title = element_text(hjust = 0.5, size = 30, face = 'bold'),
          axis.title.x = element_text(size=20),
          axis.title.y = element_text(size=20),
          legend.position = "bottom",
          legend.title=element_blank())
  return(sum.plot)
}

pdf_name <- paste0("fig/def.nosubnatadmin.plot.pdf")
pdf(pdf_name,width=12)
SBR.admin.defin %>% lapply(definition_plot)
dev.off()



pdf_name <- paste0("fig/def.LOG.nosubnatadmin.plot.pdf")
pdf(pdf_name,width=12)
SBR.admin.defin %>% lapply(definition_log_plot)
dev.off()


pdf_name <- paste0("fig/def.ratio.nosubnatadmin.plot.pdf")
pdf(pdf_name,width=12)
SBR.admin.defin %>% lapply(definition_ratio_plot)
dev.off()









