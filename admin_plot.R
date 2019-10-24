summary(admin)

admin.plot.dat <- admin %>% filter(year >=2000) 

admin.plot.28 <- admin.plot.dat %>% filter(definition2=="ge28wks")


admin.defin <- list()
for(i in 1:length(levels(admin.plot.dat$definition2))){
  admin.defin[[i]] <- admin.plot.dat %>%
    filter(definition2 == levels(admin.plot.dat$definition2)[i]) %>% 
    select(iso,year,definition2,SBR,context) %>% 
    merge(admin.plot.28,by = c("iso","year"))
}

admin.defin %>% sapply(dim)




definition_plot <- function(d.dat){
  
  plot_title <- unique(d.dat$definition2.x)
  sum.plot <- ggplot() +
    theme_bw() +
    geom_point(aes(x = SBR.y, y = log(SBR.x/SBR.y), shape = context.x,colour = Country), size =3, data = d.dat) +
    geom_hline(yintercept = 0)  +
    scale_x_continuous(name = 'SBR.28wks', minor_breaks = NULL) +
    scale_y_continuous(name = 'log(Def/def28)') +
    labs(title = plot_title) +
    theme(plot.title = element_text(hjust = 0.5, size = 30, face = 'bold'),
          axis.title.x = element_text(size=20),
          axis.title.y = element_text(size=20),
          legend.position = "bottom",
          legend.title=element_blank())
  return(sum.plot)
}

pdf_name <- paste0("fig/admin_dif.pdf")
pdf(pdf_name,width=12)
definition_plot(admin.defin[[3]])
definition_plot(admin.defin[[6]])
definition_plot(admin.defin[[7]])
definition_plot(admin.defin[[8]])
definition_plot(admin.defin[[9]])
definition_plot(admin.defin[[10]])
definition_plot(admin.defin[[11]])
definition_plot(admin.defin[[12]])
definition_plot(admin.defin[[13]])
definition_plot(admin.defin[[18]])
definition_plot(admin.defin[[20]])
dev.off()


definition_nmr_plot <- function(d.dat){
  
  plot_title <- unique(d.dat$definition2.x)
  sum.plot <- ggplot() +
    theme_bw() +
    geom_point(aes(x = NMR, y = log(SBR.x/SBR.y), shape = context.x,colour = Country), size =3, data = d.dat) +
    geom_hline(yintercept = 0)  +
    scale_x_continuous(name = 'NMR', minor_breaks = NULL) +
    scale_y_continuous(name = 'log(Def/def28)') +
    labs(title = plot_title) +
    theme(plot.title = element_text(hjust = 0.5, size = 30, face = 'bold'),
          axis.title.x = element_text(size=20),
          axis.title.y = element_text(size=20),
          legend.position = "bottom",
          legend.title=element_blank())
  return(sum.plot)
}

pdf_name <- paste0("fig/admin_dif_NMR.pdf")
pdf(pdf_name,width=12)
definition_nmr_plot(admin.defin[[3]])
definition_nmr_plot(admin.defin[[6]])
definition_nmr_plot(admin.defin[[7]])
definition_nmr_plot(admin.defin[[8]])
definition_nmr_plot(admin.defin[[9]])
definition_nmr_plot(admin.defin[[10]])
definition_nmr_plot(admin.defin[[11]])
definition_nmr_plot(admin.defin[[12]])
definition_nmr_plot(admin.defin[[13]])
definition_nmr_plot(admin.defin[[18]])
definition_nmr_plot(admin.defin[[20]])
dev.off()



#pdf_name <- paste0("fig/admin_unknown_28wks.pdf")
#pdf(pdf_name,width=12)
#definition_plot(cache)
#dev.off()



####################################
admin.defin[[2]]
table(admin.plot.dat$definition2)
admin.plot.dat$iso
