#create data list for country. used in exploratory plot
create_list_for_country <- function(data){
  iso.vec <- levels(droplevels(data$iso))
  dat.list <- list()
  for(i in 1:length(iso.vec)){
    dat.list[[i]] <- data[which(data$iso==iso.vec[i]), ]
  }
  return(dat.list)
}

