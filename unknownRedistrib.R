### Puprose of Code: Redistribute unknown data into different definition categories
### Author: Anu Mishra
### Date: 8/5/19

library(readxl)
rm(list=ls())
library(dplyr)
library(RColorBrewer)
library(scales)
library(ggplot2)

datacall <- read_excel("C:/Users/anmishra/Dropbox/UNICEF Stillbirth/Databases/Admin/data.call.all.xlsx",sheet = 1)

## NOTE: redistfun is a function to redistribute the unknown obervations to the reported weight/gestational age categories
##       based on the proportions of the distribution (i.e. maintain similar proportions for weight/age categories)
##       This is a function applied to a single row vector, so needs to be used with apply wrapper 

## Inputs: x = a row vector for fixed country/year that contains (among other things) the number of unknowns reported and the number of SBs reported for different age categories 
## nameDist = the column names of the categories that the unknowns should be distributed across
## nameUnknown = the column name for the vector of unknowns
redistFun <- function(x,namesDist,nameUnknown){
  distCol <- as.numeric(x[namesDist])
  unknownCol <- as.numeric(x[nameUnknown])
  
  prop <- distCol/sum(distCol,na.rm = T)
  add <- floor(prop*unknownCol) #floor so that we are not adding non-interger numbers of still birth
  
  if(sum(add,na.rm = T) < unknownCol){ ## because we used floor above, we may not have distributed all the unknowns 
    addDiff <- unknownCol - sum(add,na.rm = T)
    add[order(prop) <= as.numeric(addDiff)]  <- add[order(prop) <= as.numeric(addDiff)] + 1
  }
  res <- distCol + add
  
  return(res)
}


#### Distributing GA unknowns #####
### GA 1: redistribute those with missing gestional age -- option 1 ####
#subset to this observations with unknowns and GA 1
dataSub_ga1 <- datacall[datacall$frmt_ga==1 & datacall$Unknown_ga1!=0 & !is.na(datacall$Unknown_ga1),]

namesDist <- names(dataSub_ga1[15:35])
nameUnknown <- names(dataSub_ga1[36])

## apply redistribution function
ga_1_updated<- t(apply(dataSub_ga1,1,FUN = redistFun, namesDist = namesDist, nameUnknown = nameUnknown))
ga_1_updated <- as.data.frame(ga_1_updated)
names(ga_1_updated) <- namesDist
ga_1_updated <- ga_1_updated %>% dplyr::rename_all(paste0, "_wUnknown")

unknown_dataSub_ga1 <- cbind(dataSub_ga1,ga_1_updated)
unknown_dataSub_ga1 <- as.data.frame(unknown_dataSub_ga1)


## compare old and new proportions 
prop1_ga1 <- unknown_dataSub_ga1[,namesDist]/rowSums(unknown_dataSub_ga1[,namesDist])
prop2_ga1 <- unknown_dataSub_ga1[,names(ga_1_updated)]/rowSums(unknown_dataSub_ga1[,names(ga_1_updated)])

for(i in 1:ncol(prop1_ga1)){
  plot(prop1_ga1[,i],prop2_ga1[,i],main=names(prop1_ga1)[i])
  abline(a=0,b=1,lty=2)
}

#for those proportions that seem off check counts more closely (all small countries)
ga_1_problem_23 <- unknown_dataSub_ga1[which(abs(prop2_ga1$`ga23+0-23+6weeks_wUnknown`- prop1_ga1$`ga23+0-23+6weeks`) > 0.025 & !is.na(prop2_ga1$`ga23+0-23+6weeks_wUnknown`) & !is.na(prop1_ga1$`ga23+0-23+6weeks`)),
                                  c("iso3","CountryName","year",nameUnknown,namesDist,names(ga_1_updated))]

ga_1_problem_40 <- unknown_dataSub_ga1[which(abs(prop2_ga1$`ga40+0-40+6weeks_wUnknown`- prop1_ga1$`ga40+0-40+6weeks`) > 0.02 & !is.na(prop2_ga1$`ga40+0-40+6weeks_wUnknown`) & !is.na(prop1_ga1$`ga40+0-40+6weeks`)),
                                       c("iso3","CountryName","year",nameUnknown,namesDist,names(ga_1_updated))]

ga_1_problem_42 <- unknown_dataSub_ga1[which(abs(prop2_ga1$`ga>=42+weeks_wUnknown`- prop1_ga1$`ga>=42+weeks`) > 0.02 & !is.na(prop2_ga1$`ga>=42+weeks_wUnknown`) & !is.na(prop1_ga1$`ga>=42+weeks`)),
                                       c("iso3","CountryName","year",nameUnknown,namesDist,names(ga_1_updated))]



datacall.new <- left_join(datacall,unknown_dataSub_ga1[,c("iso3","year",names(ga_1_updated))], by = c("iso3","year"))

### GA 2: redistribute those with missing gestional age -- options 2   ####
dataSub_ga2 <- datacall[datacall$frmt_ga==2 & datacall$Unknown_ga2!=0 & !is.na(datacall$Unknown_ga2),]

namesDist <- names(dataSub_ga2[37:38])
nameUnknown <- names(dataSub_ga2[39])

ga_2_updated<- t(apply(dataSub_ga2,1,FUN = redistFun, namesDist = namesDist, nameUnknown = nameUnknown))
ga_2_updated <- as.data.frame(ga_2_updated)
names(ga_2_updated) <- namesDist
ga_2_updated <- ga_2_updated %>% dplyr::rename_all(paste0, "_wUnknown")

unknown_dataSub_ga2 <- cbind(dataSub_ga2,ga_2_updated)
unknown_dataSub_ga2 <- as.data.frame(unknown_dataSub_ga2)

prop1_ga2 <- unknown_dataSub_ga2[,namesDist]/rowSums(unknown_dataSub_ga2[,namesDist])
prop2_ga2 <- unknown_dataSub_ga2[,names(ga_2_updated)]/rowSums(unknown_dataSub_ga2[,names(ga_2_updated)])

for(i in 1:ncol(prop1_ga2)){
  plot(prop1_ga2[,i],prop2_ga2[,i],main=names(prop1_ga2)[i])
  abline(a=0,b=1,lty=2)
}

datacall.new <- left_join(datacall.new,unknown_dataSub_ga2[,c("iso3","year",names(ga_2_updated))], by = c("iso3","year"))

## proportions seem okay

### GA 3: redistribute those with missing gestional age -- options 3   ####
### NOTE: For this dataset only mexico has unknowns for GA age with option 3, so just doing those 
dataSub_ga3 <- datacall[datacall$frmt_ga==3 & datacall$Unknown_ga3!=0 & !is.na(datacall$Unknown_ga3),]

namesDist <- names(dataSub_ga3[40:47])
nameUnknown <- names(dataSub_ga3[81])

ga_3_updated<- t(apply(dataSub_ga3,1,FUN = redistFun, namesDist = namesDist, nameUnknown = nameUnknown))
ga_3_updated <- as.data.frame(ga_3_updated)
names(ga_3_updated) <- namesDist
ga_3_updated <- ga_3_updated %>% dplyr::rename_all(paste0, "_wUnknown")

unknown_dataSub_ga3 <- cbind(dataSub_ga3,ga_3_updated)
unknown_dataSub_ga3 <- as.data.frame(unknown_dataSub_ga3)

prop1_ga3 <- unknown_dataSub_ga3[,namesDist]/rowSums(unknown_dataSub_ga3[,namesDist],na.rm = T)
prop2_ga3 <- unknown_dataSub_ga3[,names(ga_3_updated)]/rowSums(unknown_dataSub_ga3[,names(ga_3_updated)],na.rm = T)

for(i in c(1,2,8)){
  plot(prop1_ga3[,i],prop2_ga3[,i],main=names(prop1_ga3)[i])
  abline(a=0,b=1,lty=2)
}

datacall.new <- left_join(datacall.new,unknown_dataSub_ga3[,c("iso3","year",names(ga_3_updated))], by = c("iso3","year"))
## if no missing set number of stillbirths equal to what was reported
datacall.new[(datacall.new$frmt_ga==1 & is.na(datacall.new$Unknown_ga1)) | (datacall.new$frmt_ga==1 & datacall.new$Unknown_ga1==0), names(ga_1_updated)] <- datacall.new[(datacall.new$frmt_ga==1 & is.na(datacall.new$Unknown_ga1)) | (datacall.new$frmt_ga==1 & datacall.new$Unknown_ga1==0), names(dataSub_ga1)[15:35]] 
datacall.new[(datacall.new$frmt_ga==2 & is.na(datacall.new$Unknown_ga2)) | (datacall.new$frmt_ga==2 & datacall.new$Unknown_ga2==0), names(ga_2_updated)] <- datacall.new[(datacall.new$frmt_ga==2 & is.na(datacall.new$Unknown_ga2)) | (datacall.new$frmt_ga==2 & datacall.new$Unknown_ga2==0), names(dataSub_ga2)[37:38]] 
datacall.new[(datacall.new$frmt_ga==3 & is.na(datacall.new$Unknown_ga3)) | (datacall.new$frmt_ga==3 & datacall.new$Unknown_ga3==0), names(ga_3_updated)] <- datacall.new[(datacall.new$frmt_ga==3 & is.na(datacall.new$Unknown_ga3)) | (datacall.new$frmt_ga==3 & datacall.new$Unknown_ga3==0), names(dataSub_ga3)[40:47]] 



## now recalculate sbr based on these updated distributions
ga_o1_28_names <- names(ga_1_updated)[7:21]
ga_o2_28_names <- names(ga_2_updated)[1]

ga_o1_22_names<-names(ga_1_updated)[1:21]
ga_o2_22_names<-names(ga_2_updated)[1:2]

datacall.new$ga_28_all_wUnknown <-with(datacall.new,ifelse(frmt_ga==1,rowSums(datacall.new[,ga_o1_28_names]),
                                                            ifelse(frmt_ga==2|frmt_ga==3,rowSums(datacall.new[,ga_o2_28_names]),
                                                                ifelse(frmt_ga==0 & def_short=="x28wks",stillbirth,NA))))


datacall.new$ga_22_all_wUnknown <-with(datacall.new,ifelse(frmt_ga==1,rowSums(datacall.new[,ga_o1_22_names]),
                                                           ifelse(frmt_ga==2|frmt_ga==3,rowSums(datacall.new[,ga_o2_22_names]),
                                                                  ifelse(frmt_ga==0 & def_short=="x28wks",stillbirth,NA))))


datacall.new$sbr_28wks <- 1000 * datacall.new$ga_28_all/(datacall.new$ga_28_all + datacall.new$lb)
datacall.new$sbr_28wks_wUnknown <- 1000 *  datacall.new$ga_28_all_wUnknown/(datacall.new$ga_28_all_wUnknown + datacall.new$lb)


datacall.new$sbr_22wks <- 1000 *  datacall.new$ga_22_all/(datacall.new$ga_22_all + datacall.new$lb)
datacall.new$sbr_22wks_wUnknown <- 1000 *  datacall.new$ga_22_all_wUnknown/(datacall.new$ga_22_all_wUnknown + datacall.new$lb)


#### Plotting comparison 
datacall.new$any_ga_Unknown <- ifelse((datacall.new$Unknown_ga1!=0 & !is.na(datacall.new$Unknown_ga1)) | (datacall.new$Unknown_ga2!=0 & !is.na(datacall.new$Unknown_ga2) ) | (datacall.new$Unknown_ga3!=0 & !is.na(datacall.new$Unknown_ga3)),TRUE,FALSE)
unknownGAcountry <- unique(datacall.new$CountryName[datacall.new$any_ga_Unknown])

plotGAunkonwn <- datacall.new[datacall.new$CountryName %in% unknownGAcountry,]


col.palette <- c(brewer.pal(12, "Paired")[c(2, 4, 10, 6, 8,12)], 
                 brewer.pal(8, "Dark2")[c(4,8)],
                 brewer.pal(12, "Paired")[c(1, 3, 5, 7, 9, 11)]) # add color palette

pdf("C:/Users/anmishra/Desktop/unknownGA_DistComp_wUnknownProp.pdf",width = 17, height=10)
for(i in 1:length(unique(plotGAunkonwn$CountryName))){
  plotdata<-subset(plotGAunkonwn,CountryName == unique(CountryName)[i])
  plotdata$unkonwnGA <- ifelse(plotdata$frmt_ga==1, plotdata$Unknown_ga1,
                               ifelse(plotdata$frmt_ga==2, plotdata$Unknown_ga2,
                                      ifelse(plotdata$frmt_ga==3, plotdata$Unknown_ga3,NA)))
  
  plotdata$unkonwnGA[plotdata$frmt_ga==2 & is.na(plotdata$Unknown_ga2) & !is.na(plotdata$Unknown_ga3)] <- plotdata$Unknown_ga3[plotdata$frmt_ga==2 & is.na(plotdata$Unknown_ga2) & !is.na(plotdata$Unknown_ga3)]
  
  plotdata$propUnknown <- NA
  plotdata$propUnknown[plotdata$frmt_ga==1] <- plotdata$unkonwnGA[plotdata$frmt_ga==1]/rowSums(plotdata[plotdata$frmt_ga==1,15:36],na.rm = T)
  plotdata$propUnknown[plotdata$frmt_ga==2] <- plotdata$unkonwnGA[plotdata$frmt_ga==2]/rowSums(plotdata[plotdata$frmt_ga==2,37:39],na.rm = T)
  plotdata$propUnknown[plotdata$frmt_ga==3] <- plotdata$unkonwnGA[plotdata$frmt_ga==3]/rowSums(plotdata[plotdata$frmt_ga==3,40:48],na.rm = T)
  plotdata$propUnknown[plotdata$frmt_ga==0] <- NA
  plotdata$propUnknown <- ifelse(plotdata$propUnknown > 1, NA, plotdata$propUnknown)
  
  layout(matrix(c(1,2),ncol=1,nrow=2,byrow = F), heights = c(6,2))
  par(mai = c(0.4,0.8,0.2,0.4) + 0.02,oma = c(1,1,2.2,1))
  if(all(is.na(plotdata$sbr_28wks_wUnknown))==FALSE ){
    xrange <- range(plotdata$year) 
    yrange <- max(plotdata[,c("sbr_28wks","sbr_28wks_wUnknown","sbr_22wks","sbr_22wks_wUnknown")],na.rm = T) 
    if(length(unique(xrange))==1){
      xrange=c(plotdata$year-2,plotdata$year+2)
      yrange=c(plotdata$sbr_28wks+2)
    }
    
    plot(plotdata$year, plotdata$sbr_28wks, type="o",pch = 16, col=alpha(col.palette[1],0.55),cex=1.5,xlab="Year",ylab="SBR",xlim = xrange, ylim=c(0,yrange),lty=2)
    lines(plotdata$year, plotdata$sbr_28wks_wUnknown, type="o",pch = 16, col=alpha(col.palette[2],0.55),cex=1.5)
    points(plotdata$year[plotdata$propUnknown > 0.5 & !is.na(plotdata$propUnknown)], 
           plotdata$sbr_28wks_wUnknown[plotdata$propUnknown > 0.5 & !is.na(plotdata$propUnknown)], pch = 4, col = "red",cex=2,lwd = 2)
    
    legend("bottomright",c("28 wks w/ unknowns", "28 weeks"), lty=c(1,2), pch = c(16,16), 
           col= c(alpha(col.palette[2],0.55),alpha(col.palette[1],0.55)),bty = "n",cex=1.3,y.intersp = 1.5)
    if(sum(plotdata$propUnknown > 0.5 & !is.na(plotdata$propUnknown))>0){
      legend("bottom", legend = "Prop. Unknown > 0.5",pch = 4, col = alpha("red",0.55),bty="n",cex=1.3,lwd = 2)
    }
  }
  
  if(all(is.na(plotdata$sbr_22wks_wUnknown))==FALSE ){
    lines(plotdata$year, plotdata$sbr_22wks, type="o",pch = 15, col=alpha(col.palette[1],0.55),cex=1.5,lty=2)
    
    lines(plotdata$year, plotdata$sbr_22wks_wUnknown, type="o",pch = 15, col=alpha(col.palette[2],0.55),cex=1.5)
    points(plotdata$year[plotdata$propUnknown > 0.5 & !is.na(plotdata$propUnknown)], 
           plotdata$sbr_22wks_wUnknown[plotdata$propUnknown > 0.5 & !is.na(plotdata$propUnknown)], pch = 4, col = "red",cex=2,lwd = 2)
    
    legend("bottomleft",c("22 wks w/ unknowns", "22 wks"), lty=c(1,2), pch = c(15,15), 
           col= c(alpha(col.palette[2],0.55),alpha(col.palette[1],0.55)),bty = "n",cex=1.3,y.intersp = 1.5)
    
    if(sum(plotdata$propUnknown > 0.5 & !is.na(plotdata$propUnknown))>0 & all(is.na(plotdata$sbr_28wks_wUnknown))==TRUE){
      legend("bottom", legend = "Prop. Unknown > 0.5",pch = 4, col = alpha("red",0.55),bty="n",y.intersp = 1.5)
    }
    
  }
  
  
  if(all(is.na(plotdata$sbr_28wks_wUnknown))==FALSE | all(is.na(plotdata$sbr_22wks_wUnknown))==FALSE){
    yBarMax <- max(plotdata$propUnknown,na.rm = 2) + 0.02
    barplot(height = plotdata$propUnknown,ylim=c(0,yBarMax), ylab="Proportion of Unknowns",width=0.4, names.arg = plotdata$year)
    title(plotdata$CountryName[1],outer=TRUE)
    
    if(length(unique(plotdata$Universe_ga[!is.na(plotdata$Universe_ga)]))==1){
      title(main=paste("Unknown universe:",unique(plotdata$Universe_ga[!is.na(plotdata$Universe_ga)])), line = 0, outer= TRUE,cex.main=0.8)
    }
    else if(length(unique(plotdata$Universe_ga[!is.na(plotdata$Universe_ga)])) == 2){
      
      title(main=paste("Unknown universe: ",
                       unique(plotdata$Universe_ga[!is.na(plotdata$Universe_ga)])[1]," (", 
                       min(plotdata$year[plotdata$Universe_ga %in% unique(plotdata$Universe_ga[!is.na(plotdata$Universe_ga)])[1]]),"-",
                       max(plotdata$year[plotdata$Universe_ga %in% unique(plotdata$Universe_ga[!is.na(plotdata$Universe_ga)])[1]]),")",sep = ""),
            line = 0, outer= TRUE,cex.main=0.8, adj = 0)
      
      title(main=paste("Unknown universe: ",
                       unique(plotdata$Universe_ga[!is.na(plotdata$Universe_ga)])[2]," (", 
                       min(plotdata$year[plotdata$Universe_ga %in% unique(plotdata$Universe_ga[!is.na(plotdata$Universe_ga)])[2]]),"-",
                       max(plotdata$year[plotdata$Universe_ga %in% unique(plotdata$Universe_ga[!is.na(plotdata$Universe_ga)])[2]]),")",sep = ""),
            line = 0, outer= TRUE,cex.main=0.8, adj = 1)
      
      
    }
    
    else if(length(unique(plotdata$Universe_ga[!is.na(plotdata$Universe_ga)])) == 3){
      
      title(main=paste("Unknown universe: ",
                       unique(plotdata$Universe_ga[!is.na(plotdata$Universe_ga)])[1]," (", 
                       min(plotdata$year[plotdata$Universe_ga %in% unique(plotdata$Universe_ga[!is.na(plotdata$Universe_ga)])[1]]),"-",
                       max(plotdata$year[plotdata$Universe_ga %in% unique(plotdata$Universe_ga[!is.na(plotdata$Universe_ga)])[1]]),")",sep = ""),
            line = 0, outer= TRUE,cex.main=0.8, adj = 0)
      
      title(main=paste("Unknown universe: ",
                       unique(plotdata$Universe_ga[!is.na(plotdata$Universe_ga)])[2]," (", 
                       min(plotdata$year[plotdata$Universe_ga %in% unique(plotdata$Universe_ga[!is.na(plotdata$Universe_ga)])[2]]),"-",
                       max(plotdata$year[plotdata$Universe_ga %in% unique(plotdata$Universe_ga[!is.na(plotdata$Universe_ga)])[2]]),")",sep = ""),
            line = 0, outer= TRUE,cex.main=0.8, adj = 0.5)
      
      
      title(main=paste("Unknown universe: ",
                       unique(plotdata$Universe_ga[!is.na(plotdata$Universe_ga)])[3]," (", 
                       min(plotdata$year[plotdata$Universe_ga %in% unique(plotdata$Universe_ga[!is.na(plotdata$Universe_ga)])[2]]),"-",
                       max(plotdata$year[plotdata$Universe_ga %in% unique(plotdata$Universe_ga[!is.na(plotdata$Universe_ga)])[2]]),")",sep = ""),
            line = 0, outer= TRUE,cex.main=0.8, adj = 1)
      
    }
    
  }
}
dev.off()


#### Redistribute Birthweight Unknowns ############


##### BWT1: Redistribute those with missing bw -- option 1 ####
#subset to this observations with unknowns and GA 1
dataSub_bw1 <- datacall[datacall$frmt_bw==1 & datacall$Unknown_bw1!=0 & !is.na(datacall$Unknown_bw1),]

namesDist <- names(dataSub_bw1[85:94])
nameUnknown <- names(dataSub_bw1[95])

## apply redistribution function
sex_updated<- t(apply(dataSub_bw1,1,FUN = redistFun, namesDist = namesDist, nameUnknown = nameUnknown))
sex_updated <- as.data.frame(sex_updated)
names(sex_updated) <- namesDist
sex_updated <- sex_updated %>% dplyr::rename_all(paste0, "_wUnknown")

unknown_dataSub_bw1 <- cbind(dataSub_bw1,sex_updated)
unknown_dataSub_bw1 <- as.data.frame(unknown_dataSub_bw1)


## compare old and new proportions 
prop1_bw1 <- unknown_dataSub_bw1[,namesDist]/rowSums(unknown_dataSub_bw1[,namesDist])
prop2_bw1 <- unknown_dataSub_bw1[,names(sex_updated)]/rowSums(unknown_dataSub_bw1[,names(sex_updated)])

for(i in 1:ncol(prop1_bw1)){
  plot(prop1_bw1[,i],prop2_bw1[,i],main=names(prop1_bw1)[i])
  abline(a=0,b=1,lty=2)
}

#for those proportions that seem off check counts more closely (all small countries)
sex_problem_4500plus <- unknown_dataSub_bw1[which(abs(prop1_bw1$`bw4500+g`- prop2_bw1$`bw4500+g_wUnknown`) > 0.025 & !is.na(prop2_bw1$`bw4500+g_wUnknown`) & !is.na(prop1_bw1$`bw4500+g`)),
                                       c("iso3","CountryName","year",nameUnknown,namesDist,names(sex_updated))]


datacall.new <- left_join(datacall,unknown_dataSub_bw1[,c("iso3","year",names(sex_updated))], by = c("iso3","year"))


### BW 2: redistribute those with missing bw -- options 2   ####
dataSub_bw2 <- datacall[datacall$frmt_bw==2 & datacall$Unknown_bw2!=0 & !is.na(datacall$Unknown_bw2),]

namesDist <- names(dataSub_bw2[96:97])
nameUnknown <- names(dataSub_bw2[98])

bw_2_updated<- t(apply(dataSub_bw2,1,FUN = redistFun, namesDist = namesDist, nameUnknown = nameUnknown))
bw_2_updated <- as.data.frame(bw_2_updated)
names(bw_2_updated) <- namesDist
bw_2_updated <- bw_2_updated %>% dplyr::rename_all(paste0, "_wUnknown")

unknown_dataSub_bw2 <- cbind(dataSub_bw2,bw_2_updated)
unknown_dataSub_bw2 <- as.data.frame(unknown_dataSub_bw2)

prop1_bw2 <- unknown_dataSub_bw2[,namesDist]/rowSums(unknown_dataSub_bw2[,namesDist])
prop2_bw2 <- unknown_dataSub_bw2[,names(bw_2_updated)]/rowSums(unknown_dataSub_bw2[,names(bw_2_updated)])

for(i in 1:ncol(prop1_bw2)){
  plot(prop1_bw2[,i],prop2_bw2[,i],main=names(prop1_bw2)[i])
  abline(a=0,b=1,lty=2)
}

datacall.new <- left_join(datacall.new,unknown_dataSub_bw2[,c("iso3","year",names(bw_2_updated))], by = c("iso3","year"))



### BW 3: redistribute those with missing bw -- options 3   ####
dataSub_bw3 <- datacall[datacall$frmt_bw==3 & datacall$Unknown_bw3!=0 & !is.na(datacall$Unknown_bw3),]

namesDist <- names(dataSub_bw3[99:114])
nameUnknown <- names(dataSub_bw3[115])

bw_3_updated<- t(apply(dataSub_bw3,1,FUN = redistFun, namesDist = namesDist, nameUnknown = nameUnknown))
bw_3_updated <- as.data.frame(bw_3_updated)
names(bw_3_updated) <- namesDist
bw_3_updated <- bw_3_updated %>% dplyr::rename_all(paste0, "_wUnknown")

unknown_dataSub_bw3 <- cbind(dataSub_bw3,bw_3_updated)
unknown_dataSub_bw3 <- as.data.frame(unknown_dataSub_bw3)

prop1_bw3 <- unknown_dataSub_bw3[,namesDist]/rowSums(unknown_dataSub_bw3[,namesDist],na.rm = T)
prop2_bw3 <- unknown_dataSub_bw3[,names(bw_3_updated)]/rowSums(unknown_dataSub_bw3[,names(bw_3_updated)],na.rm = T)

for(i in 11:16){
  plot(prop1_bw3[,i],prop2_bw3[,i],main=names(prop1_bw3)[i])
  abline(a=0,b=1,lty=2)
}

datacall.new <- left_join(datacall.new,unknown_dataSub_bw3[,c("iso3","year",names(bw_3_updated))], by = c("iso3","year"))



## if no missing set number of stillbirths equal to what was reported
datacall.new[(datacall.new$frmt_bw==1 & is.na(datacall.new$Unknown_bw1)) | (datacall.new$frmt_bw==1 & datacall.new$Unknown_bw1==0), names(sex_updated)] <- datacall.new[(datacall.new$frmt_bw==1 & is.na(datacall.new$Unknown_bw1)) | (datacall.new$frmt_bw==1 & datacall.new$Unknown_bw1==0), names(dataSub_bw1)[85:94]] 
datacall.new[(datacall.new$frmt_bw==2 & is.na(datacall.new$Unknown_bw2)) | (datacall.new$frmt_bw==2 & datacall.new$Unknown_bw2==0), names(bw_2_updated)] <- datacall.new[(datacall.new$frmt_bw==2 & is.na(datacall.new$Unknown_bw2)) | (datacall.new$frmt_bw==2 & datacall.new$Unknown_bw2==0), names(dataSub_bw2)[96:97]] 
datacall.new[(datacall.new$frmt_bw==3 & is.na(datacall.new$Unknown_bw3)) | (datacall.new$frmt_bw==3 & datacall.new$Unknown_bw3==0), names(bw_3_updated)] <- datacall.new[(datacall.new$frmt_bw==3 & is.na(datacall.new$Unknown_bw3)) | (datacall.new$frmt_bw==3 & datacall.new$Unknown_bw3==0), names(dataSub_bw3)[99:114]] 



## now recalculate sbr based on these updated distributions
bw_o1_1000_names <- names(sex_updated)[3:10]
bw_o2_1000_names <- names(bw_2_updated)[2]

bw_o1_500_names<-names(sex_updated)[2:10]
bw_o2_500_names<-names(bw_2_updated)[1:2]

datacall.new$bw_1000_all_wUnknown <-with(datacall.new,ifelse(frmt_bw==1,rowSums(datacall.new[,bw_o1_1000_names]),
                                                           ifelse(frmt_bw==2|frmt_bw==3,rowSums(datacall.new[,bw_o2_1000_names]),
                                                                  ifelse(frmt_bw==0 & def_short=="x1000",stillbirth,NA))))


datacall.new$bw_500_all_wUnknown <-with(datacall.new,ifelse(frmt_bw==1,rowSums(datacall.new[,bw_o1_500_names]),
                                                           ifelse(frmt_bw==2|frmt_bw==3,rowSums(datacall.new[,bw_o2_500_names]),
                                                                  ifelse(frmt_bw==0 & def_short=="x500",stillbirth,NA))))


datacall.new$sbr_1000g <- 1000 * datacall.new$bw_1000_all/(datacall.new$bw_1000_all + datacall.new$lb)
datacall.new$sbr_1000g_wUnknown <- 1000 *  datacall.new$bw_1000_all_wUnknown/(datacall.new$bw_1000_all_wUnknown + datacall.new$lb)


datacall.new$sbr_500g <- 1000 *  datacall.new$bw_500_all/(datacall.new$bw_500_all + datacall.new$lb)
datacall.new$sbr_500g_wUnknown <- 1000 *  datacall.new$bw_500_all_wUnknown/(datacall.new$bw_500_all_wUnknown + datacall.new$lb)


#### Plotting comparison 
datacall.new$any_bw_Unknown <- ifelse((datacall.new$Unknown_bw1!=0 & !is.na(datacall.new$Unknown_bw1)) | (datacall.new$Unknown_bw2!=0 & !is.na(datacall.new$Unknown_bw2) ) | (datacall.new$Unknown_bw3!=0 & !is.na(datacall.new$Unknown_bw3)),TRUE,FALSE)
unknownbwcountry <- unique(datacall.new$CountryName[datacall.new$any_bw_Unknown])

plotbwunkonwn <- datacall.new[datacall.new$CountryName %in% unknownbwcountry,]


col.palette <- c(brewer.pal(12, "Paired")[c(2, 4, 10, 6, 8,12)], 
                 brewer.pal(8, "Dark2")[c(4,8)],
                 brewer.pal(12, "Paired")[c(1, 3, 5, 7, 9, 11)]) # add color palette


pdf("C:/Users/anmishra/Desktop/unknownBW_DistComp_wUnknownProp.pdf",width = 17, height=10)
for(i in 1:length(unique(plotbwunkonwn$CountryName))){
  plotdata<-subset(plotbwunkonwn,CountryName == unique(CountryName)[i])
  plotdata$unkonwnBW <- ifelse(plotdata$frmt_bw==1, plotdata$Unknown_bw1,
                               ifelse(plotdata$frmt_bw==2, plotdata$Unknown_bw2,
                                      ifelse(plotdata$frmt_bw==3, plotdata$Unknown_bw3,NA)))
  
  plotdata$unkonwnBW[plotdata$frmt_bw==2 & is.na(plotdata$Unknown_bw2) & !is.na(plotdata$Unknown_bw3)] <- plotdata$Unknown_bw3[plotdata$frmt_bw==2 & is.na(plotdata$Unknown_bw2) & !is.na(plotdata$Unknown_bw3)]
  
  plotdata$propUnknown <- NA
  plotdata$propUnknown[plotdata$frmt_bw==1] <- plotdata$unkonwnBW[plotdata$frmt_bw==1]/rowSums(plotdata[plotdata$frmt_bw==1,85:95],na.rm = T)
  plotdata$propUnknown[plotdata$frmt_bw==2] <- plotdata$unkonwnBW[plotdata$frmt_bw==2]/rowSums(plotdata[plotdata$frmt_bw==2,96:98],na.rm = T)
  plotdata$propUnknown[plotdata$frmt_bw==3] <- plotdata$unkonwnBW[plotdata$frmt_bw==3]/rowSums(plotdata[plotdata$frmt_bw==3,99:115],na.rm = T)
  plotdata$propUnknown[plotdata$frmt_bw==0] <- NA
  plotdata$propUnknown <- ifelse(plotdata$propUnknown > 1, NA, plotdata$propUnknown)
  
  layout(matrix(c(1,2),ncol=1,nrow=2,byrow = F), heights = c(6,2))
  par(mai = c(0.4,0.8,0.2,0.4) + 0.02,oma = c(1,1,2.2,1))
  if(all(is.na(plotdata$sbr_1000g_wUnknown))==FALSE & all(is.na(plotdata$sbr_1000g))==FALSE ){
    xrange <- range(plotdata$year) 
    yrange <- max(plotdata[,c("sbr_1000g","sbr_1000g_wUnknown","sbr_500g","sbr_500g_wUnknown")],na.rm = T) 
    if(length(unique(xrange))==1){
      xrange=c(plotdata$year-2,plotdata$year+2)
      yrange=c(plotdata$sbr_1000g+2)
    }
    
    plot(plotdata$year, plotdata$sbr_1000g, type="o",pch = 16, col=alpha(col.palette[1],0.55),cex=1.5,xlab="Year",ylab="SBR",xlim = xrange, ylim=c(0,yrange),lty=2)
    lines(plotdata$year[!is.na(plotdata$sbr_1000g)], plotdata$sbr_1000g_wUnknown[!is.na(plotdata$sbr_1000g)], type="o",pch = 16, col=alpha(col.palette[2],0.55),cex=1.5)
    
    points(plotdata$year[plotdata$propUnknown > 0.5 & !is.na(plotdata$propUnknown)& !is.na(plotdata$sbr_1000g)], 
           plotdata$sbr_1000g_wUnknown[plotdata$propUnknown > 0.5 & !is.na(plotdata$propUnknown) & !is.na(plotdata$sbr_1000g)], 
           pch = 4, col = "red",cex=2,lwd = 2)
    
    legend("bottomright",c("1000g w/ unknowns", "1000g"), lty=c(1,2), pch = c(16,16), 
           col= c(alpha(col.palette[2],0.55),alpha(col.palette[1],0.55)),bty = "n",cex=1.3,y.intersp = 1.5)
    if(sum(plotdata$propUnknown > 0.5 & !is.na(plotdata$propUnknown))>0){
      legend("bottom", legend = "Prop. Unknown > 0.5",pch = 4, col = alpha("red",0.55),bty="n",cex=1.3,lwd = 2)
    }
    
  }
  
  if(all(is.na(plotdata$sbr_500g_wUnknown))==FALSE & all(is.na(plotdata$sbr_500g))==FALSE ){
    lines(plotdata$year, plotdata$sbr_500g, type="o",pch = 15, col=alpha(col.palette[1],0.55),cex=1.5,lty=2)
    lines(plotdata$year[!is.na(plotdata$sbr_500g)], plotdata$sbr_500g_wUnknown[!is.na(plotdata$sbr_500g)], type="o",pch = 15, col=alpha(col.palette[2],0.55),cex=1.5)
    
    points(plotdata$year[plotdata$propUnknown > 0.5 & !is.na(plotdata$propUnknown) & !is.na(plotdata$sbr_500g)], 
           plotdata$sbr_500g_wUnknown[plotdata$propUnknown > 0.5 & !is.na(plotdata$propUnknown) & !is.na(plotdata$sbr_500g)], 
           pch = 4, col = "red",cex=2,lwd = 2)
    
    legend("bottomleft",c("500 g w/ unknowns", "500 g"), lty=c(1,2), pch = c(15,15), 
           col= c(alpha(col.palette[2],0.55),alpha(col.palette[1],0.55)),bty = "n",cex=1.3,y.intersp = 1.5)
    
    if(sum(plotdata$propUnknown > 0.5 & !is.na(plotdata$propUnknown))>0 & all(is.na(plotdata$sbr_1000g_wUnknown))==TRUE){
      legend("bottom", legend = "Prop. Unknown > 0.5",pch = 4, col = alpha("red",0.55),bty="n",y.intersp = 1.5)
    }
    
  }
  
  if(all(is.na(plotdata$sbr_500g))==FALSE | all(is.na(plotdata$sbr_1000g))==FALSE){
    yBarMax <- max(plotdata$propUnknown,na.rm = 2) + 0.02
    barplot(height = plotdata$propUnknown,ylim=c(0,yBarMax), ylab="Proportion of Unknowns",width=0.4, names.arg = plotdata$year)
    title(plotdata$CountryName[1],outer=TRUE, line = 1)
    
    ## add universt
    if(length(unique(plotdata$Universe_bw[!is.na(plotdata$Universe_bw)]))==1){
      title(main=paste("Unknown universe:",unique(plotdata$Universe_bw[!is.na(plotdata$Universe_bw)])), line = 0, outer= TRUE,cex.main=0.8)
    }
    else if(length(unique(plotdata$Universe_bw[!is.na(plotdata$Universe_bw)])) > 1){
      
      title(main=paste("Unknown universe: ",
                       unique(plotdata$Universe_bw[!is.na(plotdata$Universe_bw)])[1]," (", 
                       min(plotdata$year[plotdata$Universe_bw %in% unique(plotdata$Universe_bw[!is.na(plotdata$Universe_bw)])[1]]),"-",
                       max(plotdata$year[plotdata$Universe_bw %in% unique(plotdata$Universe_bw[!is.na(plotdata$Universe_bw)])[1]]),")",sep = ""),
            line = 0, outer= TRUE,cex.main=0.8, adj = 0)
      
      title(main=paste("Unknown universe: ",
                       unique(plotdata$Universe_bw[!is.na(plotdata$Universe_bw)])[2]," (", 
                       min(plotdata$year[plotdata$Universe_bw %in% unique(plotdata$Universe_bw[!is.na(plotdata$Universe_bw)])[2]]),"-",
                       max(plotdata$year[plotdata$Universe_bw %in% unique(plotdata$Universe_bw[!is.na(plotdata$Universe_bw)])[2]]),")",sep = ""),
            line = 0, outer= TRUE,cex.main=0.8, adj = 1)
      
      
    }
    
    else if(length(unique(plotdata$Universe_ga[!is.na(plotdata$Universe_ga)])) == 3){
      
      title(main=paste("Unknown universe: ",
                       unique(plotdata$Universe_ga[!is.na(plotdata$Universe_ga)])[1]," (", 
                       min(plotdata$year[plotdata$Universe_ga %in% unique(plotdata$Universe_ga[!is.na(plotdata$Universe_ga)])[1]]),"-",
                       max(plotdata$year[plotdata$Universe_ga %in% unique(plotdata$Universe_ga[!is.na(plotdata$Universe_ga)])[1]]),")",sep = ""),
            line = 0, outer= TRUE,cex.main=0.8, adj = 0)
      
      title(main=paste("Unknown universe: ",
                       unique(plotdata$Universe_ga[!is.na(plotdata$Universe_ga)])[2]," (", 
                       min(plotdata$year[plotdata$Universe_ga %in% unique(plotdata$Universe_ga[!is.na(plotdata$Universe_ga)])[2]]),"-",
                       max(plotdata$year[plotdata$Universe_ga %in% unique(plotdata$Universe_ga[!is.na(plotdata$Universe_ga)])[2]]),")",sep = ""),
            line = 0, outer= TRUE,cex.main=0.8, adj = 0.5)
      
      
      title(main=paste("Unknown universe: ",
                       unique(plotdata$Universe_ga[!is.na(plotdata$Universe_ga)])[3]," (", 
                       min(plotdata$year[plotdata$Universe_ga %in% unique(plotdata$Universe_ga[!is.na(plotdata$Universe_ga)])[2]]),"-",
                       max(plotdata$year[plotdata$Universe_ga %in% unique(plotdata$Universe_ga[!is.na(plotdata$Universe_ga)])[2]]),")",sep = ""),
            line = 0, outer= TRUE,cex.main=0.8, adj = 1)
      
    }
  }
}
dev.off()

