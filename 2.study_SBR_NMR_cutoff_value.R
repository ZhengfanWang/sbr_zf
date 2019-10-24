###  SBR:NMR cutoff value study


# load high quality lmin data
hq_data <- openxlsx::read.xlsx("E:/doc/research/birth rate/SBR2018/input/High Quality LMIC data_SBR_NMR.xlsx")
full_data <- readRDS("E:/doc/research/birth rate/SBR2018/output/sbr.full.rds")


tb <- hq_data$lb + hq_data$sb
sbr <- hq_data$sbr
lb <- hq_data$lb
nmr <- hq_data$nmr

set.seed(123)
n <- length(tb)
S <- 1000
ratio_s <- matrix(NA,ncol=S,nrow=n)
for(i in 1:n){
  sb_s <- rbinom(S,tb[i],sbr[i]/1000)
  nm_s <- rbinom(S,lb[i],nmr[i]/1000)
  ratio_s[i,] <- sb_s/tb[i]*1/(nm_s/lb[i])
}


percentile2 <- apply(ratio_s,1,quantile,na.rm = TRUE,c(0.01,0.025,0.05,0.075,0.1,0.15,0.2))

t(round(apply(percentile2,1,quantile,na.rm=TRUE,c(0,0.025,0.05,0.10,0.15,0.5)),digits = 2))


#title <- c("1%","2.5%","5%","7.5%","10%","15%","20%")
#par(mfrow=c(3,3))
#for(i in 1:7){
#  histplot(percentile2[i,],breaks = 5, title = title[i], xlim = c(0,2),ylim = c(0,0.5))
#}


    