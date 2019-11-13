

library(rstan)

#setwd("E:/doc/research/birth rate/SBR2018")

fit <- readRDS(file = "simulation/rdsoutput/exratio_base_model_standardize.rds")
stan.data <- readRDS(file = "output/stan.data.exratio.rds")


print(fit,pars = c("beta","beta_dt","beta_r","rho","sigma_ar","sigma_j"))

array <- rstan::extract(fit)
array$mu_ct[,,1]

mu_ct <- array$mu_ct + array$delta_ct

muhat <-c()
for(c in 1:195){
  for(t in 1:19){
    muhat <- rbind(muhat,quantile(mu_ct[,c,t],c(.5)))
  }
}

estyears <- seq(2000,2018)
year <- rep(estyears,times=195)
countryiso <- countryRegionList$iso
iso <- rep(countryiso,each=19)
muhat <- data.frame(muhat,year,iso)

muhat


stan.data$getc

###############################################


logadj.sbr <- stan.data$Y
var.sbr <- stan.data$var_i

for(i in 1:stan.data$N){
  
  if(stan.data$getd_i[i]==2) {logadj.sbr[i] = logadj.sbr[i] + 0.07
  var.sbr[i] = var.sbr[i] + 0.09} 
  if(stan.data$getd_i[i]==3) {logadj.sbr[i] = logadj.sbr[i] - 0.38
  var.sbr[i] = var.sbr[i] + 0.15}
  if(stan.data$getd_i[i]==4) {logadj.sbr[i] = logadj.sbr[i] - 0.27
  var.sbr[i] = var.sbr[i] + 0.12}
  
}

for(i in 1:stan.data$N){
  if(stan.data$getj_i[i]==1) var.sbr[i] = var.sbr[i] + 0.06
  if(stan.data$getj_i[i]==2) {logadj.sbr[i] = logadj.sbr[i] - 0.22
  var.sbr[i] = var.sbr[i] + 0.46}
  if(stan.data$getj_i[i]==3) {logadj.sbr[i] = logadj.sbr[i] + 0.06
  var.sbr[i] = var.sbr[i] + 0.2}
}

res.data <- data.frame(year = stan.data$gett_i+1999, country_idx = stan.data$getc_i, logadj.sbr =  logadj.sbr, var.sbr = var.sbr)%>%
               merge(countryRegionList, by="country_idx")



resplot <- res.data %>%     merge(muhat,by = c("iso","year"))

res.i <- resplot$logadj.sbr - resplot$X50.

hist(res.i)
st.res.i <- res.i/sqrt(resplot$var.sbr)
length(st.res.i)

hist(st.res.i)
##individual observation function
getc.i=stan.data$getc_i
gett.i=stan.data$gett_i
getr.c=stan.data$getr_c
getj.i=stan.data$getj_i
getd.i=stan.data$getd_i

x.i<-function(varmatrix){
  varname.i<-rep(NA,stan.data$N)
  for (i in 1:stan.data$N) {varname.i[i]<-varmatrix[getc.i[i],gett.i[i]]}
  varname.i
}

gni.i<-x.i(stan.data$Z1)
nmr.i<-x.i(stan.data$Z2)
lbw.i<-x.i(stan.data$Z3)
anc4.i<-x.i(stan.data$Z4)
edu.i<-x.i(stan.data$Z5)
getr.i <- getr.c[getc.i]

length(gni.i)

# plot residuals against predictors, yhat, time
pdf_name3 <- paste0("fig/res_region.pdf")
pdf(pdf_name3, width = 10, height = 12)
par(mfrow=c(2,2))
#plot(res.i~edu.i+anc.i+lbw.i+nmr.i+gni.i, col = getr.i)
plot(st.res.i~gni.i, col = getr.i, cex=1)
curve(predict(loess(st.res.i~gni.i),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("MDG region 1", "MDG region 2", "MDG region 3"),
       col=c(1:3), pch=1, cex=0.8)

plot(st.res.i~nmr.i, col = getr.i,cex=1)
curve(predict(loess(st.res.i~nmr.i),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("MDG region 1", "MDG region 2", "MDG region 3"),
       col=c(1:3), pch=1, cex=0.8)

plot(st.res.i~lbw.i, col = getr.i,cex=1)
curve(predict(loess(st.res.i~lbw.i),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("MDG region 1", "MDG region 2", "MDG region 3"),
       col=c(1:3), pch=1, cex=0.8)

plot(st.res.i~anc4.i, col = getr.i,cex=1)
curve(predict(loess(st.res.i~anc4.i),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("MDG region 1", "MDG region 2", "MDG region 3"),
       col=c(1:3), pch=1, cex=0.8)

plot(st.res.i~edu.i, col = getr.i,cex=1)
curve(predict(loess(st.res.i~edu.i),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("MDG region 1", "MDG region 2", "MDG region 3"),
       col=c(1:3), pch=1, cex=0.8)

plot(st.res.i~ resplot$logadj.sbr, col = getr.i,cex=1)
curve(predict(loess(st.res.i ~ resplot$logadj.sbr),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("MDG region 1", "MDG region 2", "MDG region 3"),
       col=c(1:3), pch=1, cex=0.8)


######################################################
plot(st.res.i~gni.i, col = getd.i, cex=1)
curve(predict(loess(st.res.i~gni.i),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("ge28wks", "ge1000g", "ge22wks","ge500g"),
       col=c(1:3), pch=1, cex=0.8)

plot(st.res.i~nmr.i, col = getd.i,cex=1)
curve(predict(loess(st.res.i~nmr.i),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("ge28wks", "ge1000g", "ge22wks","ge500g"),
       col=c(1:3), pch=1, cex=0.8)

plot(st.res.i~lbw.i, col = getd.i,cex=1)
curve(predict(loess(st.res.i~lbw.i),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("ge28wks", "ge1000g", "ge22wks","ge500g"),
       col=c(1:3), pch=1, cex=0.8)

plot(st.res.i~anc4.i, col = getd.i,cex=1)
curve(predict(loess(st.res.i~anc4.i),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("ge28wks", "ge1000g", "ge22wks","ge500g"),
       col=c(1:3), pch=1, cex=0.8)

plot(st.res.i~edu.i, col = getd.i,cex=1)
curve(predict(loess(st.res.i~edu.i),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("ge28wks", "ge1000g", "ge22wks","ge500g"),
       col=c(1:3), pch=1, cex=0.8)

plot(st.res.i~ resplot$logadj.sbr, col = getd.i,cex=1)
curve(predict(loess(st.res.i ~ resplot$logadj.sbr),x), add = T, col = 2, lwd= 3)
abline(h=0)
legend("topleft", legend=c("ge28wks", "ge1000g", "ge22wks","ge500g"),
       col=c(1:3), pch=1, cex=0.8)

dev.off()


