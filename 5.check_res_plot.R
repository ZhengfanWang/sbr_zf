
library(rstan)

fit <- readRDS(file = "rdsoutput/reg_hs_nval_t.rds")
stan.data <- readRDS(file = "output/stan_data/hs_nval.rds")


array <- rstan::extract(fit)
mu_ct <- array$mu_ct + array$delta_ct
numcun <- max(stan.data$getc_i)
numyear <- max(stan.data$gett_i)
estyears <- seq(2000,2020)

muhat <-c()
for(c in 1:numcun){
  for(t in 1:numyear){
    muhat <- rbind(muhat,quantile(mu_ct[,c,t],c(.5)))
  }
}


year <- rep(estyears,times=numcun)
countryiso <- countryRegionList$iso
iso <- rep(countryiso,each=numyear)
muhat <- data.frame(est = muhat,year = numyear,iso)


###############################################


sd <- apply(array$sigma_i,2,median)
error <- apply(array$prep,2,median)-stan.data$Y

res.data <- data.frame(year = stan.data$gett_i+1999,
                       country_idx = stan.data$getc_i, 
                       error =  error, 
                       sd = sd)%>%
               merge(countryRegionList, by="country_idx")

res.i <- res.data$error
sd.i <- res.data$sdg

hist(res.i)
st.res.i <- res.i/sd.i
length(st.res.i)

hist(st.res.i)
##individual observation function
getc.i <- stan.data$getc_i
gett.i <- stan.data$gett_i
getr.c <- stan.data$getr_c
getj.i <- stan.data$getj_i
getd.i <- stan.data$getd_i
getr.i <- getr.c[getc.i]
x.i<-function(varmatrix){
  varname.i<-rep(NA,stan.data$N)
  for (i in 1:stan.data$N) {varname.i[i]<-varmatrix[getc.i[i],gett.i[i]]}
  varname.i
}

int_cov <- c("gni_sm","nmr","lbw_sm","anc4_sm","mean_edu_f_sm",
             "gini_sm","urban","gfr","sab","anc1_sm","abr_sm",
             "csec_sm","pab_sm","pfpr","gdp","mmr")
varname <- "nmr"

getvar_i <- function(varname,interest_cov = intcov){
   var.index <- which(varname == int_cov) 
   var.i <- x.i(stan.data$covar_array[var.index,,])
   return(var.i)
}


# plot residuals against predictors, yhat, time
pdf_name3 <- paste0("fig/res_plot.pdf")
pdf(pdf_name3, width = 10, height = 12)
par(mfrow=c(2,2))
gni.i <- getvar_i("gni_sm")
plot(st.res.i~ gni.i, col = getr.i, cex=1)
curve(predict(loess(st.res.i~gni.i),x), add = T, col = 2, lwd= 3)
abline(h=0)
#legend("topleft", legend=c("SDG region 1", "MDG region 2", "MDG region 3",
#                           "MDG region 4", "MDG region 5", "MDG region 6"),
#       col=c(1:6), pch=1, cex=0.8)

nmr.i <- getvar_i("nmr")
plot(st.res.i~nmr.i, col = getr.i,cex=1)
curve(predict(loess(st.res.i~nmr.i),x), add = T, col = 2, lwd= 3)
abline(h=0)
#legend("topleft", legend=c("SDG region 1", "MDG region 2", "MDG region 3",
#                           "MDG region 4", "MDG region 5", "MDG region 6"),
#       col=c(1:6), pch=1, cex=0.8)
dev.off()


