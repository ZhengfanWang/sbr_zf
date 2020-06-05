
library(rstan)

fit <- readRDS(file = "rdsoutput/new/regHS_nval_res202000311.rds")
stan.data <- readRDS(file = "output/stan_data/hs_nval.rds")

array <- rstan::extract(fit)

  numcun <- max(stan.data$getc_i)
numyear <- max(stan.data$yearLength)
estyears <- seq(2000,2020)
niter <- 12000


###############################################

error <- apply(array$prep,2,median)-stan.data$Y

stdev <- apply(array$prep, 2, sd)

getitest <- stan.data$getitrain_k
#getitest <- setdiff(seq(1,stan.data$N), stan.data$getitrain_k)
ntest <- length(getitest)

res.data <- data.frame(year = stan.data$gett_i+1999,
                       country_idx = stan.data$getc_i, 
                       error =  error, 
                       stdev = stdev)%>%
               merge(countryRegionList, by="country_idx")
res.data<- res.data[getitest,]
res.i <- res.data$error
sd.i <- res.data$stdev

hist(res.i)
st.res.i <- res.i/sd.i
length(st.res.i)

sdgname <- c("Southern Asia",
             "Sub-Saharan Africa",
             "Northern America, Australia and New Zealand, Central Asia and Europe",
             "Western Asia and Northern Africa ",
             "Latin America and the Caribbean",
             "Eastern Asia, South Eastern Asia and Oceania")

hist(st.res.i)
##individual observation function
getc.i <- stan.data$getc_i[getitest]
gett.i <- stan.data$gett_i[getitest]
getr.c <- stan.data$getr_c
getj.i <- stan.data$getj_i[getitest]
getd.i <- stan.data$getd_i[getitest]
getr.i <- getr.c[stan.data$getc_i[getitest]]
region <- sdgname[getr.i]

# getvar_i function needs the int_cov vector to find the location of variable
int_cov <- c("gni_sm","nmr","lbw_sm","anc4_sm","mean_edu_f_sm",
             "gini_sm","urban","gfr","sab","anc1_sm","abr_sm",
             "csec_sm","pab_sm","pfpr","gdp","mmr")
#e.g



#### first 5 important covariates
cov_5_name <- c("gni_sm","nmr","lbw_sm","anc4_sm","mean_edu_f_sm")
cov5 <- sapply(cov_5_name,getvar_i)
inter_5 <- c()
inter_5_name <- c()
for(i in 1:4){
  for(j in ((i+1):5)){
   cache <- cov5[,i]*cov5[,j]
   inter_5 <- cbind(inter_5,cache)
   cache_name <- paste0(cov_5_name[i],"*",cov_5_name[j])
   inter_5_name <- c(inter_5_name,cache_name)
  }
}
colnames(inter_5) <- inter_5_name
int_5_mat <- cbind(cov5,inter_5)
int_5_name <- c(cov_5_name,inter_5_name)

### 11 unimportant covariates
cov_11_name <- c("gini_sm","urban","gfr","sab","anc1_sm","abr_sm",
                "csec_sm","pab_sm","pfpr","gdp","mmr")
cov11 <- sapply(cov_11_name,getvar_i)
inter_11 <- c()
inter_11_name <- c()
for(i in 1:10){
  for(j in ((i+1):11)){
    cache <- cov11[,i]*cov11[,j]
    inter_11 <- cbind(inter_11,cache)
    cache_name <- paste0(cov_11_name[i],"*",cov_11_name[j])
    inter_11_name <- c(inter_11_name,cache_name)
  }
}
colnames(inter_11) <- inter_11_name
int_11_mat <- cbind(cov11,inter_11)
int_11_name <- c(cov_11_name,inter_11_name)



resplot <- function(var_name){
  var_i <- getvar_i(var_name)
  p <- ggplot() + 
       geom_point(aes(x = var_i,y = st.res.i,colour = region)) +
       geom_smooth(aes(x = var_i,y = st.res.i,colour = region),method = "loess",span = 0.5,se = F) +
       scale_x_continuous(name = ifelse(var_name %in% c("gni_sm","nmr"),paste("log",var_name),var_name)) +
       scale_y_continuous(name = 'standardized residual') +
       theme(legend.position="bottom")
       
  return(p)
}

resplot2 <- function(var_i,xname){
  p <- ggplot() + 
    geom_point(aes(x = var_i,y = st.res.i,colour = region)) +
    geom_smooth(aes(x = var_i,y = st.res.i,colour = region),method = "loess",span = 0.5,se = F) +
    scale_x_continuous(name = xname) +
    scale_y_continuous(name = 'standardized residual') +
    theme(legend.position="bottom")
  
  return(p)
}


# plot residuals against predictors, yhat, time
pdf_name3 <- paste0("fig/RES_first5cov.pdf")
pdf(pdf_name3, width = 15, height = 12)
for(i in 1:15){
print(resplot2(int_5_mat[,i],int_5_name[i]))
}

dev.off()

pdf_name3 <- paste0("fig/RES_last11cov.pdf")
pdf(pdf_name3, width = 15, height = 12)
for(i in 1:66){
  print(resplot2(int_11_mat[,i],int_11_name[i]))
}
dev.off()
