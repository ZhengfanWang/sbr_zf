library(rstan)

## There are 4 validation runs locally. You need to use the model output and model data to create residual plots. 

#val 1
rds.name1 <- "hs_val_lao_res202000319.rds"
data.name1 <- "hs_val_lao.rds"
#val 2
rds.name2 <- "hs_val_ran20_res202000319.rds"
data.name2 <- "hs_val_ran20.rds"
#val 3
rds.name3 <- "hs_val_ran20v2_res202000319.rds"
data.name3 <- "hs_val_ran20_2.rds"
#val 4
rds.name4 <- "hs_val_ran20v3_res202000319.rds"
data.name4 <- "hs_val_ran20_3.rds"


fit <- readRDS(file = paste0("rdsoutput/validation/",rds.name1))
array <- rstan::extract(fit)
stan.data <- readRDS(file = paste0("output/stan_data/validation/",data.name1))

#fit <- readRDS(file = paste0("rdsoutput/validation/",rds.name4))
#array <- rstan::extract(fit)
#stan.data <- readRDS(file = paste0("output/stan_data/validation/",data.name4))


numcun <- max(stan.data$getc_i)
numyear <- max(stan.data$yearLength)
estyears <- seq(2000,2020)
#niter <- 12000

###############################################


error <- apply(array$prep,2,median)-stan.data$Y

stdev <- apply(array$prep, 2, sd)
getitest <- getitest <- setdiff(seq(1:stan.data$N),stan.data$getitrain_k)

ntest <- length(getitest)


# getvar_i function needs the int_cov vector to find the location of variable
int_cov <- c("gni_sm","nmr","lbw_sm","anc4_sm","mean_edu_f_sm",
             "gini_sm","urban","gfr","sab","anc1_sm","abr_sm",
             "csec_sm","pab_sm","pfpr","gdp","mmr")
#e.g


####
#   replicated code, better to function it
#####
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
int_5_mat <- cbind(cov5,inter_5)[getitest,]
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
int_11_mat <- cbind(cov11,inter_11)[getitest,]
int_11_name <- c(cov_11_name,inter_11_name)


res.data_cache <- data.frame(year = stan.data$gett_i+1999,
                       country_idx = stan.data$getc_i, 
                       error =  error, 
                       stdev = stdev)%>%
  merge(countryRegionList, by="country_idx")


ncoll <- ncol(res.data_cache)

res.data <- cbind(res.data_cache[getitest,],int_5_mat,int_11_mat)
#used for aggregate a few exercise
#res.data2<-cbind(res.data_cache[getitest,],int_5_mat,int_11_mat)
#res.data3<-cbind(res.data_cache[getitest,],int_5_mat,int_11_mat)
#res.data4<-cbind(res.data_cache[getitest,],int_5_mat,int_11_mat)
#res.data<- rbind(res.data2,res.data3,res.data4)

res.i <- res.data$error
sd.i <- res.data$stdev

hist(res.i)
st.res.i <- res.i/sd.i
length(st.res.i)

#sdgname <- c("Southern Asia",
#             "Sub-Saharan Africa",
#             "Northern America, Australia and New Zealand, Central Asia and Europe",
#             "Western Asia and Northern Africa ",
#             "Latin America and the Caribbean",
#             "Eastern Asia, South Eastern Asia and Oceania")

hist(st.res.i)



#resplot <- function(var_name){
#  var_i <- getvar_i(var_name)
#  p <- ggplot() + 
#    geom_point(aes(x = var_i,y = st.res.i,colour = region)) +
#    geom_smooth(aes(x = var_i,y = st.res.i,colour = region),method = "loess",span = 0.5,se = F) +
#    scale_x_continuous(name = ifelse(var_name %in% c("gni_sm","nmr"),paste("log",var_name),var_name)) +
#    scale_y_continuous(name = 'standardized residual') +
#    theme(legend.position="bottom")
#  return(p)
#}

resplot2 <- function(var_i,xname){
  p <- ggplot() + 
    geom_point(aes(x = var_i,y = st.res.i,colour = res.data$sdg_name)) +
    geom_smooth(aes(x = var_i,y = st.res.i,colour = res.data$sdg_name),method = "loess",se = F) +
    scale_x_continuous(name = xname) +
    scale_y_continuous(name = 'standardized residual') +
    theme(legend.position="bottom")
  
  return(p)
}


# plot residuals against predictors, yhat, time
pdf_name3 <- paste0("fig/RES_first5cov_overall_v2.pdf")
pdf(pdf_name3, width = 15, height = 12)
for(i in 1:15){
  print(resplot2(res.data[,i+ncoll],int_5_name[i]))
}

dev.off()

pdf_name3 <- paste0("fig/RES_last11cov_overall_v2.pdf")
pdf(pdf_name3, width = 15, height = 12)
for(i in 1:66){
  print(resplot2(res.data[,i+ncoll],int_11_name[i]))
}
dev.off()
