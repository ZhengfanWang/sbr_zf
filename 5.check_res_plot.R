
library(rstan)

fit <- readRDS(file = "rdsoutput/regHS_nval_res.rds")
stan.data <- readRDS(file = "output/stan_data/hs_nval.rds")
smooth <- T

array <- rstan::extract(fit)

  
numcun <- max(stan.data$getc_i)
numyear <- max(stan.data$yearLength)
estyears <- seq(2000,2020)

if(smooth == T){
  est <- array$prep}else if(smooth == F){
    est <- array(dim = c(4000,stan.data$ntrain))
    for(i in 1:stan.data$ntrain){
      est[,i] <-  array$prep[,i] - 
        array$delta_ct[,getc.i[i],gett.i[i]] +
        array$gamma_c[,getc.i[i]]
    }    
  }




###############################################


sd <- apply(array$sigma_i,2,median)
error <- apply(est,2,median)-stan.data$Y

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

sdgname <- c("Southern Asia",
             "Sub-Saharan Africa",
             "Northern America, Australia and New Zealand, Central Asia and Europe",
             "Western Asia and Northern Africa ",
             "Latin America and the Caribbean",
             "Eastern Asia, South Eastern Asia and Oceania")

hist(st.res.i)
##individual observation function
getc.i <- stan.data$getc_i
gett.i <- stan.data$gett_i
getr.c <- stan.data$getr_c
getj.i <- stan.data$getj_i
getd.i <- stan.data$getd_i
getr.i <- getr.c[getc.i]
region <- sdgname[getr.i]

# getvar_i function needs the int_cov vector to find the location of variable
int_cov <- c("gni_sm","nmr","lbw_sm","anc4_sm","mean_edu_f_sm",
             "gini_sm","urban","gfr","sab","anc1_sm","abr_sm",
             "csec_sm","pab_sm","pfpr","gdp","mmr")
#e.g
nmr.i <- getvar_i("nmr",interest_cov = intcov)


resplot <- function(var_name){
  var_i <- getvar_i(var_name)
  p <- ggplot() + 
       geom_point(aes(x = var_i,y = st.res.i,colour = region)) +
       geom_smooth(aes(x = var_i,y = st.res.i),method = "loess",se = F) +
       scale_x_continuous(name = ifelse(var_name %in% c("gni_sm","nmr"),paste("log",var_name),var_name)) +
       scale_y_continuous(name = 'standardized residual') +
       theme(legend.position="bottom")
       
  return(p)
}


# plot residuals against predictors, yhat, time
pdf_name3 <- paste0("fig/res_plot_smooth.pdf")
pdf(pdf_name3, width = 15, height = 12)

int_cov %>% lapply(resplot)

dev.off()


