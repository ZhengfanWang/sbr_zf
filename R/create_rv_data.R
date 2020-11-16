create_rv_data <- function(array,standata){
  country_code <- standata$getc_i
  region_code <- standata$getr_c[standata$getc_i]
  year <- standata$gett_i + 1999
  source <- standata$getj_i
  y <- standata$Y
  predictor <- matrix(NA,ncol = standata$numcov, nrow = standata$N)
  for(cov in 1:standata$numcov){
    for(k in 1:standata$ntrain){
      predictor[k,cov] <- standata$covar_array[cov,
                                               standata$getc_i[standata$getitrain_k[k]],
                                               standata$gett_i[standata$getitrain_k[k]]]
    }}
  predictor <- as.data.frame(predictor)
  
  colnames(predictor) <- c("gni_sm","nmr","lbw_sm","anc4_sm","mean_edu_f_sm",
                           "gini_sm","urban","gfr","sab","anc1_sm","abr_sm",
                           "csec_sm","pab_sm","pfpr","gdp","mmr") #covariates name. need to change if we select different covariates.
  
  total_sd <- apply(array$sigma_i,2,median)
  percent <- t(apply(array$prep,2,quantile,probs = c(0.025,0.1,0.5,0.9,0.975)))
  
  rv_data_raw <- data.frame(country_code = standata$getc_i,
                            region_code = standata$getr_c[standata$getc_i],
                            year = standata$gett_i + 1999,
                            source = standata$getj_i,
                            y = standata$Y,
                            total_sd = apply(array$sigma_i,2,median))
  return(rv_data <- cbind(rv_data_raw,predictor,percent))
}

