data.nval <- readRDS(file = "output/stan_data/hs_nval.rds")     #stan data used for fit model



base <- readRDS("rdsoutput/new/base.rds")
hs <- readRDS("rdsoutput/new/tun1.rds")
hs_lambda_df3 <- readRDS("rdsoutput/new/lambda_df3.rds")
hs_tau_sigma_j1 <- readRDS("rdsoutput/new/tau_sigma_j1.rds")

print(base,pars = c("beta","bias_dt","sigma_j","gamma_r","tau_delta"))
print(hs,pars = c("beta","bias_dt","sigma_j","gamma_r","tau_delta"))
print(hs_lambda_df3,pars = c("beta","bias_dt","sigma_j","gamma_r","tau_delta"))
print(hs_tau_sigma_j1,pars = c("beta","bias_dt","sigma_j","gamma_r","tau_delta"))

print(base,pars = c("beta"))
print(hs,pars = c("beta"))
print(hs_lambda_df3,pars = c("beta"))
print(hs_tau_sigma_j1,pars = c("beta"))

base.array<- rstan::extract(base)
hs.array<- rstan::extract(hs)
lambda.array<- rstan::extract(hs_lambda_df3)
tau.array<- rstan::extract(hs_tau_sigma_j1)

plot_cov <- function(model,model2,model3,model4,cov){
  int_cov <- c("log_gni_sm","log_nmr","log_lbw_sm","anc4_sm","mean_edu_f_sm",
               "gini_sm","urban","gfr","sab","anc1_sm","abr_sm",
               "csec_sm","pab_sm","pfpr","gdp","mmr")
  ymax <- max(c(max(density(model$beta[,cov])$y),
                max(density(model2$beta[,cov])$y),
                max(density(model3$beta[,cov])$y),
                max(density(model4$beta[,cov])$y)))
  xlim <- range(density(model4$beta[,cov])$x)
  plot(0,xlab = "", ylab="",type = "n",main = paste0(int_cov[cov]),
       xlim=c(xlim[1], xlim[2]), ylim=c(0, ymax))
  lines(density(model$beta[,cov]),type="l",lty=1,col = "black")
  lines(density(model2$beta[,cov]),type="l",lty=1,col="red")
  lines(density(model3$beta[,cov]),type="l",lty=1,col="blue")
  lines(density(model4$beta[,cov]),type="l",lty=1,col="green")
  legend("topleft",legend = c("base","cauchy0,1","lambda_df3","tau_j1"),col=c("black","red","blue",'green'),lty=1)}


plot_lambda <- function(model2,model3,model4,cov){
  int_cov <- c("log_gni_sm","log_nmr","log_lbw_sm","anc4_sm","mean_edu_f_sm",
               "gini_sm","urban","gfr","sab","anc1_sm","abr_sm",
               "csec_sm","pab_sm","pfpr","gdp","mmr")
  ymax <- max(c(max(density(model2$lambda[,cov])$y),
                max(density(model3$lambda[,cov])$y),
                max(density(model4$lambda[,cov])$y)))
  plot(0,xlab = "", ylab="",type = "n",main = paste0("lambda_",int_cov[cov]),
       xlim=c(0,8), ylim=c(0, ymax))
  lines(density(model2$lambda[,cov]),type="l",lty=1,col="red")
  lines(density(model3$lambda[,cov]),type="l",lty=1,col="blue")
  lines(density(model4$lambda[,cov]),type="l",lty=1,col="green")
  legend("topright",legend = c("cauchy0,1","lambda_df3","tau_j1"),col=c("red","blue",'green'),lty=1)}


plot_K <- function(model2,model3,model4,cov,sd){
  int_cov <- c("log_gni_sm","log_nmr","log_lbw_sm","anc4_sm","mean_edu_f_sm",
               "gini_sm","urban","gfr","sab","anc1_sm","abr_sm",
               "csec_sm","pab_sm","pfpr","gdp","mmr")
  k1 <- 1/(1+data.nval$N*model2$lambda[,cov]^2*model2$tau^2)
  k2 <- 1/(1+data.nval$N*model3$lambda[,cov]^2*model3$tau^2)
  k3 <- 1/(1+data.nval$N*model4$lambda[,cov]^2*model4$tau^2)
  
  ymax <- max(c(max(density(k1)$y),
                max(density(k2)$y),
                max(density(k3)$y)))
  plot(0,xlab = "", ylab="",type = "n",main = paste0("K_",int_cov[cov]),
       xlim=c(0,1), ylim=c(0, ymax))
  lines(density(k1),type="l",lty=1,col="red")
  lines(density(k2),type="l",lty=1,col="blue")
  lines(density(k3),type="l",lty=1,col="green")
  legend("topright",legend = c("cauchy0,1","lambda_df3","tau_j1"),col=c("red","blue",'green'),lty=1)}



pdf_name <- paste0("fig/covariates.pdf")
pdf(pdf_name, width = 8, height = 5)
par(mfrow=c(1,3))
for(i in 1:16){
  plot_cov(base.array,hs.array,lambda.array,tau.array,i)
  plot_lambda(hs.array,lambda.array,tau.array,i)
  plot_K(hs.array,lambda.array,tau.array,i)
}
dev.off()

model <- lambda.array
sd <- "min"
cov <- 1
calculate_k <- function(model,cov,sd,niter=4000){
  int_cov <- c("log_gni_sm","log_nmr","log_lbw_sm","anc4_sm","mean_edu_f_sm",
               "gini_sm","urban","gfr","sab","anc1_sm","abr_sm",
               "csec_sm","pab_sm","pfpr","gdp","mmr")
  if(sd=="min"){stdv <- min(apply(model$sigma_i,2,median))
  }else if(sd=="max"){stdv <- max(apply(model$sigma_i,2,median))
  }else if(sd=="median"){stdv <- median(apply(model$sigma_i,2,median))
  }
  k <- 1/(1 + 
          data.nval$N*
          stdv^(-2)*
          model$lambda[,cov]^2*
          model$tau^2)
  sum(k>0.5)/4000
}

result <- NA
for(i in 1:16){
  if(calculate_k(lambda.array,i,sd="median")>0.5)(result[i] <- "out") else(result[i] <- "in")
}
result
