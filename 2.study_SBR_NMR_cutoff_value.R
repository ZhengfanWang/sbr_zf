###  SBR:NMR cutoff value study


# load high quality lmin data
hq_data <- openxlsx::read.xlsx("input/High Quality LMIC data_SBR_NMR.xlsx")
full_data <- readRDS("output/sbr.full.rds")

############################# MC method to explore cut off ######################


#-------------------------------#
#     1-step: explore hq data   #
#-------------------------------#
tb <- hq_data$lb + hq_data$sb
sbr <- hq_data$sbr
lb <- hq_data$lb
nmr <- hq_data$nmr

set.seed(123)
n <- length(tb)
S <- 1000  #num of MC simulation
log.r_s <- matrix(NA,ncol=S,nrow=n)
for(i in 1:n){
  sb_s <- rbinom(S,tb[i],sbr[i]/1000)
  nm_s <- rbinom(S,lb[i],nmr[i]/1000)
  log.r_s[i,] <- log((sb_s+0.5)/(tb[i]+1)*1/((nm_s+0.5)/(lb[i])+1))
}
sd_i <- apply(log.r_s,1,sd)

stan.data.cutoff <- list(n = n, y_i = log(sbr/nmr), sd_i = sd_i)
library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
fit <- stan(file= "mod/study_ratio_cutoff.stan",data=stan.data.cutoff,chains = 4,
     control=list(adapt_delta=0.99, max_treedepth=15))

print(fit,pars = c("mu","sigma"))
fit.chain <- rstan::extract(fit)
mu <- fit.chain$mu                  # posterior sample of overall mean of log ratio
sigma  <- fit.chain$sigma           # posterior sample of variability across settings.

mu.hat <- mean(mu)
delta.hat.sq <- var(mu)
sigma.hat.sq <- mean(sigma)^2

#----------------------------------------#
#   2-step: Make decision for full set   #
#----------------------------------------#

ftb <- full_data$nLB + full_data$nSB
fsbr <- full_data$adj_sbr_unknown
flb <- full_data$nLB
fnmr_obs <- full_data$NMR
fnmr_un <- full_data$UN_NMR

fn <- length(ftb)
S <- 1000  #num of MC simulation
flog.r_s_obs <- matrix(NA,ncol=S,nrow=fn)
flog.r_s_un <- matrix(NA,ncol=S,nrow=fn)
for(i in 1:fn){
  sb_s <- rbinom(S,ftb[i],fsbr[i]/1000)
  nm_s_obs <- rbinom(S,flb[i],fnmr_obs[i]/1000)
  nm_s_un <- rbinom(S,flb[i],fnmr_un[i]/1000)
  flog.r_s_obs[i,] <- log((sb_s+0.5)/(ftb[i]+1)*1/((nm_s_obs+0.5)/(flb[i])+1))
  flog.r_s_un[i,] <- log((sb_s+0.5)/(ftb[i]+1)*1/((nm_s_un+0.5)/(flb[i])+1))
}
v_i_obs <- apply(flog.r_s_obs,1,var)
v_i_un <- apply(flog.r_s_un,1,var)
v_i_obs[is.na(v_i_obs)] <- 0
v_i_un[is.na(v_i_un)] <- 0
v_i <- ifelse(is.na(v_i_obs),v_i_un,v_i_obs)

sigma <- sqrt(delta.hat.sq + sigma.hat.sq)
sigma_i_obs <- sqrt(delta.hat.sq + sigma.hat.sq + v_i_obs)
sigma_i_un <- sqrt(delta.hat.sq + sigma.hat.sq + v_i_un)
sigma_i <- sqrt(delta.hat.sq + sigma.hat.sq + v_i)

log.ratio_i_obs <- log(full_data$rSN)
log.ratio_i_un <- log(full_data$rSN_UN)
log.ratio_i <- log(ifelse(is.na(full_data$rSN),full_data$rSN_UN,full_data$rSN))

prob_i_obs <- unlist(map2(log.ratio_i_obs,sigma_i_obs,pnorm,mean = mu.hat))
prob_i_un <- unlist(map2(log.ratio_i_un,sigma_i_un,pnorm,mean = mu.hat))
prob_i <- unlist(map2(log.ratio_i,sigma_i,pnorm,mean = mu.hat))

hist(prob_i_obs,freq = FALSE, breaks = 20)
hist(prob_i_un,freq = FALSE, breaks = 20)

cutoff_bound <- exp(qnorm(0.05,mu.hat,sigma))
round(cutoff_bound,digits = 2)
mean(prob_i_obs<0.05,na.rm = T)
mean(prob_i_un<0.05,na.rm = T)
SBR.full.ratio <- full_data %>% rename(exclude_based_on_obssbrnmr_ratio = exclusion_ratio) %>% 
                                mutate(prob_based_on_obssbrnmr = round(prob_i_obs,digit=4),
                                       v_based_on_obssbrnmr = v_i_obs,
                                       prob_based_on_sbrunnmr = round(prob_i_un,digit=4),
                                       v_based_on_sbrunnmr = v_i_un,
                                       exclude_based_on_obssbrnmr_ratio = replace(exclude_based_on_obssbrnmr_ratio,
                                                                                  prob_i_obs <= 0.05&definition_rv == "ge28wks",
                                                                                  "TRUE"),
                                       exclude_based_on_obssbrnmr_ratio = replace(exclude_based_on_obssbrnmr_ratio,
                                                                                  prob_i_obs > 0.05&definition_rv == "ge28wks",
                                                                                  "FALSE")
                                       #,
                                       #exclude_based_on_obssbrnmr_ratio = replace(exclude_based_on_obssbrnmr_ratio,
                                       #                                            is.na(prob_i_obs),
                                       #                                            "missing obs NMR")
                                       ) %>% 
                                mutate(exclude_based_on_sbrUNnmr_ratio = NA) %>% 
                                mutate(exclude_based_on_sbrUNnmr_ratio = replace(exclude_based_on_sbrUNnmr_ratio,
                                                                                 prob_i_un <= 0.05&definition_rv == "ge28wks",
                                                                                 "TRUE"),
                                       exclude_based_on_sbrUNnmr_ratio = replace(exclude_based_on_sbrUNnmr_ratio,
                                                                                 prob_i_un > 0.05&definition_rv == "ge28wks",
                                                                                 "FALSE")
                                       #,
                                       #exclude_based_on_sbrUNnmr_ratio = replace(exclude_based_on_sbrUNnmr_ratio,
                                       #                                            is.na(prob_i_un),
                                       #                                            "missing UN NMR")
                                       ) %>% 
                                mutate(exclusion_ratio = NA) %>% 
                                mutate(exclusion_ratio = replace(exclusion_ratio,
                                                                 is.na(prob_i),
                                                                 "missing nmr and un nmr")) %>% 
                                mutate(exclusion_ratio = replace(exclusion_ratio,
                                                                 prob_i<0.05 & definition_rv == "ge28wks",
                                                                 "prob < 0.05 and 28wks def")) 
#table(SBR.full.ratio$exclusion_ratio)
write.csv(SBR.full.ratio,"output/fullset.csv")
saveRDS(SBR.full.ratio,"output/fullset.rds")
################################################################################################

#--------------------------------------#
#   exploratory plot after exclusion   # 
#--------------------------------------#
SBR.clean <- SBR.full.ratio %>% filter(is.na(exclusion_notes),is.na(exclusion_ratio))
clean_data_list <- create_list_for_country(SBR.clean)
pdf_name <- paste0("fig/exploratory_plot/exploratory_clean_data.pdf")
pdf(pdf_name,width=12)
clean_data_list %>% lapply(exploratory_plot)
dev.off()



#percentile2 <- apply(ratio_s,1,quantile,na.rm = TRUE,c(0.01,0.025,0.05,0.075,0.1,0.15,0.2))
#t(round(apply(percentile2,1,quantile,na.rm=TRUE,c(0,0.025,0.05,0.10,0.15,0.5)),digits = 2))
#title <- c("1%","2.5%","5%","7.5%","10%","15%","20%")
#par(mfrow=c(3,3))
#for(i in 1:7){
#  histplot(percentile2[i,],breaks = 5, title = title[i], xlim = c(0,2),ylim = c(0,0.5))
#}

################################################################################

#             OOOOOOOOOOOOOOOOOOOOOOOOLD CODE                #
############################## Spline method to find cut off

if(FALSE){
dim(hq_data)

range.nmr <- range(hq_data$nmr)
Y <- hq_data$sbr_nmr_ratio
X <- hq_data$nmr
X.pred <- seq(range.nmr[1],range.nmr[2], 0.1)
numx <- length(X.pred)
B <- t(bs(X, knots = seq(range.nmr[1],range.nmr[2], 3), degree=3, intercept = TRUE)) # creating the B-splines
x.grid<-seq(from=range.nmr[1], to = range.nmr[2])
num_data <- length(X);
num_basis <- nrow(B)

stan.data.cutoff <- list(Y = Y, X = X, B=B , num_data = num_data , num_basis = num_basis)
library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

fit<- rstan::stan_model(file= "mod/study_ratio_cutoff.stan")

fit_spline<-sampling(fit,iter=500,data = stan.data.cutoff,control = list(adapt_delta=0.95))
fit_spline

fitr <- lm(Y ~ bs(X,knots = c(10,20,30,40,50,60)))
summary(fitr)

plot(X,Y,col="black",xlab="nmr",ylab="SBR:NMR",ylim = c(0,1.6))
predict(fitr,newdata = list(X=x.grid),interval = "predict")
points(x.grid,predict(fitr,newdata = list(X=x.grid)),col="red",lwd=2,type="l")
points(x.grid,predict(fitr,newdata = list(X=x.grid),interval = "predict")[,2],col="red",lwd=2,type="l",lty=2)
points(x.grid,predict(fitr,newdata = list(X=x.grid),interval = "predict")[,3],col="red",lwd=2,type="l",lty=2)
abline(h=0.33)
text(58, 0.35, "0.33", pos=2) 
abline(h=0.5)
text(58, 0.525,"0.5", pos = 2)
#adding cutpoints
abline(v=c(10,20,30,40,50,60),lty=2,col="darkgreen")
}
