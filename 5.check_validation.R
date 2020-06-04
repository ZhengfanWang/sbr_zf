library(rstan)
## Read in countryRegionList info including MDG region, income region
source("1.iso.R")

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


fit <- readRDS(file = "rdsoutput/new/regHS_nval_res202000311.rds")
array <- rstan::extract(fit)
stan.data <- readRDS(file = "output/stan_data/hs_nval.rds")
percent <- t(apply(array$prep,2,quantile,probs = c(0.05,0.1,0.5,0.9,0.95)))
colnames(percent) <- c("ci05","ci1","yhat","ci9","ci95")
rv_data_raw <- data.frame(country_idx = stan.data$getc_i,
                          region_code = stan.data$getr_c[stan.data$getc_i],
                          year = stan.data$gett_i + 1999,
                          source = stan.data$getj_i,
                          y = stan.data$Y,
                          stdev = apply(array$sigma_i,2,median))
rv_data <- cbind(rv_data_raw,percent) %>%
  merge(countryRegionList, by="country_idx") %>% 
  mutate(residual = y - yhat,
         standardized_residual = residual/stdev)


getitest <- seq(1:stan.data$N)
ntest <- length(getitest)
round(c(
  sum(rv_data$y[getitest] < rv_data$ci05[getitest])/ntest,
  sum(rv_data$y[getitest] < rv_data$ci1[getitest])/ntest,
  sum(rv_data$y[getitest] > rv_data$ci9[getitest])/ntest,
  sum(rv_data$y[getitest] > rv_data$ci95[getitest])/ntest,
  mean(rv_data$residual[getitest]),
  mean(abs(rv_data$residual[getitest])),
  ntest
),digits = 4)

# create dataset

get_coverage <- function(rds.name, data.name){
  fit <- readRDS(file = paste0("rdsoutput/validation/",rds.name))
  array <- rstan::extract(fit)
  stan.data <- readRDS(file = paste0("output/stan_data/validation/",data.name))
percent <- t(apply(array$prep,2,quantile,probs = c(0.05,0.1,0.5,0.9,0.95)))
colnames(percent) <- c("ci05","ci1","yhat","ci9","ci95")
rv_data_raw <- data.frame(country_idx = stan.data$getc_i,
                          region_code = stan.data$getr_c[stan.data$getc_i],
                          year = stan.data$gett_i + 1999,
                          source = stan.data$getj_i,
                          y = stan.data$Y,
                          stdev = apply(array$sigma_i,2,median))
rv_data <- cbind(rv_data_raw,percent) %>%
           merge(countryRegionList, by="country_idx") %>% 
           mutate(residual = y - yhat,
                  standardized_residual = residual/stdev)


getitest <- setdiff(seq(1:stan.data$N),stan.data$getitrain_k)
ntest <- length(getitest)
return(c(
sum(rv_data$y[getitest] < rv_data$ci05[getitest])/ntest,
sum(rv_data$y[getitest] < rv_data$ci1[getitest])/ntest,
sum(rv_data$y[getitest] > rv_data$ci9[getitest])/ntest,
sum(rv_data$y[getitest] > rv_data$ci95[getitest])/ntest,
mean(sum(rv_data$residual[getitest])),
mean(sum(abs(rv_data$residual[getitest]))),
ntest
)
)
}

cov1 <- get_coverage(rds.name1,data.name1)
cov2 <- get_coverage(rds.name2,data.name2)
cov3 <- get_coverage(rds.name3,data.name3)
cov4 <- get_coverage(rds.name4,data.name4)
################## indices of test set
# getitest <- setdiff(seq(1,stan.data$N), stan.data$getitrain_k)
# ntest <- length(getitest)

# PIT
#pit.j <- rep(NA, ntest)
#for (j in 1:ntest){
#  i <- getitest[j]
#  yrepi.s <- array$prep[,i]
#  pit.j[j] <- mean(yrepi.s <= stan.data$Y[i])
#} 

################## indices of insample test set

getitest <- setdiff(seq(1:stan.data$N),stan.data$getitrain_k)
ntest <- length(getitest)

# PIT

pit.j <- rep(NA, ntest)
for (j in 1:ntest){
  i <- getitest[j]
  yrepi.s <- array$prep[,i]
  pit.j[j] <- mean(yrepi.s <= stan.data$Y[i])
} 

#create test set 
rv_data_test <- cbind(rv_data[getitest,],pit.j) 


rv_data_test %>% summarise(mean_residual = mean(residual),
                           median_residual = median(residual),
                           abs_error = mean(abs(residual)),
                           mean_standardize_residual = mean(standardized_residual)
                           )



hist(pit.j, freq = F, xlab = "PIT-values", main = "Predicting obs, should look uniform")  # should look uniform
abline(h=1)  

p <- 0.1
qbinom(c(0.1, 0.9), size = ntest, prob = p)/ntest # 80% PI for prop of left out obs in one bin of PIT values with range of p 
qbinom(c(0.25, 0.75), size = ntest, prob = p)/ntest # 50% PI for prop of left out obs in one bin of PIT values with range of p 
p <- 0.25
qbinom(c(0.1, 0.9), size = ntest, prob = p)/ntest # 80% PI for prop of left out obs in one bin of PIT values with range of p 
qbinom(c(0.25, 0.75), size = ntest, prob = p)/ntest # 50% PI for prop of left out obs in one bin of PIT values with range of p 

# coverage follows from pit
all<-
c(ntest,
mean(pit.j < 0.025), # % below 95% PI
mean(pit.j < 0.05), # % below 90% PI
mean(pit.j < 0.1),
mean(pit.j < 0.5),
mean(pit.j > 0.9), # % above 95% PI 
mean(pit.j > 0.95), 
mean(pit.j > 0.975))


pit_func <- function(source_type,pit,get_source){
  return(c(sum(get_source==source_type),
           mean(pit[get_source==source_type] < 0.025),
           mean(pit[get_source==source_type] < 0.05),
           mean(pit[get_source==source_type] < 0.1),
           mean(pit[get_source==source_type] < 0.5),
           mean(pit[get_source==source_type] > 0.9),
           mean(pit[get_source==source_type] > 0.95),
           mean(pit[get_source==source_type] > 0.975)))
}

admin <- round(pit_func(1,pit.j,stan.data$getj_i),digits = 4)
HMIS <- round(pit_func(2,pit.j,stan.data$getj_i),digits = 4)
lit_rev <- round(pit_func(3,pit.j,stan.data$getj_i),digits = 4)
survey <- round(pit_func(4,pit.j,stan.data$getj_i),digits = 4)

cache <- rbind(admin,HMIS,lit_rev,survey,all)
colnames(cache) <- c("N","pit<0.025","pit<0.05","pit<0.1","pit<0.5","pit>0.9","pit>0.95","pit>0.975")
cache
write.csv(cache,"output/pit_by_sourcetype.csv")

