cal_pit <- function(array,standata){
  getitest <- setdiff(seq(1,standata$N), standata$getitrain_k)
  ntest <- length(getitest)
  # PIT
  pit.j <- rep(NA, ntest)
  for (j in 1:ntest){
    i <- getitest[j]
    yrepi.s <- array$prep[,i]
    pit.j[j] <- mean(yrepi.s <= standata$Y[i])
  } 
  return(pit.j)
}