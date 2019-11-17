# la: added a function, that outputs SD for logratio based on some inputs
getsd_logratio <- function(tb, # numeric vector with total births
                           sbr, # numeric vector with sbr
                           nmr, # numeric vector with nmr
                           lb # numeric vector with live births
){
  set.seed(123)
  n <- length(tb)
  S <- 1000  #num of MC simulation
  log.r_s <- matrix(NA, ncol=S, nrow=n)
  for(i in 1:n){
    sb_s <- rbinom(S,tb[i],sbr[i]/1000)
    nm_s <- rbinom(S,lb[i],nmr[i]/1000)
    log.r_s[i,] <- log((sb_s+0.5)/(tb[i]+1)*1/((nm_s+0.5)/(lb[i])+1))
  }  
  apply(log.r_s, 1, sd)
}