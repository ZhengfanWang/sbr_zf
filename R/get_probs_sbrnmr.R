get_probs_sbrnmr <- function(ftb, fsbr, fnmr, flb, params_cutoff){
  v_i <- getsd_logratio(ftb, fsbr, fnmr, flb)^2
  # set variances equal to zero if missing
  v_i[is.na(v_i)] <- 0
  sigma_i <- sqrt(params_cutoff$delta.hat.sq + params_cutoff$sigma.hat.sq + v_i)
  log.ratio_i <- log(fsbr/fnmr)
  unlist(map2(log.ratio_i, sigma_i, pnorm, mean = params_cutoff$mu.hat)) # returns the probs
}