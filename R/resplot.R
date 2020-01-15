getvar_i <- function(varname,interest_cov = intcov){
  var.index <- which(varname == int_cov) 
  var.i <- x.i(stan.data$covar_array[var.index,,])
  return(var.i)
}

x.i<-function(varmatrix){
  varname.i<-rep(NA,stan.data$N)
  for (i in 1:stan.data$N) {varname.i[i]<-varmatrix[getc.i[i],gett.i[i]]}
  varname.i
}