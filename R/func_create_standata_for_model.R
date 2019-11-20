#function to create standata for model

covarMatrix <-function (covar,
                        estyears,
                        dataset=covarset){
  yearLength <- length(estyears)
  numcoun<- max(countryRegionList$country_idx)
  cMatrix<- matrix(ncol=yearLength,nrow=numcoun)
  for (i in 1:numcoun){
    for ( j in 1: yearLength){
      cMatrix[i,j] <- as.numeric(dataset[dataset$iso == countryRegionList$iso[i] & dataset$year== estyears[j],covar])
    }
  }
  return (cMatrix)
}

standardize <- function(x){
  return((x-mean(x))/sd(x))
}

create_covar_array <- function(dataset = covarset , interest_cov,estyears){
  ncovar <- length(interest_cov)
  numcoun <- max(countryRegionList$country_idx)
  yearLength <- length(estyears)
  covararray <- array(NA, dim = c(ncovar,numcoun,yearLength))
  
  for(i in 1:ncovar){
    covararray[i,,] <- standardize(covarMatrix(interest_cov[i],estyears = estyears,dataset = covarset))
  }
  return(covararray)
}
