data{
  int<lower=1> N;
  real y[N];
  int<lower=0,upper=195> getc_i[N];
  int<lower=0,upper=3> getr_c[195];
  int<lower=0,upper=2> getj_i[N];
  real x[N];
  real sei[N];
  real sbr28[N];
//  real nmr[N];
}
 parameters{
   real alpha;
   real<lower=0,upper=20> sigma;
 }
 
 model{
   
   alpha ~ normal(0, 10);

   for(i in 1:N){
     y[i] ~ normal(x[i] + alpha ,sqrt(square(sei[i])+square(sigma)));
   }
 }
 
 
 
 generated quantities {
  vector[N] yhat;
  for(n in 1:N)
    yhat[n] = normal_rng(x[n] + alpha ,sqrt(square(sei[n])+square(sigma)));
}


