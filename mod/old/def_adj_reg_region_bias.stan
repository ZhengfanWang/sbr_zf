data{
  int<lower=1> N;
  real y[N];
  int<lower=0,upper=195> getc_i[N];
  int<lower=0,upper=3> getr_c[195];
 // int<lower=0,upper=2> getj_i[N];
  real x[N];
  real sei[N];
  real sbr28[N];
 // real nmr[N];
}
parameters{
  real alpha[2];
  real<lower=0,upper=20> sigma[2];
}


model{
  alpha ~ normal(0, 10);
  for(i in 1:N){
    y[i] ~ normal(x[i] + alpha[getr_c[getc_i[i]]] , sqrt(square(sei[i]) + square(sigma[getr_c[getc_i[i]]])) );
  }
}


generated quantities {
  vector[N] yhat;
  for(n in 1:N)
    yhat[n] = normal_rng(x[n] + alpha[getr_c[getc_i[n]]] , sqrt(square(sei[n]) + square(sigma[getr_c[getc_i[n]]])) );
}

