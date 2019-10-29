data{
  int<lower = 0> n; //num of hq obs
  vector[n] y_i; // log(sbr/nmr)
  vector[n] sd_i; //sd_i of log(sbr/nmr)_i
}

parameters{
  vector[n] theta_tilde;  // random effect
  real mu;            // overall mean
  real<lower = 0, upper = 10> sigma;         //sigma^2 variability across observations 
}

transformed parameters{
  vector[n] theta;       // random effect uncentered para
  theta = mu + theta_tilde*sigma;
}

model{
  theta_tilde ~ normal(0,1);
  y_i ~ normal(theta,sd_i);
}
