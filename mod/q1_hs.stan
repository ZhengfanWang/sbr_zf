data{
  int<lower=0> N; //number of observations
  int<lower=0> numcov; //number of preidctors
  int<lower=0> numcountry; // number of countries
  int<lower=0> numregion; // number of regions
  int<lower=0> numdef; //number of definitions
  int<lower=0> numsource; // num of source type
  int<lower=0> yearLength; //number of est year
  int<lower=0> ntrain;  //loo 
  int estyears[yearLength]; //vector of est years;
  real Y[N]; // vector of log sbr
  int<lower=0,upper=numcountry> getc_i[N];          // country for given obs
  int<lower=0,upper=numregion> getr_c[numcountry];  // vector of region given country
  int<lower=1,upper=yearLength> gett_i[N];       // time for given obs
  int<lower=0,upper=numdef> getd_i[N];                // definition type for given obs
  int<lower=0> getitrain_k[ntrain];              // loo or not
  
  //source type indicator variable
  int <lower=0,upper=1> datatype1_i[N];        // admin
  int <lower=0,upper=1> datatype2_i[N];        // hmis 
  int <lower=0,upper=1> datatype3_i[N];        //subnat.admin
  int <lower=0,upper=1> datatype4_i[N];        //subnat.lr
  int <lower=0,upper=1> datatype5_i[N];        //survey
  
  //def type indicator variable
  int <lower=0,upper=1> deftype1_i[N];
  int <lower=0,upper=1> deftype2_i[N];
  int <lower=0,upper=1> deftype3_i[N];
  int <lower=0,upper=1> deftype4_i[N];

  real covar_array[numcov,numcountry,yearLength];   //coariates array: super slow!!!!!!!
   
  vector[numdef] eta_d;               // definition adjustment bias
  real<lower=0> phi_d[numdef];               // definition adjustment var

  real<lower=0> var_i[N];              // sampling error^2

  //input data  spline
  int<lower=0> K;                  //number of basis
  int<lower=0> H;
  matrix[yearLength,H] Z_th;
}


transformed data{
  real slab_scale = 3;
  real nu_local = 1;
  real nu_global =1;
  real scale_global = 0.333;
  real slab_df = 3;
  //def bias
  vector[N] bias_def_i;
  for(i in 1:N){
    bias_def_i[i] = eta_d[1]*deftype1_i[i]+
                    eta_d[2]*deftype2_i[i]+
                    eta_d[3]*deftype3_i[i]+
                    eta_d[4]*deftype4_i[i];}
}

parameters {
  
  //mean part
  //hs prior
  real<lower=0> tau;
  vector<lower=0>[numcov] lambda;
  real<lower=0> caux;
  vector[numcov] beta_tilde;
  
  //deviance part
  vector[numsource-1] bias_dt_tilde;

  real<lower=0,upper=5> sigma_j[numsource-1];

  real<lower=0,upper=5> sigma_c;

  //parameters: P spline 2 order
  real<lower=0,upper=3> tau_delta;      // sd for spline coefficients
  vector[numregion] gamma_r;
  vector[numcountry] gamma_c;
  matrix[H,numcountry] delta_hc;
}

transformed parameters {
  //hs prior
  vector[numcov] beta;
  vector<lower=0>[numcov] lambda_tilde;
  real<lower=0> cc;
  vector[numsource-1] bias_dt = bias_dt_tilde *5;
  matrix[numcountry,yearLength] mu_ct;
  real bias_dt_i[N];
  real sigma_i[N];
  matrix[numcountry,yearLength] delta_ct;
  real<lower=0> var_j[numsource-1];
  
   cc = slab_scale * sqrt(caux);
   lambda_tilde = sqrt(cc^2 * square(lambda) ./ (cc^2 + tau^2 *square(lambda)));
   beta = beta_tilde .* lambda_tilde * tau;
  for(j in 1:(numsource-1)){
    var_j[j]= square(sigma_j[j]);}
  
  for(c in 1:numcountry){
    for(t in 1:yearLength){
      mu_ct[c,t] = to_row_vector(covar_array[,c,t]) * beta;
    }}
  
  for(i in 1:N){
    bias_dt_i[i] = bias_dt[1]*datatype2_i[i]+
                   bias_dt[2]*datatype3_i[i]+
                   bias_dt[3]*datatype4_i[i]+
                   bias_dt[4]*datatype5_i[i];

    sigma_i[i] = sqrt(  0.0025*datatype1_i[i]+
                      var_j[1]*datatype2_i[i]+
                      var_j[2]*datatype3_i[i]+
                      var_j[3]*datatype4_i[i]+
                      var_j[4]*datatype5_i[i]+
                      var_i[i] + phi_d[getd_i[i]]);
  }
  for(c in 1:numcountry){
    for(t in 1:yearLength){
      delta_ct[c,t] = gamma_c[c]+
        dot_product(Z_th[t,],delta_hc[,c]);
    }}
}


model {
  // mean part
  // hs prior
    beta_tilde ~ normal(0,1);
    lambda ~ student_t(nu_local, 0 ,1);
    tau ~ student_t(nu_global,0,scale_global);
    caux ~ inv_gamma(0.5*slab_df,0.5*slab_df);

  // P spline
  gamma_r[] ~ normal(0,1);
  for(c in 1:numcountry){
    gamma_c[c] ~ normal(gamma_r[getr_c[c]],sigma_c);
  }
  
  for(h in 1:H){
    delta_hc[h,] ~ normal(0,tau_delta);
  }
  
  //bias part
  bias_dt_tilde ~ normal(0,1);// source type bias part
  
  sigma_j ~ normal(0,1);// source type sd trun[0,5] Normal(0,1)
  
  //main part
  for(k in 1:ntrain){
    Y[getitrain_k[k]] ~ normal(mu_ct[getc_i[getitrain_k[k]],gett_i[getitrain_k[k]]]
                  + bias_dt_i[getitrain_k[k]]
                  + bias_def_i[getitrain_k[k]]
                  + delta_ct[getc_i[getitrain_k[k]],gett_i[getitrain_k[k]]]
                  ,
                  sigma_i[getitrain_k[k]]);
  }
}

generated quantities{
  vector[N] log_lik;
for (i in 1:N) log_lik[i] = normal_lpdf(Y[i] | mu_ct[getc_i[i],gett_i[i]]
                  + bias_dt_i[i]
                  + bias_def_i[i]
                  + delta_ct[getc_i[i],gett_i[i]], 
                  sigma_i[i]);
}

