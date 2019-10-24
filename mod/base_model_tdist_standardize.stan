data{
  int<lower=0> N; //number of observations
  int<lower=0> d; //number of preidctors
  int<lower=0> numcountry; // number of countries
  int<lower=0> numregion; // number of regions
  int<lower=0> yearLength; //number of est year
  
  int estyears[yearLength]; //vector of est years;
  real Y[N]; // vector of log sbr
  int<lower=0,upper=numcountry> getc_i[N];          // country for given obs
  int<lower=0,upper=numregion> getr_c[numcountry];  // vector of region given country
  int<lower=1,upper=yearLength> gett_i[N];       // time for given obs   
  int<lower=0,upper=3> getj_i[N];                // source type for given obs
  int<lower=0,upper=4> getd_i[N];                // definition type for given obs
  int<lower=0,upper=30> nu[N];                 //df for t
  
  int <lower=0,upper=1> datatype2_i[N];
  int <lower=0,upper=1> datatype3_i[N];
  

  int <lower=0,upper=1> deftype1_i[N];
  int <lower=0,upper=1> deftype2_i[N];
  int <lower=0,upper=1> deftype3_i[N];
  int <lower=0,upper=1> deftype4_i[N];
  vector[4] eta_d;               // definition adjustment bias
  real<lower=0> phi_d[4];               // definition adjustment var
  
  real<lower=0> var_i[N];              // sampling error 
  
  matrix[numcountry,yearLength] Z1;
  matrix[numcountry,yearLength] Z2;
  matrix[numcountry,yearLength] Z3;
  matrix[numcountry,yearLength] Z4;
  matrix[numcountry,yearLength] Z5;
}

transformed data{
  vector[N] b_i;
  for(i in 1:N){
      b_i[i] = eta_d[1]*deftype1_i[i]+
             eta_d[2]*deftype2_i[i]+
             eta_d[3]*deftype3_i[i]+
             eta_d[4]*deftype4_i[i];}
  
}

parameters {
//mean part
                              //  real beta_w;
 vector[d] beta_tilde;
//deviance part
 vector[2] beta_dt_tilde;
  
  real<lower=0,upper=20> sigma_j[3];
//AR1
  real<lower=-1,upper=1> rho;
  matrix[numcountry,yearLength] epsilon_star;
  real<lower=0> sigma_ar;
                                        //real<lower=0> sigma_w;
  real<lower=0> sigma_r;
  real<lower=0> sigma_c;
  
  vector[numregion] eps_r;
  vector[numcountry] eps_c;
  real<lower=0> sigma;
  }
  
transformed parameters {
  vector[d] beta = beta_tilde *sigma;
  vector[2] beta_dt = beta_dt_tilde *sigma;
  matrix[numcountry,yearLength] mu_ct;
  real z_i[N];
  matrix[numcountry,yearLength] delta_ct;
  vector[numregion] beta_r; 
  vector[numcountry] beta_c;
  
  for(r in 1:numregion){
    beta_r[r]=  sigma_r* eps_r[r];
   }
   
  for(c in 1:numcountry){
   beta_c[c] = beta_r[getr_c[c]] + sigma_c* eps_c[c];
   }
  
  
  for(c in 1:numcountry){
    for(t in 1:yearLength){
  mu_ct[c,t] = beta_c[c] +
            beta[1]*Z1[c,t] +
            beta[2]*Z2[c,t] +
            beta[3]*Z3[c,t] +
            beta[4]*Z4[c,t] +
            beta[5]*Z5[c,t];
  }}
  
  for(i in 1:N){
    z_i[i] = beta_dt[1]*datatype2_i[i]+
             beta_dt[2]*datatype3_i[i];
             

  }
  delta_ct[,1] = sigma_ar/(1-rho^2)*epsilon_star[,1];
  for(t in 2:yearLength){
    delta_ct[,t]=rho * delta_ct[,t-1] + sigma_ar * epsilon_star[,t];
  }
  
}  


model {
// mean part
                                    //beta_w ~normal(0,sigma_w);



target += normal_lpdf(eps_r| 0, 1);

target += normal_lpdf(eps_c| 0, 1);

beta_tilde ~ normal(0,1);

//bias part
beta_dt_tilde ~ normal(0,1);

//ar1 deviance part
for(t in 1:yearLength){
epsilon_star[,t]~ normal(0,1);}

//main part
for(i in 1:N){
            Y[i] ~ student_t(nu[i],
            mu_ct[getc_i[i],gett_i[i]]
            + z_i[i] 
            + b_i[i] 
            + delta_ct[getc_i[i],gett_i[i]]
            ,
            var_i[i] + phi_d[getd_i[i]] + 
            sigma_j[getj_i[i]]);
   }

}
