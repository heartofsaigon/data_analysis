
data {
  int<lower=0> N;
  int<lower=0> J;
  int<lower=0> colX1;
  int<lower=0> colX2;
  vector[N] y;
  matrix[N,colX1] X1;
  matrix[N,colX2] X2;
  matrix[N, 2] time;
  array[N] int id;
}

parameters {
  real beta0;
  vector[colX1] beta1;
  vector[colX2] beta2;
  vector[2] beta_time; 
  real<lower=1e-10> sigma;
  vector[J] b0;
  real<lower=1e-10> sigma_b0;
}

transformed parameters {
  vector[N] mu = beta0 + X1*beta1 + X2*beta2 + time*beta_time + b0[id];
}


model {
  
  beta0 ~ normal(0,100);
  beta1 ~ normal(0,100);
  beta2 ~ normal(0,100);
  beta_time ~ normal(0,100);
  sigma_b0 ~ cauchy(0,2);
  b0 ~ normal(0, sigma_b0);
  sigma ~ cauchy(0,2);
  
  target+= normal_lpdf(y| mu, sigma);
}

generated quantities {
  vector[N] log_lik;
  for(i in 1:N){
    log_lik[i] = normal_lpdf(y[i]|mu[i], sigma);
  }
}












