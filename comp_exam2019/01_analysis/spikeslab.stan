
data {
  int<lower=0> N;
  int<lower=0> colX;
  array[N] int<lower=0,upper=1> y;
  matrix[N,colX] X;
}

transformed data {
  real<lower=0> sigma_beta1_spike = 0.001;
}

parameters {
  real beta0;
  vector[colX] beta1;
  real<lower=0.1> sigma_beta1_slab;
  real<lower=0,upper=1>  decision;
}

transformed parameters {
  vector[N] lin = beta0 + X*beta1;
}


model {
  
  beta0 ~ normal(0,100);
  decision ~ uniform(0,1);
  sigma_beta1_slab ~ normal(0,5);
  
  target+= log_mix(decision, normal_lpdf(beta1|0,sigma_beta1_slab),
  normal_lpdf(beta1|0,sigma_beta1_spike));
  
  y ~ bernoulli_logit(lin);
}

generated quantities {
  
  vector [colX] inc_ind;
  real log_slab;
  real log_spike;
  vector[colX] p_inclusion;
  
  for(i in 1:colX){
    log_slab = log(decision) + normal_lpdf(beta1[i] | 0.0, sigma_beta1_slab);
    log_spike = log(1 - decision) + normal_lpdf(beta1[i] | 0.0, sigma_beta1_spike);
    p_inclusion[i] = exp(log_slab) / (exp(log_slab) + exp(log_spike));
    inc_ind[i] = bernoulli_rng(p_inclusion[i]);
  }
}

