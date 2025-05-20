
data {
  int<lower=1> N;           // number of observations
  int<lower=1> P;           // number of predictors
  int<lower=1> G;
  
  matrix[N, P] X;           // predictor matrix (without intercept column)
  array [N] int<lower=0,upper=1> y; // binary outcome (0 or 1)
  vector [N] wt;
  array [N] int<lower=1,upper=G> group;
}

transformed data {
  real ess = sum(wt)^2/sum(wt^2);
  vector [N] weight = wt/sum(wt)*N;
  real spike = 1e-3;
}

parameters {
  real         beta0;       // intercept
  vector[P]    beta;        // regression coefficients
  vector [G] b;
  real<lower=1e-10> sigma_b;
  real<lower=0.05> sigma_beta;
  real<lower=0,upper=1> decision;
}

model {
  // 1) Priors
  beta0 ~ normal(0, 5);
  sigma_b ~ cauchy(0,2.5);
  b ~ normal(0, sigma_b);
  decision ~ beta(1,1);
  
  
  target += log_mix(decision,
    normal_lpdf(beta| 0, sigma_beta),
    normal_lpdf(beta|0, spike)
  );


  target+= weight.*bernoulli_logit_lpmf(y|beta0 + X*beta + b[group]);
}

generated quantities {

vector [P] inc_ind;
  real log_slab;
  real log_spike;
  vector[P] p_inclusion;
  
  for(i in 1:P){
    log_slab = log(decision) + normal_lpdf(beta[i] | 0.0, sigma_beta);
    log_spike = log(1 - decision) + normal_lpdf(beta[i] | 0.0, spike);
    p_inclusion[i] = exp(log_slab) / (exp(log_slab) + exp(log_spike));
    inc_ind[i] = bernoulli_rng(p_inclusion[i]);
  }
  
}



