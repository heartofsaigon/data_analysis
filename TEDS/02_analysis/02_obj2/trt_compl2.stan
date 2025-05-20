
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
}

parameters {
  real         beta0;       // intercept
  vector[P]    beta;        // regression coefficients
  vector [G] b;
  real<lower=1e-10> sigma_b;
}

model {
  beta0 ~ normal(0, 5);
  beta ~ normal(0,5);
  sigma_b ~ cauchy(0,2.5);
  b ~ normal(0, sigma_b);
  
  target+= weight.*bernoulli_logit_lpmf(y|beta0 + X*beta + b[group]);
}



