


data {
  int<lower=1> N;           // number of observations
  int<lower=1> P;           // number of predictors

  matrix[N, P] X;           // predictor matrix (without intercept column)
  array [N] int<lower=0,upper=1> y; // binary outcome (0 or 1)
  vector [N] wt;
  
  int<lower=1> G;
  array [N] int <lower=1,upper=G> group;

}

transformed data {
  vector [N] weight = wt/sum(wt)*N;
}

parameters {
  real         beta0;       // intercept
  vector[P]    beta;        // regression coefficients
  vector[G] b;
  real<lower=1e-10> sigma_b;
}

model {
  // 1) Priors
  beta0 ~ normal(0, 5);
  beta ~ normal(0,5);
  sigma_b ~ cauchy(0,2.5);
  b ~ normal(0, sigma_b);

  target+= weight.*bernoulli_logit_lpmf(y|beta0 + X*beta + b[group]);
}

generated quantities {

}

