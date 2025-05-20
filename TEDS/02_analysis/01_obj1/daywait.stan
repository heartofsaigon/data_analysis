
data {
  int<lower=1> N;           // total number of rows
  int<lower=1> P;           // number of predictors
  int<lower=2> K;           // number of ordinal categories
  
  array [N] int<lower=1,upper=K> y;
  matrix [N,P] X;
  vector [N] wt;
  
  int<lower=1> G;                   // number of groups
  array [N] int<lower=1,upper=G> group;    // group membership for each i
}

transformed data {
   real ess = sum(wt)^2/sum(wt^2);
   vector [N] weight = wt/sum(wt)*N;
}

parameters {
//  real beta0;
  vector[P]       beta;       // regression coefficients
  ordered [K-1] c;
  vector [G] b;
  real<lower=1e-10> sigma_b;
}

model {
  // 1) Priors
//  beta0 ~ normal(0,5);
  beta ~ normal(0, 5);
  c    ~ normal(0, 5);
  b ~ normal(0, sigma_b);
  sigma_b ~ normal(0,5);

  // observed
  target += ordered_logistic_lpmf(y|  X*beta + b[group], c).*weight;
}

generated quantities {
  
}



