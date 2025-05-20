
data {
  int<lower=1> N;                    // total number of rows
  int<lower=1> P;                    // number of fixed covariates
  int<lower=1> G;                    // number of groups

  vector[N]  y;  
  vector [N] wt; // observed y’s
  matrix[N, P] X;                     // design matrix
  array [N] int<lower=1,upper=G> group;      // group ID for each row
}

transformed data {
  vector [N] weight = wt/sum(wt)*N;
}


parameters {
  vector[P]       beta;              // fixed‐effects
  real             beta0;         // hyper‐mean of group intercepts
  real<lower=1e-10>    sigma_b;      // hyper‐sd of group intercepts
  vector[G]        b;            // group intercepts
  real<lower=1e-10>    sigma_y;          // residual SD
}

transformed parameters {
  real icc = sigma_b^2/(sigma_b^2 + sigma_y^2);
}


model {
  // Priors
  beta0 ~ normal(0,100);
  beta         ~ normal(0, 100);
  sigma_b  ~ cauchy(0, 2.5);
  b        ~ normal(0, sigma_b);
  sigma_y      ~ cauchy(0, 2.5);

  // Likelihood (all y, now filled)
    target += weight.*normal_lpdf(y|beta0 + X*beta + b[group], sigma_y);
}


