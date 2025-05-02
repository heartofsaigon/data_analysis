

data {
  int<lower=0> n;
  int<lower=0> n_time_point;
  int<lower=0> n_cov;
  vector<lower=0> [n_time_point] time_point;
  array [n] int<lower=0,upper=1> event;
  vector<lower=0> [n] event_time;
  matrix[n,n_cov] X;
  
  real<lower=0> r; // gamma0
  real<lower=0> c0;
  real<lower=0> eps;

}//close data

transformed data {
  int<lower=0> Tnum = n_time_point -1; // number of intervals
  array [n,Tnum] int<lower=0,upper=1>  risk; // risk set
  array [n, Tnum] int<lower=0>  dN; // event set
  
  // the following codes do: 
  // - consider the interval starting at time_j, 
  // - if obs died before time_j --> no information --> risk set and dead set iare zero
  // - if obs time greater than time_j, risk set is 1. we just know that patient could die in time_j-time_{j+1} or live longer than this interval
  //    - we now consider if patient died in current interval. if dying, and no censor, dead set is 1, otherwise, zero
  
  for(i in 1:n){for(j in 1:Tnum){ // for each subject, we consider each interval. Tnum equals the number of time point minus 1, as we have n-1 intervals from n nodes
    
    if(event_time[i] - time_point[j] + eps >0){ // if obs greater than current time point, patient in risk and contribute to loglik
    risk[i,j] = 1; // therefore risk =1 
    if(time_point[j+1] - event_time[i] - eps > 0 && event[i]){
      dN[i,j] = 1; // dN is if patient died in interval t_i and t_{i+1}
      } else{dN[i,j] = 0; }
      } else{ // if patient died in the past, there is no information in the current interval (lower bound is time_j)
        risk[i,j] = 0; // therefore risk is zero
        dN[i,j] = 0; // dead set is zero
        }
        }}
//------------------------------------------------------------------------------
 vector [Tnum] dL0_star;
 for(i in 1:Tnum){
    dL0_star[i] = r*(time_point[i+1] - time_point[i])+eps;
    }// contribution at each interval based on time length. 
  }

parameters {
  vector[n_cov] beta;
  vector<lower=eps> [Tnum] dL0; // intensity??
  } 

transformed parameters {
   // matrix [n, Tnum] Idt = rep_matrix(eps, n, Tnum);
   //  for(i in 1:n){for(j in 1:Tnum){
   //    if(risk[i,j]){Idt[i,j] = exp(X[i,]*beta)*dL0[j]+eps;} // this is the model of poisson.
   //  // risk[i,j] tell us if that patient is in risk, if so, contribution, which is dL, is considered
   //  }}
   
  vector[n] linpred = exp(X * beta);                // n×1
  matrix[n, Tnum] Idt = (linpred * dL0')            // n×Tnum outer product
                        .* (to_matrix(risk)+eps);          // zero out non‑risk cells
}

model {
  beta ~ normal(0, 10);
  dL0 ~ gamma(dL0_star*c0, c0);

  for(i in 1:Tnum){target += poisson_lpmf(dN[,i]|Idt[,i]);}
  
}

generated quantities {
  // vector [n*Tnum] log_lik;
  // int k = 1;
  // for(i in 1:n){for(j in 1:Tnum){
  //   log_lik[k] = poisson_lpmf(dN[i,j]|Idt[i,j]);
  //   k+=1;
  // }}

}
