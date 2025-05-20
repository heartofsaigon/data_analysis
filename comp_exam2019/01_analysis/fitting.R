
setwd( "/Users/nam-anhtran/heartofsaigon/data_analysis/comp_exam2019")
pacman::p_load(tidyverse, cmdstanr, posterior, HDInterval, loo)

df = read_csv("00_rawdata/Hypoxia_proxy_refined_updated.csv")|>
  mutate(BirthWeight = BirthWeight/1000,
         CoolingTime = ifelse(is.na(CoolingTime),0, CoolingTime))

#-----------

mod_dat = list()
mod_dat$N = nrow(df)
mod_dat$J = df$ID|> unique()|> length()
mod_dat$y = df$Copeptin
mod_dat$id = df$ID
mod_dat$time = cbind(time1 = df$Time, time2 = df$Time^2)
mod_dat$X1 = cbind(gender = df$Gender, birthweight = df$BirthWeight, 
                   gender_time = df$Gender*df$Time, birthweight_time = df$BirthWeight*df$Time)
mod_dat$colX1 = ncol(mod_dat$X1)
mod_dat$X2 = cbind(
  HIESeverity = ifelse(df$HIESeverity== "severe",1,0),
  Apgar5 = df$Apgar5, 
  ph = df$pH,
  lactate = df$Lactate,
  GestationalAge = df$GestationalAge, 
  CoolingTime = df$CoolingTime
)
mod_dat$colX2 = ncol(mod_dat$X2)

init_fun = function(chain_id){
  set.seed(chain_id)
  list(beta0 = rnorm(1), 
       beta1 = rnorm(mod_dat$colX1),  
       beta2 = rnorm(mod_dat$colX2),
       beta_time = rnorm(2),
       sigma = rgamma(1,5,5),
       b0 = rnorm(mod_dat$J),
       sigma_b0 = rgamma(1,5,5)
       )
}

mod = cmdstan_model(stan_file = "01_analysis/fitting.stan")
fit = mod$sample(data = mod_dat, chains = 3, parallel_chains = 3, refresh = 1000,
                 iter_warmup = 4000, iter_sampling = 2000, init = init_fun,
                 max_treedepth = 15)
fit$summary("beta1", mean, hdi, rhat)
waic(fit$draws("log_lik", format = "matrix"))

fit$save_object(file = "02_results/fit1_Copeptin.rds")

### -------------------------------------------------------------------
mod_dat$y = df$NSE



mod2 = cmdstan_model(stan_file = "01_analysis/fitting.stan")
fit2 = mod2$sample(data = mod_dat, chains = 3, parallel_chains = 3, refresh = 1000,
                 iter_warmup = 4000, iter_sampling = 2000, init = init_fun,
                 max_treedepth = 15)
fit2$summary("beta1", mean, hdi, rhat)
waic(fit2$draws("log_lik", format = "matrix"))
fit2$save_object(file = "02_results/fit2_NSE.rds")


