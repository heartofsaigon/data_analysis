

rm(list = ls())
pacman::p_load(tidyverse, cmdstanr, beepr, HDInterval, posterior)

df = read_csv(file = "01_data/clean2.csv")
  

y = df$drug

X = cbind(
  alcohol = df$alcohol,
  time = df$time,
  alc_time = df$alcohol*df$time,

  AGE = df$AGE,
  
  GENDER = df$GENDER,
  
  RACE1 = c(df$RACE==1)*1,
  RACE2 = c(df$RACE==2)*1,

  EDUC1 = (df$EDUC==1)*1,
  EDUC2 = (df$EDUC==2)*1,
  
  PSYPROB = df$PSYPROB,
  
  SERVICES1 = ifelse(df$SERVICES ==1, 1, 0),
  SERVICES2 = ifelse(df$SERVICES ==2, 1, 0),
  
  PSOURCE1 = ifelse(df$PSOURCE ==1,1,0),
  PSOURCE2 = ifelse(df$PSOURCE ==2,1,0),
  PSOURCE3 = ifelse(df$PSOURCE ==3,1,0),
  
  NOPRIOR = df$NOPRIOR,
  
  EMPLOY1 = c(df$EMPLOY==1)*1,
  EMPLOY2 = c(df$EMPLOY==2)*1
)


glm(y ~ X, family = "binomial")|>
  broom::tidy(conf.int = T)

mod_dat = list(y = y, X = X, N = nrow(X),  P = ncol(X),  wt = df$wt, 
               G = df$STFIPS|> unique()|> length(), 
               group = df$STFIPS|> compose(as.integer, factor)())

init_fun = function(chain_id){
  set.seed(chain_id)
  list(
    beta0 = rnorm(1), 
    beta = rnorm(mod_dat$P), 
                 b = rnorm(mod_dat$G), 
                 sigma_b = rgamma(1,5,5)
  )
}


mod = cmdstan_model(stan_file = "../02_analysis/03_obj3/drugalcohol.stan")
fit = mod$sample(data = mod_dat, chains = 2, parallel_chains = 2,
                 iter_warmup = 4000, iter_sampling = 2000,
                 refresh = 500, max_treedepth = 12 , init = init_fun)



fit$save_object(file = "../03_results/drug_alcohol.rds")

##########
####################
################################

fit  = read_rds("03_results/drug_alcohol.rds")

write_csv(
fit$summary(c("beta"), mean = ~ mean(exp(.)), hdi = ~hdi(exp(.)))|>
  slice(1:3)|>
  mutate(variable = c("alcohol use", "discharge", "alcohol use x discharge"))|>
  mutate(across(where(is.double), \(i) ifelse(i<0.001, "<0.001", as.character(round(i,2))))),

file = "03_results/drugalcohol.csv"
)










