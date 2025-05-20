
rm(list = ls())
pacman::p_load(tidyverse, cmdstanr, beepr, HDInterval, posterior, rstanarm)

df = read_csv(file = "01_data/clean1.csv")
df<- df[, c("wt", "STFIPS", "DAYWAIT", 
            "PSOURCE", "AGE", "GENDER", "RACE", "EDUC", "HLTHINS", "SUB1",
            "FREQ1", "PSYPROB", "NOPRIOR", "SERVICES", "METHUSE", "EMPLOY", "LIVARAG")]|>
  mutate(AGE = (AGE-mean(AGE))/sd(AGE))

#--------------------




# ---
y = df$DAYWAIT + 1

X = cbind(
  
  PSOURCE1 = c(df$PSOURCE==1)*1,
  PSOURCE2 = c(df$PSOURCE==2)*1,
  
  AGE = df$AGE,
  
  GENDER = df$GENDER,
  
  RACE1 = c(df$RACE==1)*1,
  RACE2 = c(df$RACE==2)*1,
  
  EDUC1 = (df$EDUC==1)*1,
  EDUC2 = (df$EDUC==2)*1,
  
  HLTHINS1 = (df$HLTHINS==1)*1,
  HLTHINS2 = (df$HLTHINS==2)*1,
  HLTHINS3 = (df$HLTHINS==3)*1,
  #---
  SUB1_2 = (df$SUB1==2)*1,
  SUB1_3 = (df$SUB1==3)*1,
  
  FREQ1_1 = (df$FREQ1==1)*1,
  FREQ1_2 = (df$FREQ1==2)*1,
  
  PSYPROB = df$PSYPROB,
  NOPRIOR = df$NOPRIOR,
  
  
  SERVICES_1 = c(df$SERVICES==1)*1,
  SERVICES_2 = c(df$SERVICES==2)*1,
  
  METHUSE = df$METHUSE,
  
  EMPLOY1 = c(df$EMPLOY==1)*1,
  EMPLOY2 = c(df$EMPLOY==2)*1,
  
  LIVARAG1 = c(df$LIVARAG==1)*1,
  LIVARAG2 = c(df$LIVARAG==2)*1
  # ---
  
)




#X = X[,3:10]
mod_dat = list(
  N = nrow(X), P = ncol(X), K = length(unique(y)),
  #N_obs = length(na.omit(y)), obs_idx = which(!is.na(y)), y_obs = y[which(!is.na(y))],
  #N_mis = sum(is.na(y)), mis_idx = which(is.na(y)),
  X = X, G = unique(df$STFIPS)|> length(), group = df$STFIPS|> factor()|> as.numeric(),
  wt = df$wt, y = y
)


init_fun = function(chain_id){
  set.seed(chain_id)
  list(
       beta0 = rnorm(1),
       beta = rnorm(mod_dat$P),
       c = rnorm(mod_dat$K-1)|> sort(),
       b = rnorm(mod_dat$G),
       sigma_b = rgamma(1,5,5)
       )
}

mod = cmdstan_model(stan_file = "02_analysis/01_obj1/daywait.stan")
fit = mod$sample(data = mod_dat, chains = 2, parallel_chains = 3,
                 iter_warmup = 10000, iter_sampling = 2000,
                 refresh = 500, init = init_fun)


fit$summary(c("b"), mean, hdi, rhat)$rhat|> sort()
fit$save_object(file = "03_results/wait.rds")

#######
#################
#############################
############################################




