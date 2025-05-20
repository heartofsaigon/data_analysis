
rm(list = ls())
setwd( "/Users/nam-anhtran/heartofsaigon/data_analysis/comp_exam2019")
pacman::p_load(tidyverse, cmdstanr, posterior, HDInterval, loo, extraDistr)

df = read_csv("00_rawdata/Hypoxia_proxy_refined_updated.csv")|>
  mutate(BirthWeight = BirthWeight/1000,
         CoolingTime = ifelse(is.na(CoolingTime),0, CoolingTime),
         HIESeverity = ifelse(HIESeverity=="severe",1,0))

df<-
group_by(df, ID)|>
  summarise_all(mean)|>
  ungroup()

#-----------

mod_dat = list()
mod_dat$N = nrow(df)
mod_dat$y = df$NeurologicalOutcome
mod_dat$X = cbind(
  Copeptin = df$Copeptin,
  nse = df$NSE,
  gender = df$Gender,
  birthweight = df$BirthWeight,
  HIESeverity = df$HIESeverity,
  Apgar5 = df$Apgar5, 
  ph = df$pH,
  lactate = df$Lactate,
  GestationalAge = df$GestationalAge, 
  CoolingTime = df$CoolingTime
)
mod_dat$colX = ncol(mod_dat$X)
mod_dat$X<- mod_dat$X|> apply(2,\(i) (i-mean(i))/sd(i) )


init_fun = function(chain_id){
  set.seed(chain_id)
  list(beta0 = rnorm(1), 
       beta1 = rnorm(mod_dat$colX),  
       sigma_beta1_slab = rnorm(1,10,1),
       decision = runif(mod_dat$colX)[1]
  )
}

mod = cmdstan_model(stan_file = "01_analysis/spikeslab.stan")
fit = mod$sample(data = mod_dat, chains = 3, parallel_chains = 3, refresh = 1000,
                 iter_warmup = 6000, iter_sampling = 2000, init = init_fun,
                 max_treedepth = 15, adapt_delta = 0.95)


fit$summary(c("beta1","decision"), rhat)

d = fit$draws("inc_ind", format = "matrix")|>
  `colnames<-`(mod_dat$X|> colnames())
apply(d, 2,mean)|> as.matrix()

re<-
lapply(1:nrow(d), \(i){
  h<-
  ifelse(d[i,]==1, colnames(d),"")|>
    sort()|>
    {\(i) i[!i==""] }()
  mystring<-
    str_c(h,collapse  = ", ")
  r = length(h)
  tibble(string = mystring, n = r)
})|>
  reduce(bind_rows)

filter(re, n !=0)



fit$save_object(file = "02_results/spikeslab.rds")

