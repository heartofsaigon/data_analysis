rm(list = ls())
pacman::p_load(tidyverse, cmdstanr, beepr, HDInterval, posterior)

df = read_csv(file = "../01_data/clean1.csv")
df<- df[, c("wt", "STFIPS", "LOS", 
            "PSOURCE", "AGE", "GENDER", "RACE", "EDUC", "HLTHINS", "SUB1_D", "REASON",
            "FREQ1_D", "PSYPROB", "NOPRIOR", "SERVICES_D", "METHUSE", "EMPLOY", "LIVARAG")]|>
  mutate(AGE = (AGE-mean(AGE))/sd(AGE))


# ---
y = df$LOS

X = cbind(
  
  PSOURCE1 = c(df$PSOURCE==1)*1,
  PSOURCE2 = c(df$PSOURCE==2)*1,
  
  REASON = df$REASON,
  
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
  SUB1_D_2 = (df$SUB1_D==2)*1,
  SUB1_D_3 = (df$SUB1_D==3)*1,
  
  FREQ1_D_1 = (df$FREQ1_D==1)*1,
  FREQ1_D_2 = (df$FREQ1_D==2)*1,
  
  PSYPROB = df$PSYPROB,
  NOPRIOR = df$NOPRIOR,
  
  
  SERVICES_D_1 = c(df$SERVICES_D==1)*1,
  SERVICES_D_2 = c(df$SERVICES_D==2)*1,
  
  METHUSE = df$METHUSE,
  
  EMPLOY1 = c(df$EMPLOY==1)*1,
  EMPLOY2 = c(df$EMPLOY==2)*1,
  
  LIVARAG1 = c(df$LIVARAG==1)*1,
  LIVARAG2 = c(df$LIVARAG==2)*1
  # ---
  
)


mod_dat = list(
  N = nrow(X), P = ncol(X), K = length(unique(y)),
  #N_obs = length(na.omit(y)), obs_idx = which(!is.na(y)), y_obs = y[which(!is.na(y))],
  #N_mis = sum(is.na(y)), mis_idx = which(is.na(y)),
  X = X, G = unique(df$STFIPS)|> length(), group = df$STFIPS|> compose(as.numeric, factor)(),
  wt = df$wt, y = y
)


init_fun = function(chain_id){
  set.seed(chain_id)
  list(
    beta0 = rnorm(1),
    beta = rnorm(mod_dat$P),
    b = rnorm(mod_dat$G),
    sigma_b = rgamma(1,5,5),
    sigma_y = rgamma(1,5,5)
  )
}

mod = cmdstan_model(stan_file = "02_analysis/01_obj1/los.stan")
fit = mod$sample(data = mod_dat, chains = 2, parallel_chains = 3,
                 iter_warmup = 2000, iter_sampling = 2000,
                 refresh = 500, init = init_fun)

fit$summary(c("b"),rhat)|> pull()|> sort()
fit$save_object(file = "03_results/los.rds")

#######
#################
#############################
############################################




fit1 = read_rds("../03_results/wait.rds")
fit2 = read_rds("../03_results/los.rds")

state_name<- data.frame(name = df$STFIPS|> factor(), 
                        num = df$STFIPS|> factor()|> as.integer())|>
  distinct_all()|>
  arrange(num)|>
  pull(name)




b1 = fit1$draws("b", format = "matrix")|>
  `colnames<-`(state_name)

(apply(b1, 2,mean))|>
  {\(i) names(i)[which.min(abs(i))] }()

b1 = apply(b1, 2, \(i) i - b1[,"Arkansas"] )

# wt = df$wt|> unique()|> {\(i) i/sum(i)}()
or = 
  apply(b1 ,2, \(i) c(mean = mean(exp(i)), hdi(exp(i))))|>
  t()|>
  as.data.frame()|>
  rownames_to_column(var = "variable")|>
  as_tibble()|>
  arrange(mean)|>
  mutate(variable = factor(variable, levels = variable))

p1<-
ggplot(or, aes(x = mean, y = variable)) +
  geom_point() +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
  theme_minimal(base_size = 14)+
  xlab("Odds ratio")+
  ylab("State")


#---


b2 = fit2$draws("b", format = "matrix")|>
  `colnames<-`(state_name)


# b1 = apply(b, 2, \(i) i - b[,"District of Columbia"] )

los = 
  apply(b2 ,2, \(i) c(mean = mean((i)), hdi((i))))|>
  t()|>
  as.data.frame()|>
  rownames_to_column(var = "variable")|>
  as_tibble()|>
  mutate(variable = factor(variable, levels = or$variable[order(or$mean)]) )

  

p2<-
ggplot(los, aes(x = mean, y = variable)) +
  geom_point() +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
  theme_minimal(base_size = 14)+
  xlab("LOS")+
  labs(y = NULL)+
  theme(
    axis.text.y  = element_blank(),  # remove x-axis labels
    axis.ticks.y = element_blank()   # remove x-axis ticks
  )

ggpubr::ggarrange(p1,p2, ncol = 2)
















