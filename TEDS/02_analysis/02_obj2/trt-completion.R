
rm(list = ls())
pacman::p_load(tidyverse, cmdstanr, beepr, HDInterval, posterior)

df = read_csv(file = "01_data/clean1.csv")
df<- df|>
  select(
    wt,
    STFIPS,
    REASON, # outcome
    PSOURCE, # primary predictor
    AGE, GENDER, RACE,  # demographic
    PSYPROB, SERVICES, # Treatment-Related Variables:
    EMPLOY, EDUC, # Socioeconomic Factors
    HLTHINS,  # socioeconomic
    SUB1_D, FREQ1_D, NOPRIOR,   # substance
     LIVARAG, METHUSE   # Clinical and Social Factors
  )

y = df$REASON
X = cbind(
  PSOURCE1 = (df$PSOURCE==1)*1,
  PSOURCE2 = (df$PSOURCE==2)*1,
  PSOURCE3 = (df$PSOURCE==3)*1,
  
  AGE = df$AGE,
  
  GENDER = df$GENDER,
  
  RACE1 = c(df$RACE==1)*1,
  RACE2 = c(df$RACE==2)*1,
  
  PSYPROB = df$PSYPROB,
  
  SERVICES1 = ifelse(df$SERVICES ==1, 1,0), 
  SERVICES2 = ifelse(df$SERVICES ==2, 1,0), 
  
  EMPLOY1 = c(df$EMPLOY==1)*1,
  EMPLOY2 = c(df$EMPLOY==2)*1,
  
  EDUC1 = (df$EDUC==1)*1,
  EDUC2 = (df$EDUC==2)*1,
  
  HLTHINS1 = (df$HLTHINS==1)*1,
  HLTHINS2 = (df$HLTHINS==2)*1,
  HLTHINS3 = (df$HLTHINS==3)*1,
  
  SUB1_2 = (df$SUB1_D==2)*1,
  SUB1_3 = (df$SUB1_D==3)*1,
  
  FREQ1_1 = (df$FREQ1_D==1)*1,
  FREQ1_2 = (df$FREQ1_D==2)*1,
  
  NOPRIOR = df$NOPRIOR,
  
  METHUSE = df$METHUSE,
  
  LIVARAG1 = c(df$LIVARAG==1)*1,
  LIVARAG2 = c(df$LIVARAG==2)*1
)

mod_dat = list(y = y, X = X, N = nrow(X),  P = ncol(X), G = length(unique(df$STFIPS)),
               group = df$STFIPS|> compose(as.integer, factor)(), wt = df$wt)


init_fun = function(chain_id){
  set.seed(chain_id)
  list(
    beta0 = rnorm(1), beta = rnorm(mod_dat$P), sigma_b = runif(1,1,10),
    b = rnorm(mod_dat$G)
  )
}

mod = cmdstan_model(stan_file = "02_analysis/02_obj2/trt_compl.stan")
fit = mod$sample(data = mod_dat, chains = 2, parallel_chains = 2,
                 iter_warmup = 5000, iter_sampling = 2000,
                 refresh = 500, max_treedepth = 12 , init = init_fun)
# mod2 = cmdstan_model(stan_file = "02_analysis/02_obj2/trt_compl2.stan")
# fit2 = mod2$sample(data = mod_dat, chains = 2, parallel_chains = 2,
#                  iter_warmup = 5000, iter_sampling = 2000,
#                  refresh = 500, max_treedepth = 12 , init = init_fun)




fit$save_object(file = "03_results/trt_compl.rds")
# fit2$save_object(file = "03_results/trt_compl2.rds")  

 
##########
######################
######################################

fit = read_rds("../03_results/trt_compl.rds")
fit2 = read_rds("../03_results/trt_compl2.rds")

fit$summary("p_inclusion", mean)|>
  mutate(variable = colnames(X))|>
  arrange(mean)|>
  {\(i) `names<-`(i$mean, i$variable) }()
  

r<-
fit$summary(c("beta"), mean = ~ mean(exp(.)), hdi = ~ hdi(exp(.)))|>
  mutate(variable = colnames(X))|>
  slice(1:3)|>
  mutate(variable = c("Care provider", "Community referral", "Court/criminal justice referral"))
write_csv(r, file = "../03_results/psource.csv")

  
####
 
df_plot<-   
fit$summary("p_inclusion", mean)|>
  mutate(variable = colnames(X))|>
  mutate(variable = str_remove_all(variable, "_{0,1}\\d"))|>
  distinct_all()|>
  arrange((mean))|>
  mutate(variable = factor(variable, levels = variable))


ggplot(df_plot, aes(x = variable, y = mean)) +
  geom_col(fill = "steelblue") +         # or pick a palette of your choice
  coord_flip() +                         # makes it horizontal
  scale_y_continuous(expand = c(0, 0)) + # no space below
  labs(
    x     = NULL,                        # drop x‐axis title
    y     = "inclusion probability",                # you can rename as needed
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 12),  # adjust label size
    axis.text.x = element_text(size = 12),
    panel.grid.major.y = element_blank()    # remove horizontal grid lines
  )
  
 
