
rm(list = ls())
setwd("~/heartofsaigon/data_analysis/TEDS")

pacman::p_load(tidyverse, cmdstanr)
load("00_rawdata/tedsa_puf_2022_r.rdata")
load("00_rawdata/tedsd_puf_2022_r.rdata")
statecode = read_csv("00_rawdata/State_Codes.csv")
s = statecode$Label|> `names<-`(statecode$Value)

#----------
# Create a named vector for STFIPS labels


#-----------
dfa<-
TEDSA_PUF_2022|>
  select(
    CASEID,
    
  # states
    STFIPS,
    
    # outcome --------
    DAYWAIT,
    
    # main covariates
    PSOURCE, # Referral source (recoded: 1 = Criminal justice, 0 = Other). [treatment system factors]
    
    
    # demographics-------------------------
    AGE, # Age at admission (12 categories).
    GENDER, # Biological sex (1 = Male, 2 = Female, -9 = Missing).
    RACE, # Race (9 categories, e.g., 4 = Black, 5 = White).
  #  ETHNIC, # Ethnicity (6 categories, e.g., 1 = Puerto Rican, 4 = Not Hispanic). [we dont choose this as race is a finer set of this]
  #  MARSTAT, # Marital status (4 categories, e.g., 1 = Never married).
    EDUC, # Education at admission (5 categories, e.g., 3 = Grade 12).
    
    # funcding----------------------------
    HLTHINS, # Health insurance (4 categories, e.g., 2 = Medicaid, 4 = None).
  #  PRIMPAY, # Primary payment source (7 categories, e.g., 4 = Medicaid). [health insurance is sufficient, this covariate is a finer set but not necessary]
  #  PRIMINC, # Source of income/support (5 categories, e.g., 1 = Wages/salary).
    
    # severity of substance use --------------
    SUB1, # Primary substance use at admission (e.g., 2 = Alcohol, 5 = Heroin).
    FREQ1, # Frequency of primary substance use (1 = No use, 2 = Some use, 3 = Daily).
  #  DSMCRIT, #  DSM diagnosis (19 categories, e.g., 5 = Opioid dependence).
    PSYPROB, #  Co-occurring disorders (1 = Yes, 2 = No).
  #  FRSTUSE1, # Age at first use of primary substance (7 categories, e.g., 1 = 11 years and under).
    NOPRIOR, # Number of previous treatment episodes (e.g., 0 = None, 1 = 1 episode).
    
    # treatment system factors ------------------
    SERVICES, # Type of treatment service/setting (e.g., 1 = Hospital detox, 6 = Intensive outpatient).
    METHUSE, # Medication-assisted opioid therapy (1 = Yes, 2 = No).
    
    # Social/Economic Factors----------------
    EMPLOY, # Employment status (4 categories, e.g., 1 = Full-time, 3 = Unemployed).
  #  DETNLF, # Detailed not in labor force (5 categories, e.g., 1 = Homemaker, applicable if EMPLOY = 4).
    LIVARAG, # Living arrangements (3 categories, e.g., 1 = Homeless).
 #   PREG, # Pregnant at admission (1 = Yes, 2 = No, only for females). [remove since all male are set to missing, which is not reasonable [should be no]]
  #  VET, # Veteran status (1 = Yes, 2 = No).
  #  ARRESTS, # Arrests in past 30 days (0 = None, 1 = Once, 2 = Two or more).
    
    # Behavioral Factors, ------------------------
  #  FREQ_ATND_SELF_HELP # Self-help group attendance (5 categories, e.g., 1 = No attendance).
  
  # ----- objectieve 3 
  ALCFLG, # alcohol use (2 categories: 0 = Not reported, 1 = Reported)
  ALCDRUG # drug use (0 = None, 1 = Alcohol only, 2 = Other drugs only, 3 = Alcohol and other drugs)
  )|>
  filter(DAYWAIT != -9, SUB1 != -9, ALCDRUG != -9, ALCFLG != -9)

dfa<-
dfa|>
  dplyr::mutate(
    # outcome
    DAYWAIT = case_when(DAYWAIT == 0 ~ 0, DAYWAIT == 1 ~ 1, DAYWAIT %in% c(2,3,4) ~ 2, .default = NA_real_),
    # age: more than 10 groups, using midpoints and treat them as continuous
    AGE = c(13, 16, 19, 22.5, 27, 32, 37, 42, 47, 52, 59.5, 70)[AGE], #|> {\(i) (i-mean(i))/sd(i)}(),
    GENDER = ifelse(GENDER== -9, NA_real_, GENDER) -1,
    RACE = case_when(RACE ==4 ~ 0, RACE == 5  ~ 1, RACE == -9 ~ NA_real_, .default =  2),
    EDUC = case_when(EDUC == 1 ~ 0, EDUC %in% c(2,3) ~ 1, EDUC %in% c(4,5) ~ 2, .default = NA_real_),
    HLTHINS = case_when(HLTHINS == 4 ~ 0, HLTHINS ==1 ~ 1, HLTHINS == 2 ~ 2, HLTHINS == 3 ~ 3, .default = NA_real_),
    SUB1 = case_when(SUB1 == 1 ~ 0, # none 
                     SUB1 %in% c(2,13,14,15,16) ~ 1,  # depressants
                     SUB1 %in% c(3,5,6,7,10,11,12) ~ 2,  # opioids and stimulants
                     SUB1 %in% c(4,8,9,17,18,19) ~ 3, # Cannabis, Hallucinogens, and Other (Marijuana, PCP, Hallucinogens, Inhalants, OTC, Other)
                     .default = NA_real_
                     ),
    
    drug = ifelse(ALCDRUG %in% c(2,3), 1,0),
    alcohol = ALCFLG,
    
    FREQ1 = ifelse(FREQ1 == -9, NA_real_, FREQ1) -1,
    PSYPROB = if_else(PSYPROB == -9, NA_real_, PSYPROB) - 1,
    NOPRIOR = case_when(NOPRIOR == -9 ~ NA_real_, NOPRIOR == 0 ~0, .default = 1),
    PSOURCE = case_when(PSOURCE == -9 ~ NA_real_, PSOURCE ==1 ~ 0,  PSOURCE %in% c(2,3) ~ 1, PSOURCE %in% c(4,5,6) ~ 2, PSOURCE == 7 ~ 3),
    SERVICES = case_when(SERVICES %in% c(1,2) ~ 0, SERVICES %in% c(3,4,5) ~ 1, SERVICES %in% c(6,7,8) ~ 2),
    METHUSE = ifelse(METHUSE == -9, NA_real_, METHUSE) -1,
    EMPLOY = case_when(EMPLOY %in% c(3,4) ~0, EMPLOY %in% c(1) ~ 1, EMPLOY %in% c(2) ~ 2, .default = NA_real_),
    LIVARAG = ifelse(LIVARAG == -9, NA_real_, LIVARAG) - 1,
    ALCDRUG = ifelse(ALCDRUG %in% c(2,3), 1, 0)
  )



#----------------------------------------------------


dfd = TEDSD_PUF_2022|>
  select(
    CASEID, 
    LOS,
    SUB1_D,
    FREQ1_D,
    REASON,
    SERVICES_D = SERVICES_D,
    EMPLOY_D = EMPLOY_D
  )|>
  filter(LOS != -9, SUB1_D != -9, REASON != -9)

dfd<- dfd|>
  mutate(
    LOS = case_when(LOS == 31 ~ 38, LOS == 32 ~ 53, LOS == 33 ~ 75.5, LOS == 34 ~ 105.5,
                    LOS == 35 ~ 150.5, LOS == 36 ~ 273, LOS == 37 ~ 438, .default = LOS),
    REASON = ifelse(REASON == 1, 1, 0), 
    SERVICES_D = case_when(SERVICES_D %in% c(1,2) ~ 0, SERVICES_D %in% c(3,4,5) ~ 1, SERVICES_D %in% c(6,7,8) ~ 2),
    EMPLOY_D = case_when(EMPLOY_D %in% c(3,4) ~0, EMPLOY_D %in% c(1) ~ 1, EMPLOY_D %in% c(2) ~ 2, .default = NA_real_),
    FREQ1_D = ifelse(FREQ1_D == -9, NA_real_, FREQ1_D) -1,
    SUB1_D = case_when(SUB1_D == 1 ~ 0, # none 
                     SUB1_D %in% c(2,13,14,15,16) ~ 1,  # depressants
                     SUB1_D %in% c(3,5,6,7,10,11,12) ~ 2,  # opioids and stimulants
                     SUB1_D %in% c(4,8,9,17,18,19) ~ 3, # Cannabis, Hallucinogens, and Other (Marijuana, PCP, Hallucinogens, Inhalants, OTC, Other)
                     .default = NA_real_
    ),
    drug_dics = ifelse(SUB1_D %in% c(3:19), 1,0),
    alcohol_dics = ifelse(SUB1_D == 2 , 1, 0)
  )





df<- inner_join(dfa, dfd)|>
  na.omit()


k<- 
  nest_by(df,  STFIPS, .keep = T)|>
  ungroup()|>
  # make sure each state has enough number of subgroups for all covariate if interest. 
  filter( map_dbl(data, \(d) length(unique(d$DAYWAIT)))==3)|>
  filter( map_dbl(data, \(d) length(unique(d$drug_dics)))==2)|>
  filter( map_dbl(data, \(d) length(unique(d$drug)))==2)|>
  filter( map_dbl(data, \(d) length(unique(d$REASON)))==2)|>
  filter( map_dbl(data, \(d) length(unique(d$PSOURCE)))==4)|>
  # make sure the proportion of each subgroup in each state is at least 1%.
  filter( map_dbl(data, \(d) min(table(d$DAYWAIT))/nrow(d))> 0.01)|>
  filter( map_dbl(data, \(d) min(table(d$drug_dics))/nrow(d))> 0.01)|>
  filter( map_dbl(data, \(d) min(table(d$drug))/nrow(d))> 0.01)|>
  filter( map_dbl(data, \(d) min(table(d$REASON))/nrow(d))> 0.01)|>
  filter( map_dbl(data, \(d) min(table(d$PSOURCE))/nrow(d))> 0.01)|>
  pull(data)|>
  reduce(bind_rows)|>
  mutate(STFIPS = s[STFIPS|> as.character()])

  

write_csv(k, file = "01_data/clean.csv")

####################################################################################

p<- c(
(table(k$DAYWAIT)/nrow(k)),
(table(k$drug)/nrow(k)),
(table(k$drug_dics)/nrow(k)),
(table(k$REASON)/nrow(k)),
(table(k$PSOURCE)/nrow(k)),
(table(k$LOS)/nrow(k))
)


n0 = (qnorm(0.975)^2*p*(1-p)/0.03^2)|> ceiling()|> max()

df_new = 
  nest_by(k, STFIPS, .keep = T)|>
  ungroup()|>
  mutate(N.h = map_dbl(data, nrow))|>
  mutate(n.h = ceiling(N.h/sum(N.h)*n0))|>
  arrange(n.h)|>
  # since min(n0) = 1, we need to set minimum obsrvation for each state.
  mutate(n.h_add = ceiling((n0- n()*30)*N.h/sum(N.h)) )|>
  mutate(n.h_new = 30 + n.h)|>
  mutate(wt = N.h/n.h_new)|>
  mutate(newdat = map2(data,n.h_new, \(i,j) sample_n(i,j)))|>
  mutate(newdat = map2(newdat, wt, \(i,j) mutate(i,wt = j)))|>
  pull(newdat)|>
  reduce(bind_rows)

# check 
nest_by(df_new, STFIPS)|> # 3
  ungroup()|>
  mutate(n = map_dbl(data, \(i) unique(i$DAYWAIT)|> length() ))|>
  pull(n)|>
  table()

nest_by(df_new, STFIPS)|> # 2
  ungroup()|>
  mutate(n = map_dbl(data, \(i) unique(i$drug)|> length() ))|>
  pull(n)|>
  table()

nest_by(df_new, STFIPS)|> # 2
  ungroup()|>
  mutate(n = map_dbl(data, \(i) unique(i$drug_dics)|> length() ))|>
  pull(n)|>
  table()

nest_by(df_new, STFIPS)|> # 2
  ungroup()|>
  mutate(n = map_dbl(data, \(i) unique(i$REASON)|> length() ))|>
  pull(n)|>
  table()

nest_by(df_new, STFIPS)|> # 4
  ungroup()|>
  mutate(n = map_dbl(data, \(i) unique(i$PSOURCE)|> length() ))|>
  pull(n)|>
  table()


write_csv(df_new, file = "01_data/clean1.csv")


###########

d1 = select(df_new, - SUB1_D, -FREQ1_D, - SERVICES_D, -EMPLOY_D, - drug_dics, -alcohol_dics)|>
  mutate(time = 0)
d2 = select(df_new, - SUB1, -FREQ1, - SERVICES, -EMPLOY, - drug, -alcohol)|>
  rename(SUB1 = SUB1_D, FREQ1 = FREQ1_D, SERVICES = SERVICES_D, EMPLOY = EMPLOY_D,drug = drug_dics,alcohol = alcohol_dics)|>
  mutate(time = 1)

df_new2<-
bind_rows(d1,d2)|>
  arrange(CASEID)

write_csv(df_new2, file = "01_data/clean2.csv")














