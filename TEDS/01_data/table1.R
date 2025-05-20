
rm(list = ls())
pacman::p_load(tidyverse, cmdstanr)

df = read_csv(file = "../01_data/clean1.csv")|>
  select(AGE, GENDER, RACE, # demographic
         ALCFLG, SUB1, FREQ1, # substance use
         PSYPROB, # clinical factors
         NOPRIOR, # treatment history
         PSOURCE, # referral pathways
         EDUC # socioeconomic status
         )
df<-
mutate(df,
       Age = AGE,
       Gender = ifelse(GENDER ==0, "male", "female"),
       Race = case_when(RACE ==0 ~ "black", RACE ==1 ~ "white", .default = "others"),
       `Alcohol reported at admission` = ifelse(ALCFLG == 0 , "no", "yes"),
       `Substance use at admission` = case_when(SUB1 == 1 ~ "depressants", SUB1 == 2 ~ "opioids and stimulants", SUB1 == 3 ~ "others"),
       `Frequency of use at admission` = case_when(FREQ1 ==0 ~ "no use", FREQ1 ==1 ~ "some use", FREQ1 ==2 ~ "daily use"),
       `Mental and substance use disorders` = ifelse(PSYPROB == 0, "yes", "no"),
       `Previous substance use treatment` = ifelse(NOPRIOR == 0, "no", "yes"),
       `Referral source` = case_when(PSOURCE ==0 ~ "individual", PSOURCE == 1 ~ "care provider", 
                                     PSOURCE == 2 ~ "community referral", .default = "Court/criminal justice referral"),
       Education = case_when(EDUC == 0 ~ "lower than high school", EDUC ==1 ~ "highschool", .default = "college/university"),
       .keep = "none"
       )


write_csv(df, file = "../03_results/table1.csv")


