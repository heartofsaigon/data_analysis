
pacman::p_load(tidyverse, cmdstanr)

pbc = read_csv(file = "01_data/pbc.csv")

na.cov = which(sapply(pbc, compose(~ .>0,sum,is.na)))
pbc[,na.cov]


