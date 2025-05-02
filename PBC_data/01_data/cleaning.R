

pacman::p_load(tidyverse)

# 1. Read in with read.table()
pbc_raw <- read.table(
  "00_rawdata/pbc.dat",          # or the full path to your .dat file
  header      = FALSE,    # there is no header line
  na.strings  = ".",      # “.” in the file becomes NA
  stringsAsFactors = FALSE
)

# 2. Assign the correct column names (from the pbc documentation)
colnames(pbc_raw) <- c(
  "id", "time", "status", "treatment", "age", "sex",
  "ascites", "hepato", "spiders", "edema",
  "bili", "chol", "albumin", "copper", "alk_phos",
  "sgot", "trig", "platelet", "prothrombin", "stage"
)

write_csv(pbc_raw, file = "01_data/pbc.csv")






