# Load necessary libraries
library(tidyverse)
library(knitr)
library(kableExtra)
library(rmarkdown)

# Set seed for reproducibility
set.seed(123)

# Step 1: Create baseline characteristics for 85 neonates
n_neonates <- 85
J <- n_neonates  # Define J as the number of unique IDs (groups)
neonates <- tibble(
  ID = 1:n_neonates,
  Gender = rbinom(n_neonates, 1, 0.5), # 0 = female, 1 = male
  BirthWeight = rnorm(n_neonates, mean = 3200, sd = 500), # grams
  Cooling = rbinom(n_neonates, 1, 0.5), # 0 = no, 1 = yes
#  NeurologicalOutcome = rbinom(n_neonates, 1, 0.5), # Placeholder, will be overwritten
  HIESeverity = sample(c("mild", "moderate", "severe"), n_neonates, replace = TRUE, prob = c(0.3, 0.4, 0.3)), # Severity distribution
  Apgar5 = rnorm(n_neonates, mean = 6, sd = 2) %>% pmax(0) %>% pmin(10), # 5-min Apgar score (0-10)
  pH = rnorm(n_neonates, mean = 7.0, sd = 0.1) %>% pmax(6.8) %>% pmin(7.2), # Cord blood pH
  Lactate = rnorm(n_neonates, mean = 7, sd = 2) %>% pmax(4) %>% pmin(12), # Serum lactate (mmol/L)
  GestationalAge = rnorm(n_neonates, mean = 39, sd = 2) %>% pmax(34) %>% pmin(42), # Weeks
  CoolingTime = rnorm(n_neonates, mean = 4, sd = 2) %>% pmax(0) %>% pmin(6) # Hours to cooling (if Cooling = 1)
) %>%
  mutate(CoolingTime = ifelse(Cooling == 0, NA, CoolingTime))

# Step 2: Expand to longitudinal data (6 time points per neonate)
time_points <- c(6, 12, 24, 48, 72, 168)
data_long <- neonates %>%
  crossing(Time = time_points)

# Step 3: Simulate Copeptin and NSE trajectories with additional covariates
data_long <- data_long %>%
  mutate(
    SeverityEffect = case_when(
      HIESeverity == "mild" ~ 0.8,
      HIESeverity == "moderate" ~ 1.0,
      HIESeverity == "severe" ~ 1.2
    ),
    ApgarEffect = (10 - Apgar5) / 10,
    pHEffect = (7.2 - pH) / 0.2,
    LactateEffect = (Lactate - 4) / 8,
    GAEffect = 1 + (42 - GestationalAge) / 10,
    CoolingTimeEffect = ifelse(Cooling == 1, (CoolingTime - 2) / 4, 0),
    
    Copeptin_base = 35 * exp(-0.0105 * (Time - 6)) + rnorm(n(), 0, 8),
    Copeptin = Copeptin_base * 
      SeverityEffect * 
      ApgarEffect * 
      pHEffect * 
      LactateEffect * 
      GAEffect * 
      (1 + CoolingTimeEffect) * 
      (1 - 0.2 * Cooling) * 
      (1 + 0.1 * Gender) * 
      (1 - 0.00001 * BirthWeight),
    
    NSE_base = case_when(
      Time <= 24 ~ 35 + (50 - 35) * (Time - 6) / (24 - 6),
      TRUE ~ 50 * exp(-0.009 * (Time - 24))
    ) + rnorm(n(), 0, 10),
    NSE = NSE_base * 
      SeverityEffect * 
      ApgarEffect * 
      pHEffect * 
      LactateEffect * 
      GAEffect * 
      (1 + CoolingTimeEffect) * 
      (1 - 0.2 * Cooling) * 
      (1 + 0.1 * Gender) * 
      (1 - 0.00001 * BirthWeight)
  ) %>%
  mutate(
    Copeptin = pmax(Copeptin, 0),
    NSE = pmax(NSE, 0)
  ) %>%
  select(-Copeptin_base, -NSE_base, -SeverityEffect, -ApgarEffect, -pHEffect, -LactateEffect, -GAEffect, -CoolingTimeEffect)

# Step 4: Generate NeurologicalOutcome based on 5 relevant covariates
data_outcome <- data_long %>%
  filter(Time == 168) %>%
  mutate(
    std_Copeptin = scale(Copeptin)[, 1],
    std_NSE = scale(NSE)[, 1],
    std_Gender = scale(Gender)[, 1],
    std_BirthWeight = scale(BirthWeight)[, 1],
    std_Cooling = scale(Cooling)[, 1],
    std_HIESeverity_moderate = scale(as.integer(HIESeverity == "moderate"))[, 1],
    std_HIESeverity_severe = scale(as.integer(HIESeverity == "severe"))[, 1],
    std_Apgar5 = scale(Apgar5)[, 1],
    std_pH = scale(pH)[, 1],
    std_Lactate = scale(Lactate)[, 1],
    std_GestationalAge = scale(GestationalAge)[, 1],
    std_CoolingTime = scale(ifelse(Cooling == 1, CoolingTime, 0))[, 1]
  )

true_beta <- c(
  std_Copeptin = 1.5,
  std_NSE = 1.0,
  std_Gender = 0.8,
  std_BirthWeight = -0.5,
  std_Cooling = 1.2,
  std_HIESeverity_moderate = 0,
  std_HIESeverity_severe = 0,
  std_Apgar5 = 0,
  std_pH = 0,
  std_Lactate = 0,
  std_GestationalAge = 0
)

sigma_u <- 1.0
u <- rnorm(J, 0, sigma_u)  # Use J for random intercepts
X <- as.matrix(data_outcome[, names(true_beta)])
alpha <- -0.5
eta <- alpha + X %*% true_beta + u[data_outcome$ID]
prob <- 1 / (1 + exp(-eta))
data_outcome$NeurologicalOutcome <- rbinom(n_neonates, 1, prob)

data_long <- data_long %>%
  left_join(data_outcome %>% select(ID, NeurologicalOutcome), by = "ID")

# Step 5: Save the dataset (CSV only for data)
write_csv(data_long, "00_rawdata/Hypoxia_proxy_refined_updated.csv")

# Step 6: Create and export the codebook directly as a PDF
codebook <- tibble(
  Variable = c("ID", "Time", "Copeptin", "NSE", "Gender", "BirthWeight", "Cooling", "NeurologicalOutcome", 
               "HIESeverity", "Apgar5", "pH", "Lactate", "GestationalAge", "CoolingTime"),
  Description = c(
    "Neonate identifier (1 to 85)",
    "Time of measurement in hours (6, 12, 24, 48, 72, 168)",
    "Copeptin biomarker level (ng/mL)",
    "Neuron-specific enolase biomarker level (ng/mL)",
    "Gender (0 = female, 1 = male)",
    "Birth weight (grams)",
    "Received therapeutic hypothermia (0 = no, 1 = yes)",
    "Neurological outcome at 2 years (0 = unfavorable, 1 = favorable)",
    "HIE severity (mild, moderate, severe)",
    "5-minute Apgar score (0-10)",
    "Cord blood pH (6.8-7.2)",
    "Serum lactate level (mmol/L, 4-12)",
    "Gestational age (weeks, 34-42)",
    "Time to cooling initiation (hours, 0-6 if Cooling = 1, NA if Cooling = 0)"
  )
)

write_csv(codebook, "00_rawdata/codebook.csv")

