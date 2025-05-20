

setwd( "/Users/nam-anhtran/heartofsaigon/data_analysis/comp_exam2019")
pacman::p_load(tidyverse, cmdstanr)

df = read_csv("00_rawdata/Hypoxia_proxy_refined_updated.csv")|>
  mutate(BirthWeight = BirthWeight/1000)

ggplot(df, aes(x = Time, y = Copeptin, group = ID))+
  geom_line(alpha = 0.3)


mutate(df, BirthWeight = ntile(BirthWeight,3),
       BirthWeight = factor(BirthWeight, labels = c("Low", "Medium", "High")))|>
ggplot(aes(x = Time, y = Copeptin, color = BirthWeight, group = ID)) +
  geom_smooth(method = "lm", se = FALSE, aes(group = BirthWeight)) +
  labs(title = "Copeptin Trajectories by Continuous BirthWeight",
       x = "Time (hours)", y = "Copeptin (ng/mL)",
       color = "BirthWeight (g)") +
  theme_minimal() 

mutate(df, Gender = ifelse(Gender==1, "female", "male"))|>
ggplot(aes(x = Time, y = Copeptin, color = Gender, group = ID)) +
  geom_smooth(method = "lm", se = FALSE, aes(group = Gender)) +
  labs(title = "Copeptin Trajectories by Continuous BirthWeight",
       x = "Time (hours)", y = "Copeptin (ng/mL)",
       color = "BirthWeight (g)") +
  theme_minimal() 










