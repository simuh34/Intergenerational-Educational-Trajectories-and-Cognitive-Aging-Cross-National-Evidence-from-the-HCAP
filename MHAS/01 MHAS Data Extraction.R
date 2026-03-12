library(reshape2)
library(haven)
library(dplyr)

# Set working directory and load data
data <- read_dta("D:\\R project\\data\\H_MHAS_c.dta")

# Select relevant columns
data <- data %>%
  select(unhhidnp, rahhidnp, rabyear, r4agey, ragender, r4height, r3height, h4rural, raeducl, raedyrs,rameduc_m, rafeduc_m) 

# Rename columns
data <- data %>%
  rename(yob = rabyear, age = r4agey, sex = ragender, urbanicity = h4rural,Harmonized_education = raeducl,Education = raedyrs,Mother_education =rameduc_m, Father_education = rafeduc_m)

# Generate height
data <- data %>%
  mutate(height = ifelse(!is.na(r4height), r4height, r3height))



# Save the cleaned data
write.csv(data, "D:\\R project\\Cognition\\MHAS\\mhas_survey.csv")
