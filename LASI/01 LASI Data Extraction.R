library(dplyr)
library(haven)

# Load data
data_lasi <- read_dta("D:\\R project\\data\\H_LASI_a3.dta")

# Select relevant columns and rename them
data <- data_lasi %>%
  select(prim_key, ragender,radadeducl, ramomeducl, raeducl, rachshlta, r1mheight, ragender, hh1rural, rabyear,r1mheight,r1agey,raedyrs,rameduc_l,rafeduc_l, r1caste) %>%
  rename(
    id_lasidad = prim_key,
    sex = ragender,
    Harmonized_father_education = radadeducl,
    Harmonized_mother_education = ramomeducl,
    Harmonized_education = raeducl,
    height = r1mheight,
    child_health = rachshlta,
    urbanicity = hh1rural,
    yob = rabyear,
    height = r1mheight,
    age = r1agey,
    Education = raedyrs,
    Mother_education = rameduc_l,
    Father_education = rafeduc_l,
    caste = r1caste)
  # ) %>%
  # mutate(id_lasidad = as.integer(id_lasidad))  # Ensure id is an integer

# Save the cleaned data
write.csv(data, "D:\\R project\\Cognition\\LASI\\LASI_edu.csv")
