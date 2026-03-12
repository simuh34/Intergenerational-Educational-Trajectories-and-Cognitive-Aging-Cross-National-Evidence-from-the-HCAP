library(reshape2)
library(haven)
library(dplyr)

# Load the dataset
elsa_data <- read_stata("D:\\R project\\data\\H_ELSA_g3.dta")

# Select the required columns
elsa_data <- elsa_data %>%
  select(idauniq, idauniqc, rabyear, r8agey, ragender, raracem, r6mheight, r8mheight, rachshlt, raeducl,raedyrs_e, ramomeduage, radadeduage)

# Rename columns
elsa_data <- elsa_data %>%
  rename(
    yob = rabyear,
    age = r8agey,
    sex = ragender,
    race = raracem,
    child_health = rachshlt,
    edu = raeducl,
    Education = raedyrs_e,
    Mother_education = ramomeduage,
    Father_education = radadeduage
  )

# Generate 'height' variable using r8mheight, replace missing values with r6mheight
elsa_data <- elsa_data %>%
  mutate(height = ifelse(is.na(r8mheight), r6mheight, r8mheight))

# Save the modified data
write.csv(elsa_data, "D:\\R project\\Cognition\\ELSA\\elsa_survey.csv")
