library(reshape2)
library(haven)
library(dplyr)

# Load the dataset
charls_data <- read_stata("D:\\R project\\data\\charls\\H_CHARLS_D_Data.dta")

# load the hcap data (2018)
hcap <- read_stata("D:\\R project\\data\\hcap\\interim-HcapHarmoniz-305.dta")
charls_hcap <- filter(hcap, study == 9)
charls_hcap <- charls_hcap %>%
                select(id_charls, 
                       age,
                       fgcp,
                       fexf,
                       flang,
                       forient,
                       fmem)

# Select the required columns
charls_data <- charls_data %>%
  select(ID, 
         rabyear,
         ragender, 
         r4hukou, 
         h4rural,
         raeduc_c,   #education
         rameduc_c,  #mother education
         rafeduc_c,  #father education
         raeducl,    #harmonized education
         ramomeducl, #harmonized level of mother education
         radadeducl) #harmonized level of mother education

charls_data <- inner_join(charls_data, charls_hcap, by = c("ID" = "id_charls"))

# Rename columns
charls_data <- charls_data %>%
  rename(
    yob = rabyear,
    sex = ragender,
    hukou = r4hukou,
    Education = raeduc_c,
    Mother_education = rameduc_c,
    Father_education = rafeduc_c
  )

# Save the modified data
write.csv(charls_data, "D:\\R project\\Cognition\\CHARLS\\charls_survey.csv")
