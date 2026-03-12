library(reshape2)
library(haven)
library(dplyr)

# Load the dataset
hrs_data_c <- read_stata("D:\\R project\\data\\H_HRS_c.dta")
hrs_rand <- read_dta('D:\\R project\\data\\randhrs1992_2020v2.dta')

subdat_r <- subset(hrs_rand,
                   select = c(hhid,hhidpn,rabyear, r13agey_e,ragender,r13height, r11height, rameduc, rafeduc,raestrat,raracem,rahispan,raedyrs))

subdat_h <- subset(hrs_data_c,
                   select = c(hhid,hhidpn,raeducl,ramomeducl,radadeducl,h13rural,rachshlt))

hrs_data <- merge(subdat_r, subdat_h, by = c("hhid","hhidpn"))



hrs_data <- hrs_data %>%
  rename(
    ID = hhidpn,
    yob = rabyear,
    age = r13agey_e,
    sex = ragender,
    urbanicity = h13rural,
    Harmonized_education = raeducl,
    Harmonized_mother_education = ramomeducl,
    Harmonized_father_education = radadeducl,
    race = raracem,
    ethnic = rahispan,
    child_health = rachshlt,
    Education = raedyrs,
    Mother_education = rameduc,
    Father_education = rafeduc
    
  )

hrs_data$ID <- as.character(hrs_data$ID)

hrs_data <- hrs_data %>%
  mutate(height = ifelse(is.na(r13height), r11height, r13height))


write.csv(hrs_data, "D:\\R project\\Cognition\\HRS\\hrs_survey.csv")

