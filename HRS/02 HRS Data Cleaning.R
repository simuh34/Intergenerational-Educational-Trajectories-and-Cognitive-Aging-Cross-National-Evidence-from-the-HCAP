library(dplyr)
library(haven)
library(tableone)
library(mice)

# Load the datasets
hrs_survey <- read.csv("D:\\R project\\Cognition\\HRS\\hrs_survey.csv")
hrs_factors <- read_dta("D:\\R project\\data\\hcap\\HRS factor scores.dta")

# Rename 'hhidpn' to 'id_hrs'
hrs_survey <- hrs_survey %>%
  rename(id_hrs = ID)

# Merge the datasets by 'id_hrs'
merged_data_hrs <- merge(hrs_survey, hrs_factors, by = "id_hrs")

# hcap <- read_stata("D:\\R project\\data\\hcap\\interim-HcapHarmoniz-305.dta")
# hrs_hcap <- filter(hcap, study == 1)
# hrs_hcap <- hrs_hcap %>%
#   select(id_hrs, 
#          age)
# names(hrs_hcap) <- c("id_hrs", "age_hcap")
# 
# merged_data_hrs <- merge(merged_data_hrs, hrs_hcap, by = "id_hrs")

# Rename variables with the suffix '_hrs' for the merged data
merged_data_hrs <- merged_data_hrs %>%
  rename(
    r11mheight_hrs = r11height,
    r13mheight_hrs = r13height,
    child_health_hrs = child_health,
    urbanicity_hrs = urbanicity,
    age_hrs = age,
    sex_hrs = sex,
    yob_hrs = yob,
    ethnic_hrs = ethnic,
    raestrat_hrs = raestrat,
    height_hrs = height,
    study_hrs = study,
    race_hrs = race,
    fgcp_hrs = fgcp,
    fgcp_bayes3_hrs = fgcp_bayes3,
    fexf_hrs = fexf,
    fexf_bayes3_hrs = fexf_bayes3,
    flang_hrs = flang,
    flang_bayes3_hrs = flang_bayes3,
    forient_hrs = forient,
    forient_bayes3_hrs = forient_bayes3,
    fmem_hrs = fmem,
    fmem_bayes3_hrs = fmem_bayes3
  )

#age
#merged_data_hrs <- merged_data_hrs[merged_data_hrs$age_hrs >= 65,]
summary(merged_data_hrs)

#race
#Generate a combined race: 0 non-hisp white, 1 non-hisp black, 2 hispanic, 3 other 
merged_data_hrs$Race <- ifelse(merged_data_hrs$race_hrs == 1 & 
                        merged_data_hrs$ethnic_hrs == 0, 0,
                      ifelse(merged_data_hrs$race_hrs == 2 & 
                               merged_data_hrs$ethnic_hrs == 0, 1,
                             ifelse(merged_data_hrs$ethnic_hrs == 1, 2, 3)))

merged_data_hrs$Race <- ifelse(merged_data_hrs$Race == 0, "Non-Hispanic White" , 
                      ifelse(merged_data_hrs$Race == 1, "Non-Hispanic Black",
                             ifelse(merged_data_hrs$Race == 2, "Hispanic","Other")))
merged_data_hrs$Race <-factor(merged_data_hrs$Race, levels = c("Non-Hispanic White","Non-Hispanic Black","Hispanic","Other"))
table(merged_data_hrs$Race)

#urban
merged_data_hrs$urbanicity_hrs <- ifelse(merged_data_hrs$urbanicity_hrs == 1, "Rual" , ifelse(merged_data_hrs$urbanicity_hrs == 0, "Urban", merged_data_hrs$urbanicity_hrs))
merged_data_hrs$urbanicity <- factor(merged_data_hrs$urbanicity_hrs, levels = c("Rual","Urban"))
table(merged_data_hrs$urbanicity)

#sex
merged_data_hrs$sex_hrs <- ifelse(merged_data_hrs$sex_hrs == 1, "Men" , ifelse(merged_data_hrs$sex_hrs == 2, "Women", merged_data_hrs$sex_hrs))
merged_data_hrs$sex <- as.factor(merged_data_hrs$sex_hrs)
table(merged_data_hrs$sex)

####Education####
# participant education
sum(is.na(merged_data_hrs$Education))
sum(is.na(merged_data_hrs$Harmonized_education))
missing_table <- table(
  is.na(merged_data_hrs$Education), 
  is.na(merged_data_hrs$Harmonized_education))
rownames(missing_table) <- c("Raw_edu Not Missing", "Raw_edu Missing")
colnames(missing_table) <- c("Harmonized_edu Not Missing", "Harmonized_edu Missing")
print(missing_table)
#test_1 <- merged_data_hrs[is.na(merged_data_hrs$Education),]

# impute na for harmonized participant education
merged_data_hrs <- merged_data_hrs %>%
  mutate(Harmonized_education = ifelse(
    is.na(Harmonized_education), 
    case_when(
      Education >= 0 & Education <= 9 ~ 1,
      Education > 9 & Education <= 13 ~ 2,
      Education > 13 ~ 3,
      TRUE ~ NA_real_  
    ),
    Harmonized_education
  ))


# parent education
merged_data_hrs$highest_education_parent <- ifelse(
  is.na(merged_data_hrs$Father_education) & is.na(merged_data_hrs$Mother_education), NA, 
  ifelse(
    is.na(merged_data_hrs$Father_education), 
    merged_data_hrs$Mother_education,
    ifelse(
      is.na(merged_data_hrs$Mother_education), 
      merged_data_hrs$Father_education,
      ifelse(
        merged_data_hrs$Father_education > merged_data_hrs$Mother_education, 
        merged_data_hrs$Father_education, 
        merged_data_hrs$Mother_education))))

# harmonized parent education
merged_data_hrs$H_highest_education_parent <- ifelse(
  is.na(merged_data_hrs$Harmonized_father_education) & is.na(merged_data_hrs$Harmonized_mother_education), NA, 
  ifelse(
    is.na(merged_data_hrs$Harmonized_father_education), 
    merged_data_hrs$Harmonized_mother_education,
    ifelse(
      is.na(merged_data_hrs$Harmonized_mother_education), 
      merged_data_hrs$Harmonized_father_education,
      ifelse(
        merged_data_hrs$Harmonized_father_education > merged_data_hrs$Harmonized_mother_education, 
        merged_data_hrs$Harmonized_father_education, 
        merged_data_hrs$Harmonized_mother_education))))

#impute na for harmonized parental education
merged_data_hrs <- merged_data_hrs %>%
  mutate(H_highest_education_parent = ifelse(
    is.na(H_highest_education_parent), 
    case_when(
      highest_education_parent >= 0 & highest_education_parent <= 9 ~ 1,
      highest_education_parent > 9 & highest_education_parent <= 13 ~ 2,
      highest_education_parent > 13 ~ 3,
      TRUE ~ NA_real_  
    ),
    H_highest_education_parent
  ))

sum(is.na(merged_data_hrs$H_highest_education_parent))
sum(is.na(merged_data_hrs$highest_education_parent))
missing_table <- table(
  is.na(merged_data_hrs$highest_education_parent), 
  is.na(merged_data_hrs$H_highest_education_parent))
rownames(missing_table) <- c("Raw_edu Not Missing", "Raw_edu Missing")
colnames(missing_table) <- c("Harmonized_edu Not Missing", "Harmonized_edu Missing")
print(missing_table)

#drop na
hrs_data <- merged_data_hrs %>% 
  filter(!is.na(Education) )
#n = 3343

hrs_data <- hrs_data %>% 
  filter(!is.na(highest_education_parent))
#n = 3143

#drop outcome na
hrs_data <- hrs_data %>% 
  filter(!is.na(fgcp_hrs))
#n = 3143

#missingness values (table2)
missing_values <- colSums(is.na(hrs_data[, c("fgcp_hrs", "fexf_hrs","flang_hrs","forient_hrs","fmem_hrs","age_hrs", "sex_hrs","Education" ,"Father_education", "Mother_education" , "Harmonized_education","Harmonized_mother_education","Harmonized_father_education","Race","urbanicity_hrs")]))
print(missing_values)

#extract complete cases
hrs_complete <- hrs_data[complete.cases(hrs_data), ]
write.csv(hrs_complete, "D:\\R project\\Cognition\\hcap_stata_data\\hrs_complete.csv")

# Covariance missing value imputation 
hrs_data_converted <- hrs_data[, c("age_hrs", "sex_hrs","Race","urbanicity_hrs")]
classes <- sapply(hrs_data_converted, class)
labelled_vars <- names(classes[classes == "labelled"])
hrs_data_converted <- hrs_data_converted %>%
  mutate_if(names(.) %in% c("sex_hrs","race_hrs","urbanicity_hrs"), as.factor)
set.seed(1005)
mice_mod <- mice(hrs_data_converted, method = "cart", m =1, maxit = 5)
imputed_data <- complete(mice_mod)
common_cols <- intersect(names(hrs_data), names(imputed_data))
hrs_data[common_cols] <- imputed_data[common_cols]

#percentile rank of education
## percentile ranking of respondents' education level
hrs_data <- hrs_data %>%
  mutate(percentile_rank_edu = rank(Education,ties.method = c("max"))/length(Education))

## percentile ranking of respondent' parent education level
hrs_data <- hrs_data %>%
  mutate(percentile_rank_pedu =  rank(highest_education_parent,ties.method = c("max"))/length(highest_education_parent))

#demographic summary table
## table1
hrs_data$H_highest_education_parent <- as.factor(hrs_data$H_highest_education_parent)
hrs_data$Harmonized_education <- as.factor(hrs_data$Harmonized_education)
hrs_data$sex_hrs <- as.factor(hrs_data$sex_hrs)
hrs_data$Race <- as.factor(hrs_data$Race)
base_hrs <- CreateTableOne(vars = c("fgcp_hrs", "fexf_hrs","flang_hrs","forient_hrs","fmem_hrs","age_hrs", "sex_hrs", "Harmonized_education","H_highest_education_parent",  "Race","percentile_rank_pedu","percentile_rank_edu","urbanicity_hrs"),
                            data = hrs_data, 
                            includeNA = FALSE)
print(base_hrs, showAllLevels = TRUE) 
base_hrs_summary <- print(base_hrs, showAllLevels = TRUE, catDigits = 3, printToggle = FALSE)
base_hrs_df <- as.data.frame(base_hrs_summary)
base_hrs_df <- cbind(Variable = rownames(base_hrs_df), base_hrs_df)

## table3
hrs_data$education_quan <- cut(
  hrs_data$percentile_rank_edu,
  breaks = quantile(hrs_data$percentile_rank_edu, probs = c(0, 0.25, 0.75, 1)),
  labels = c(1, 2, 3),
  include.lowest = TRUE
)

hrs_data$education_p_quan <- cut(
  hrs_data$percentile_rank_pedu,
  breaks = quantile(hrs_data$percentile_rank_pedu, probs = c(0, 0.25, 0.75, 1)),
  labels = c(1, 2, 3),
  include.lowest = TRUE
)

table(hrs_data$education_quan)
table(hrs_data$education_p_quan)

df_schooling <- subset(hrs_data, education_quan == 1, select = Education)
range(df_schooling$Education)
df_schooling <- subset(hrs_data, education_quan == 2, select = Education)
range(df_schooling$Education)
df_schooling <- subset(hrs_data, education_quan == 3, select = Education)
range(df_schooling$Education)

df_schooling <- subset(hrs_data, education_p_quan == 1, select = highest_education_parent)
range(df_schooling$highest_education_parent)
df_schooling <- subset(hrs_data, education_p_quan == 2, select = highest_education_parent)
range(df_schooling$highest_education_parent)
df_schooling <- subset(hrs_data, education_p_quan == 3, select = highest_education_parent)
range(df_schooling$highest_education_parent)

base_hrs <- CreateTableOne(vars = c("education_quan"),
                           data = hrs_data, 
                           includeNA = FALSE)
print(base_hrs, showAllLevels = TRUE) 

base_hrs_p <- CreateTableOne(vars = c("education_p_quan"),
                           data = hrs_data, 
                           includeNA = FALSE)
print(base_hrs_p, showAllLevels = TRUE) 

#chi-sq by gender
## pr
contingency_table <- table(hrs_data$sex_hrs, hrs_data$education_quan)
chi_square_test <- chisq.test(contingency_table)
print(chi_square_test)
table.prop(hrs_data$sex_hrs, hrs_data$education_quan)

## H
contingency_table <- table(hrs_data$sex_hrs, hrs_data$Harmonized_education)
chi_square_test <- chisq.test(contingency_table)
print(chi_square_test)

freq_table <- table(hrs_data$education_quan,hrs_data$sex_hrs)
print(freq_table)
prop.table(freq_table, margin = 2)

freq_table <- table(hrs_data$Harmonized_education,hrs_data$sex_hrs)
print(freq_table)
prop.table(freq_table, margin = 2)

write.csv(hrs_data,"D:\\R project\\Cognition\\HRS\\hrs_imputed.csv")
