library(dplyr)
library(haven)
library(tableone)
library(mice)

# Load the first dataset (elsa_survey.dta)
elsa_data <- read.csv("D:\\R project\\Cognition\\ELSA\\elsa_survey.csv")

# Rename variables
elsa_data <- elsa_data %>%
  rename(id_elsa = idauniq)

# Load the second dataset (ELSA factor scores.dta) and merge with the first dataset by id_elsa
elsa_factors <- read_dta("D:\\R project\\data\\hcap\\ELSA factor scores.dta")

# Perform a 1:1 merge on id_elsa
merged_data_elsa <- inner_join(elsa_data, elsa_factors, by = "id_elsa")

#hcap <- read_stata("D:\\R project\\data\\hcap\\interim-HcapHarmoniz-305.dta")
# elsa_hcap <- filter(hcap, study == 2)
# elsa_hcap <- elsa_hcap %>%
#   select(id_elsa, 
#          age)
# names(elsa_hcap) <- c("id_elsa", "age_hcap")
# merged_data_elsa <- merge(merged_data_elsa, elsa_hcap, by = "id_elsa")

# Rename the variables to add "_elsa" suffix
merged_data_elsa <- merged_data_elsa %>%
  rename(
    idauniqc_elsa = idauniqc, yob_elsa = yob, age_elsa = age, sex_elsa = sex,
    race_elsa = race, Harmonized_education  = edu, child_health_elsa = child_health,
    height_elsa = height,  study_elsa = study,
    fgcp_elsa = fgcp, fgcp_bayes3_elsa = fgcp_bayes3, fexf_elsa = fexf,
    fexf_bayes3_elsa = fexf_bayes3, flang_elsa = flang, flang_bayes3_elsa = flang_bayes3,
    forient_elsa = forient, forient_bayes3_elsa = forient_bayes3,
    fmem_elsa = fmem, fmem_bayes3_elsa = fmem_bayes3
  )

#age
#merged_data_elsa <- merged_data_elsa[merged_data_elsa$age_elsa >= 65,]

#race
#Generate a combined race: 1.white,4.non-white
merged_data_elsa$race <- ifelse(merged_data_elsa$race == 1, "White", ifelse(merged_data_elsa$race == 4, "Non-White", merged_data_elsa$race))
merged_data_elsa$race <- as.factor(merged_data_elsa$race)
merged_data_elsa$race <- relevel(merged_data_elsa$race, ref = "White")
table(merged_data_elsa$race)

#sex
merged_data_elsa$sex_elsa <- ifelse(merged_data_elsa$sex_elsa == 1, "Men" , ifelse(merged_data_elsa$sex_elsa == 2, "Women", merged_data_elsa$sex_elsa))
merged_data_elsa$sex <- as.factor(merged_data_elsa$sex_elsa)
table(merged_data_elsa$sex)

####Education 
#0.none 1.age 14 or under 2.age 15 3.age 16 4.age 17 5.age 18  6.age 19 or over 
# impute na for harmonized participant education
# Notes: age 14 or under were categorize as less than upper secondary 
merged_data_elsa$Harmonized_education <- ifelse(
  is.na(merged_data_elsa$Harmonized_education),
  ifelse(merged_data_elsa$Education == 1, 1,
         ifelse(merged_data_elsa$Education %in% 2:5, 2,
                ifelse(merged_data_elsa$Education == 6, 3,
                       merged_data_elsa$Education))),
  merged_data_elsa$Harmonized_education)


# parent education
merged_data_elsa$highest_education_parent <- ifelse(
  is.na(merged_data_elsa$Father_education) & is.na(merged_data_elsa$Mother_education), NA, 
  ifelse(
    is.na(merged_data_elsa$Father_education), 
    merged_data_elsa$Mother_education,
    ifelse(
      is.na(merged_data_elsa$Mother_education), 
      merged_data_elsa$Father_education,
      ifelse(
        merged_data_elsa$Father_education > merged_data_elsa$Mother_education, 
        merged_data_elsa$Father_education, 
        merged_data_elsa$Mother_education))))

#harmonized
## map harmonized parental education
merged_data_elsa$Harmonized_father_education <- ifelse(merged_data_elsa$Father_education >= 1 & merged_data_elsa$Father_education<= 4, 1, 
                                       ifelse(merged_data_elsa$Father_education == 5 | merged_data_elsa$Father_education == 6, 2, 3)) 
merged_data_elsa$Harmonized_mother_education <- ifelse(merged_data_elsa$Mother_education >= 1 & merged_data_elsa$Mother_education<= 4, 1, 
                                       ifelse(merged_data_elsa$Mother_education == 5 | merged_data_elsa$Mother_education == 6, 2, 3))  
#harmonized highest parental education
merged_data_elsa$H_highest_education_parent <- ifelse(
  is.na(merged_data_elsa$Harmonized_father_education) & is.na(merged_data_elsa$Harmonized_mother_education), 
  NA, 
  ifelse(
    is.na(merged_data_elsa$Harmonized_father_education), 
    merged_data_elsa$Harmonized_mother_education,
    ifelse(
      is.na(merged_data_elsa$Harmonized_mother_education), 
      merged_data_elsa$Harmonized_father_education,
      ifelse(
        merged_data_elsa$Harmonized_father_education > merged_data_elsa$Harmonized_mother_education, 
        merged_data_elsa$Harmonized_father_education, 
        merged_data_elsa$Harmonized_mother_education
      ))))

# impute na for harmonized parental education
merged_data_elsa$H_highest_education_parent <- ifelse(
  is.na(merged_data_elsa$H_highest_education_parent),
  ifelse(merged_data_elsa$highest_education_parent == 1, 1,
         ifelse(merged_data_elsa$highest_education_parent %in% 2:5, 2,
                ifelse(merged_data_elsa$highest_education_parent == 6, 3,
                       merged_data_elsa$highest_education_parent))),
  merged_data_elsa$H_highest_education_parent)

#check missingness of education
sum(is.na(merged_data_elsa$Education))
sum(is.na(merged_data_elsa$Harmonized_education))
missing_table <- table(
  is.na(merged_data_elsa$Education), 
  is.na(merged_data_elsa$Harmonized_education))
rownames(missing_table) <- c("Raw_edu Not Missing", "Raw_edu Missing")
colnames(missing_table) <- c("Harmonized_edu Not Missing", "Harmonized_edu Missing")
print(missing_table)

#check missingness of parental education
sum(is.na(merged_data_elsa$H_highest_education_parent))
sum(is.na(merged_data_elsa$highest_education_parent))
missing_table <- table(
  is.na(merged_data_elsa$highest_education_parent), 
  is.na(merged_data_elsa$H_highest_education_parent))
rownames(missing_table) <- c("Raw_edu Not Missing", "Raw_edu Missing")
colnames(missing_table) <- c("Harmonized_edu Not Missing", "Harmonized_edu Missing")
print(missing_table)

#drop na
elsa_data <- merged_data_elsa %>% 
  filter(!is.na(Education) & !is.na(Harmonized_education))
#n = 1253
elsa_data <- elsa_data %>% 
  filter(!is.na(H_highest_education_parent) & !is.na(highest_education_parent))
#n = 1244

#missingness values (table2)
missing_values <- colSums(is.na(elsa_data[, c("fgcp_elsa","fexf_elsa", "flang_elsa", "forient_elsa", "fmem_elsa", "age_elsa","yob_elsa", "sex_elsa","Education" ,"Father_education", "Mother_education" , "Harmonized_education","Harmonized_father_education","Harmonized_mother_education" , "race")]))
print(missing_values)

#extract complete cases
elsa_complete <- elsa_data[complete.cases(elsa_data), ]
write.csv(elsa_complete, "D:\\R project\\Cognition\\hcap_stata_data\\elsa_complete.csv")

#Covariance missing value imputation
elsa_data_converted <- elsa_data[, c("age_elsa", "sex_elsa","race")]
classes <- sapply(elsa_data_converted, class)
labelled_vars <- names(classes[classes == "labelled"])
elsa_data_converted <- elsa_data_converted %>%
  mutate_if(names(.) %in% c("sex_elsa","race_elsa"), as.factor)
set.seed(1005)
mice_mod <- mice(elsa_data_converted, method = "cart", m =1, maxit = 5)
imputed_data <- complete(mice_mod)
common_cols <- intersect(names(elsa_data), names(imputed_data))
elsa_data[common_cols] <- imputed_data[common_cols]

#percentile rank of education
## percentile ranking of respondents' education level
elsa_data <- elsa_data %>%
  mutate(percentile_rank_edu = rank(Education,ties.method = c("max"))/length(Education))

## percentile ranking of respondent' parent education level
elsa_data <- elsa_data %>%
  mutate(percentile_rank_pedu =  rank(highest_education_parent,ties.method = c("max"))/length(highest_education_parent))

#demographic summary table
## table1
elsa_data$H_highest_education_parent <- as.factor(elsa_data$H_highest_education_parent)
elsa_data$Harmonized_education <- as.factor(elsa_data$Harmonized_education)
elsa_data$sex_elsa <- as.factor(elsa_data$sex_elsa)
elsa_data$race <- as.factor(elsa_data$race)
base_elsa <- CreateTableOne(vars = c("fgcp_elsa","fexf_elsa", "flang_elsa", "forient_elsa", "fmem_elsa", "age_elsa", "sex_elsa", "Harmonized_education","H_highest_education_parent",  "race","percentile_rank_pedu","percentile_rank_edu"),
                           data = elsa_data, 
                           includeNA = FALSE)
print(base_elsa, showAllLevels = TRUE) 
base_elsa_summary <- print(base_elsa, showAllLevels = TRUE, catDigits = 3, printToggle = FALSE)
base_elsa_df <- as.data.frame(base_elsa_summary)
base_elsa_df <- cbind(Variable = rownames(base_elsa_df), base_elsa_df)

## table3
elsa_data$education_quan <- cut(
  elsa_data$percentile_rank_edu,
  breaks = quantile(elsa_data$percentile_rank_edu, probs = c(0, 0.25, 0.75, 1)),
  labels = c(1, 2, 3),
  include.lowest = TRUE
)

elsa_data$education_p_quan <- cut(
  elsa_data$percentile_rank_pedu,
  breaks = quantile(elsa_data$percentile_rank_pedu, probs = c(0, 0.25, 0.75, 1)),
  labels = c(1, 2, 3),
  include.lowest = TRUE
)
table(elsa_data$education_quan)
table(elsa_data$education_p_quan)

df_schooling <- subset(elsa_data, education_quan == 1, select = Education)
range(df_schooling$Education)
df_schooling <- subset(elsa_data, education_quan == 2, select = Education)
range(df_schooling$Education)
df_schooling <- subset(elsa_data, education_quan == 3, select = Education)
range(df_schooling$Education)

df_schooling <- subset(elsa_data, education_p_quan == 1, select = highest_education_parent)
range(df_schooling$highest_education_parent)
df_schooling <- subset(elsa_data, education_p_quan == 2, select = highest_education_parent)
range(df_schooling$highest_education_parent)
df_schooling <- subset(elsa_data, education_p_quan == 3, select = highest_education_parent)
range(df_schooling$highest_education_parent)

base_elsa <- CreateTableOne(vars = c("education_quan"),
                           data = elsa_data, 
                           includeNA = FALSE)
print(base_elsa, showAllLevels = TRUE) 

base_elsa <- CreateTableOne(vars = c("education_p_quan"),
                           data = elsa_data, 
                           includeNA = FALSE)
print(base_elsa, showAllLevels = TRUE) 

#chi-sq by gender
## pr
contingency_table <- table(elsa_data$sex_elsa, elsa_data$education_quan)
chi_square_test <- chisq.test(contingency_table)
print(chi_square_test)

## H
contingency_table <- table(elsa_data$sex_elsa, elsa_data$Harmonized_education)
chi_square_test <- chisq.test(contingency_table)
print(chi_square_test)

freq_table <- table(elsa_data$education_quan,elsa_data$sex_elsa)
print(freq_table)
prop.table(freq_table, margin = 2)

freq_table <- table(elsa_data$Harmonized_education,elsa_data$sex_elsa)
print(freq_table)
prop.table(freq_table, margin = 2)

write.csv(elsa_data,"D:\\R project\\Cognition\\elsa\\elsa_imputed.csv")
