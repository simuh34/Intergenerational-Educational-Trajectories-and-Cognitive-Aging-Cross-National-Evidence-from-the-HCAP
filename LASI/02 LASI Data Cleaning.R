# Load necessary libraries
library(dplyr)
library(haven)
library(tableone)
library(mice)

# Load the first dataset (lasi_survey.dta)
lasi_data <- read.csv("D:\\R project\\Cognition\\LASI\\LASI_edu.csv")

# Convert id_lasidad to a string variable (similar to tostring in Stata)
lasi_data$id_lasidad <- as.character(lasi_data$id_lasidad)

# Load the second dataset (LASI factor scores.dta) and merge with the first dataset by id_lasidad
lasi_factors <- read_dta("D:\\R project\\data\\hcap\\lasi factor scores.dta")

combined_data <- inner_join(lasi_data, lasi_factors, by = "id_lasidad")

# extract age from hcap
hcap_lasi <- read_dta("D:\\R project\\data\\hcap\\H_DAD.dta")
hcap_lasi <- hcap_lasi[,c("r1agey", "prim_key")]
names(hcap_lasi) <- c("age_lasi", "id_lasidad")

combined_data$id_lasidad <- as.character(combined_data$id_lasidad)
combined_data <- left_join(combined_data, hcap_lasi, by = "id_lasidad")

#hcap <- read_stata("D:\\R project\\data\\hcap\\interim-HcapHarmoniz-305.dta")
# lasi_hcap <- filter(hcap, study == 4)
# lasi_hcap <- lasi_hcap %>%
#   select(id_lasidad,
#          age)
# names(lasi_hcap) <- c("id_lasidad", "age_hcap")
# combined_data <- left_join(combined_data, lasi_hcap, by = "id_lasidad")


# Rename the varileft_join()# Rename the variables to add "_lasi" suffix
merged_data_lasi <- combined_data %>%
  rename(
    yob_lasi = yob, sex_lasi = sex, urbanicity_lasi = urbanicity,
    height_lasi = height, child_health_lasi = child_health,
    study_lasi = study, fgcp_lasi = fgcp, fgcp_bayes3_lasi = fgcp_bayes3,
    fexf_lasi = fexf, fexf_bayes3_lasi = fexf_bayes3, flang_lasi = flang,
    flang_bayes3_lasi = flang_bayes3, forient_lasi = forient, forient_bayes3_lasi = forient_bayes3,
    fmem_lasi = fmem, fmem_bayes3_lasi = fmem_bayes3
  )

#age
merged_data_lasi <- merged_data_lasi[merged_data_lasi$age_lasi >= 65,]

#Generate a combined urbanicity_lasi: 1.caste village, 0.urban community
merged_data_lasi$caste <- ifelse(merged_data_lasi$caste == 1|merged_data_lasi$caste == 2, 'Scheduled caste or scheduled tribe',ifelse(merged_data_lasi$caste == 3|merged_data_lasi$caste == 4, 'Other',merged_data_lasi$caste))
table(merged_data_lasi$caste)

#sex
merged_data_lasi$sex_lasi <- ifelse(merged_data_lasi$sex_lasi == 1, "Men" , ifelse(merged_data_lasi$sex_lasi == 2, "Women", merged_data_lasi$sex_lasi))
merged_data_lasi$sex <- as.factor(merged_data_lasi$sex_lasi)
table(merged_data_lasi$sex)

#rural
merged_data_lasi$urbanicity_lasi <- ifelse(merged_data_lasi$urbanicity_lasi == 1, "Rural" , ifelse(merged_data_lasi$urbanicity_lasi == 0, "Urban", merged_data_lasi$urbanicity_lasi))
merged_data_lasi$urbanicity <- as.factor(merged_data_lasi$urbanicity_lasi)
table(merged_data_lasi$urbanicity)

####Education####
# participant education
sum(is.na(merged_data_lasi$Education))
sum(is.na(merged_data_lasi$Harmonized_education))
missing_table <- table(
  is.na(merged_data_lasi$Education), 
  is.na(merged_data_lasi$Harmonized_education))

# parent education
merged_data_lasi <- subset(merged_data_lasi, !(is.na(Father_education) & is.na(Mother_education)))
merged_data_lasi$highest_education_parent<- ifelse(
  is.na(merged_data_lasi$Father_education), merged_data_lasi$Mother_education,
  ifelse(
    is.na(merged_data_lasi$Mother_education), merged_data_lasi$Father_education,
    ifelse(merged_data_lasi$Father_education> merged_data_lasi$Mother_education, merged_data_lasi$Father_education, merged_data_lasi$Mother_education)
  ))

# harmonized
merged_data_lasi$H_highest_education_parent<- ifelse(
  is.na(merged_data_lasi$Harmonized_father_education), merged_data_lasi$Harmonized_mother_education,
  ifelse(
    is.na(merged_data_lasi$Harmonized_mother_education), merged_data_lasi$Harmonized_father_education,
    ifelse(merged_data_lasi$Harmonized_father_education> merged_data_lasi$Harmonized_mother_education, merged_data_lasi$Harmonized_father_education, merged_data_lasi$Harmonized_mother_education)
  ))

sum(is.na(merged_data_lasi$H_highest_education_parent))
sum(is.na(merged_data_lasi$highest_education_parent))

#drop na
lasi_data <- merged_data_lasi %>% 
  filter(!is.na(Education) )
#n = 2864

lasi_data <- lasi_data %>% 
  filter(!is.na(highest_education_parent))
#n = 2864

#missingness values (table2)
missing_values <- colSums(is.na(lasi_data[, c("fgcp_lasi","fexf_lasi","flang_lasi","forient_lasi","fmem_lasi", "age_lasi", "sex_lasi","Education" ,"Father_education", "Mother_education" , "Harmonized_education","Harmonized_mother_education","Harmonized_father_education", "caste","urbanicity_lasi")]))
print(missing_values)

#extract complete cases
lasi_complete <-lasi_data[complete.cases(lasi_data), ]
write.csv(lasi_complete, "D:\\R project\\Cognition\\hcap_stata_data\\lasi_complete.csv")

# Covariance missing value imputation
lasi_data_converted <- lasi_data[, c("age_lasi", "sex_lasi","caste")]
classes <- sapply(lasi_data_converted, class)
labelled_vars <- names(classes[classes == "labelled"])
lasi_data_converted <- lasi_data_converted %>%
  mutate_if(names(.) %in% c("sex_lasi","caste"), as.factor)
set.seed(1005)
mice_mod <- mice(lasi_data_converted, method = "cart", m =1, maxit = 5)
imputed_data <- complete(mice_mod)
common_cols <- intersect(names(lasi_data), names(imputed_data))
lasi_data[common_cols] <- imputed_data[common_cols]

#percentile rank of education
## percentile ranking of respondents' education level
lasi_data <- lasi_data %>%
  mutate(percentile_rank_edu = rank(Education,ties.method = c("max"))/length(Education))

## percentile ranking of respondent' parent education level
lasi_data <- lasi_data %>%
  mutate(percentile_rank_pedu =  rank(highest_education_parent,ties.method = c("max"))/length(highest_education_parent))

#demographic summary table
## table1
lasi_data$H_highest_education_parent <- as.factor(lasi_data$H_highest_education_parent)
lasi_data$Harmonized_education <- as.factor(lasi_data$Harmonized_education)
lasi_data$sex_lasi <- as.factor(lasi_data$sex_lasi)
lasi_data$caste <- as.factor(lasi_data$caste)
base_lasi <- CreateTableOne(vars = c("fgcp_lasi","fexf_lasi","flang_lasi","forient_lasi","fmem_lasi", "age_lasi", "sex_lasi", "Harmonized_education","H_highest_education_parent",  "caste", "urbanicity_lasi","percentile_rank_pedu","percentile_rank_edu","education_mobility"),
                           data = lasi_data, 
                           includeNA = FALSE)
print(base_lasi, showAllLevels = TRUE) 
base_lasi_summary <- print(base_lasi, showAllLevels = TRUE, catDigits = 3, printToggle = FALSE)
base_lasi_df <- as.data.frame(base_lasi_summary)
base_lasi_df <- cbind(Variable = rownames(base_lasi_df), base_lasi_df)

## table3
quantiles_r <- quantile(lasi_data$percentile_rank_edu, probs = c(0.25, 0.75))
quantiles_p <- quantile(lasi_data$percentile_rank_pedu, probs = c(0.25, 0.75))

lasi_data$education_quan <- cut(lasi_data$percentile_rank_edu,
                               breaks = c(-Inf, quantiles_r, Inf),
                               labels = c(1, 2, 3),
                               right = TRUE)
lasi_data$education_p_quan <- cut(lasi_data$percentile_rank_pedu,
                                 breaks = c(-Inf, quantiles_p, Inf),
                                 labels = c(1, 2, 3),
                                 right = TRUE)
table(lasi_data$education_quan)
table(lasi_data$education_p_quan)

df_schooling <- subset(lasi_data, education_quan == 1, select = Education)
range(df_schooling$Education)
df_schooling <- subset(lasi_data, education_quan == 2, select = Education)
range(df_schooling$Education)
df_schooling <- subset(lasi_data, education_quan == 3, select = Education)
range(df_schooling$Education)

df_schooling <- subset(lasi_data, education_p_quan == 1, select = highest_education_parent)
range(df_schooling$highest_education_parent)
df_schooling <- subset(lasi_data, education_p_quan == 2, select = highest_education_parent)
range(df_schooling$highest_education_parent)
df_schooling <- subset(lasi_data, education_p_quan == 3, select = highest_education_parent)
range(df_schooling$highest_education_parent)

base_lasi <- CreateTableOne(vars = c("education_quan"),
                           data = lasi_data, 
                           includeNA = FALSE)
print(base_lasi, showAllLevels = TRUE) 

base_lasi <- CreateTableOne(vars = c("education_p_quan"),
                           data = lasi_data, 
                           includeNA = FALSE)
print(base_lasi, showAllLevels = TRUE)

#chi-sq by gender
## pr
contingency_table <- table(lasi_data$sex_lasi, lasi_data$education_quan)
chi_square_test <- chisq.test(contingency_table)
print(chi_square_test)

## H
contingency_table <- table(lasi_data$sex_lasi, lasi_data$Harmonized_education)
chi_square_test <- chisq.test(contingency_table)
print(chi_square_test)

freq_table <- table(lasi_data$education_quan,lasi_data$sex_lasi)
print(freq_table)
prop.table(freq_table, margin = 2)

freq_table <- table(lasi_data$Harmonized_education,lasi_data$sex_lasi)
print(freq_table)
prop.table(freq_table, margin = 2)

write.csv(lasi_data,"D:\\R project\\Cognition\\LASI\\lasi_imputed.csv")
