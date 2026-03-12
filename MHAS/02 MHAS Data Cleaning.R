# Load necessary libraries
library(dplyr)
library(haven)
library(tableone)

# Load the first dataset (mhas_survey.dta)
mhas_data <- read.csv("D:\\R project\\Cognition\\MHAS\\mhas_survey.csv")

# Rename variables
mhas_data <- mhas_data %>%
  rename(id_mexcog = rahhidnp)

# Convert id_mexcog to numeric (similar to destring in Stata)
mhas_data$id_mexcog <- as.numeric(mhas_data$id_mexcog)

# Load the second dataset (MHAS factor scores.dta) and merge with the first dataset by id_mexcog
mhas_factors <- read_dta("D:\\R project\\data\\hcap\\MHAS factor scores.dta")

# Perform a 1:1 merge on id_mexcog
combined_data <- inner_join(mhas_data, mhas_factors, by = "id_mexcog")

# hcap <- read_stata("D:\\R project\\data\\hcap\\interim-HcapHarmoniz-305.dta")
# mhas_hcap <- filter(hcap, study == 3)
# mhas_hcap <- mhas_hcap %>%
#   select(id_mexcog, 
#          age)
# names(mhas_hcap) <- c("id_mexcog", "age_hcap")
# combined_data <- merge(combined_data, mhas_hcap, by = "id_mexcog")

# Rename the variables to add "_mhas" suffix
merged_data_mhas <- combined_data %>%
  rename(
    unhhidnp_mhas = unhhidnp, id_mexcog_mhas = id_mexcog, yob_mhas = yob, age_mhas = age,
    sex_mhas = sex, urbanicity_mhas = urbanicity, height_mhas = height,
    study_mhas = study, fgcp_mhas = fgcp,
    fgcp_bayes3_mhas = fgcp_bayes3, fexf_mhas = fexf, fexf_bayes3_mhas = fexf_bayes3,
    flang_mhas = flang, flang_bayes3_mhas = flang_bayes3, forient_mhas = forient,
    forient_bayes3_mhas = forient_bayes3, fmem_mhas = fmem, fmem_bayes3_mhas = fmem_bayes3
  )

#age
merged_data_mhas <- merged_data_mhas[merged_data_mhas$age_mhas >= 65,]

#urbanicity_mhas
#Generate a combined urbanicity_mhas: 1.rural, 0.urban 
merged_data_mhas$rural <- ifelse(merged_data_mhas$urbanicity_mhas == 1, 'rural',ifelse(merged_data_mhas$urbanicity_mhas == 0, 'urban',merged_data_mhas$urbanicity_mhas))
table(merged_data_mhas$rural)

#sex
merged_data_mhas$sex_mhas <- ifelse(merged_data_mhas$sex_mhas == 1, "Men" , ifelse(merged_data_mhas$sex_mhas == 2, "Women", merged_data_mhas$sex_mhas))
merged_data_mhas$sex <- as.factor(merged_data_mhas$sex_mhas)
table(merged_data_mhas$sex)

####Education####
# participant education
sum(is.na(merged_data_mhas$Education))
sum(is.na(merged_data_mhas$Harmonized_education))
missing_table <- table(
  is.na(merged_data_mhas$Education), 
  is.na(merged_data_mhas$Harmonized_education))
rownames(missing_table) <- c("Raw_edu Not Missing", "Raw_edu Missing")
colnames(missing_table) <- c("Harmonized_edu Not Missing", "Harmonized_edu Missing")
print(missing_table)

# parent education(equal use as harmonized and education)
merged_data_mhas$highest_education_parent<- ifelse(
  is.na(merged_data_mhas$Father_education), merged_data_mhas$Mother_education,
  ifelse(
    is.na(merged_data_mhas$Mother_education), merged_data_mhas$Father_education,
    ifelse(merged_data_mhas$Father_education> merged_data_mhas$Mother_education, merged_data_mhas$Father_education, merged_data_mhas$Mother_education)
  ))

merged_data_mhas$H_highest_education_parent <- factor(ifelse(merged_data_mhas$highest_education_parent== 1|merged_data_mhas$highest_education_parent== 2, 1, ifelse(merged_data_mhas$highest_education_parent== 3,2,ifelse(merged_data_mhas$highest_education_parent== 4, 3, merged_data_mhas$highest_education_parent))))

sum(is.na(merged_data_mhas$H_highest_education_parent))
sum(is.na(merged_data_mhas$highest_education_parent))
missing_table <- table(
  is.na(merged_data_mhas$highest_education_parent), 
  is.na(merged_data_mhas$H_highest_education_parent))
rownames(missing_table) <- c("Raw_edu Not Missing", "Raw_edu Missing")
colnames(missing_table) <- c("Harmonized_edu Not Missing", "Harmonized_edu Missing")
print(missing_table)

mhas_data <- merged_data_mhas %>% 
  filter(!is.na(Education) & !is.na(Harmonized_education))
#n = 1245
mhas_data <- mhas_data %>% 
  filter(!is.na(H_highest_education_parent) & !is.na(highest_education_parent))
#n = 1118


#missingness values (table2)
missing_values <- colSums(is.na(mhas_data[, c("fgcp_mhas","fexf_mhas","flang_mhas","forient_mhas","fmem_mhas", "age_mhas", "sex_mhas","Education" ,"Father_education", "Mother_education" , "rural")]))
print(missing_values)

#extract complete cases
mhas_complete <- mhas_data[complete.cases(mhas_data), ]
write.csv(mhas_complete, "D:\\R project\\Cognition\\hcap_stata_data\\mhas_complete.csv")

# Covariance missing value imputation 
# mhas_data_converted <- mhas_data[, c("age_mhas", "sex_mhas","Race")]
# classes <- sapply(mhas_data_converted, class)
# labelled_vars <- names(classes[classes == "labelled"])
# mhas_data_converted <- mhas_data_converted %>%
#   mutate_if(names(.) %in% c("sex_mhas","race_mhas"), as.factor)
# set.seed(1005)
# mice_mod <- mice(mhas_data_converted, method = "cart", m =1, maxit = 5)
# imputed_data <- complete(mice_mod)
# common_cols <- intersect(names(mhas_data), names(imputed_data))
# mhas_data[common_cols] <- imputed_data[common_cols]

#percentile rank of education
## percentile ranking of respondents' education level
mhas_data <- mhas_data %>%
  mutate(percentile_rank_edu = rank(Education,ties.method = c("max"))/length(Education))

## percentile ranking of respondent' parent education level
mhas_data <- mhas_data %>%
  mutate(percentile_rank_pedu =  rank(highest_education_parent,ties.method = c("max"))/length(highest_education_parent))

#demographic summary table
## table1
#mhas_data$H_highest_education_parent <- as.factor(mhas_data$H_highest_education_parent)
mhas_data$Harmonized_education <- as.factor(mhas_data$Harmonized_education)
mhas_data$sex_mhas <- as.factor(mhas_data$sex_mhas)
mhas_data$rural <- as.factor(mhas_data$rural)
base_mhas <- CreateTableOne(vars = c("fgcp_mhas","fexf_mhas","flang_mhas","forient_mhas","fmem_mhas", "age_mhas", "sex_mhas", "Harmonized_education","H_highest_education_parent","rural","percentile_rank_pedu","percentile_rank_edu","education_mobility"),
                           data = mhas_data, 
                           includeNA = FALSE)
print(base_mhas, showAllLevels = TRUE) 
base_mhas_summary <- print(base_mhas, showAllLevels = TRUE, catDigits = 3, printToggle = FALSE)
base_mhas_df <- as.data.frame(base_mhas_summary)
base_mhas_df <- cbind(Variable = rownames(base_mhas_df), base_mhas_df)

## table3
quantiles_r <- quantile(mhas_data$percentile_rank_edu, probs = c(0.25, 0.75))
quantiles_p <- quantile(mhas_data$percentile_rank_pedu, probs = c(0.25, 0.75))

mhas_data$education_quan <- cut(mhas_data$percentile_rank_edu,
                               breaks = c(-Inf, quantiles_r, Inf),
                               labels = c(1, 2, 3),
                               right = TRUE)
mhas_data$education_p_quan <- cut(mhas_data$percentile_rank_pedu,
                                 breaks = c(-Inf, quantiles_p, Inf),
                                 labels = c(1, 2, 3),
                                 right = TRUE)
table(mhas_data$education_quan)
table(mhas_data$education_p_quan)

df_schooling <- subset(mhas_data, education_quan == 1, select = Education)
range(df_schooling$Education)
df_schooling <- subset(mhas_data, education_quan == 2, select = Education)
range(df_schooling$Education)
df_schooling <- subset(mhas_data, education_quan == 3, select = Education)
range(df_schooling$Education)

df_schooling <- subset(mhas_data, education_p_quan == 1, select = highest_education_parent)
range(df_schooling$highest_education_parent)
df_schooling <- subset(mhas_data, education_p_quan == 2, select = highest_education_parent)
range(df_schooling$highest_education_parent)
df_schooling <- subset(mhas_data, education_p_quan == 3, select = highest_education_parent)
range(df_schooling$highest_education_parent)

base_mhas <- CreateTableOne(vars = c("education_quan"),
                           data = mhas_data, 
                           includeNA = FALSE)
print(base_mhas, showAllLevels = TRUE) 

base_mhas <- CreateTableOne(vars = c("education_p_quan"),
                           data = mhas_data, 
                           includeNA = FALSE)
print(base_mhas, showAllLevels = TRUE) 

#chi-sq by gender
## pr
contingency_table <- table(mhas_data$sex_mhas, mhas_data$education_quan)
chi_square_test <- chisq.test(contingency_table)
print(chi_square_test)

## H
contingency_table <- table(mhas_data$sex_mhas, mhas_data$Harmonized_education)
chi_square_test <- chisq.test(contingency_table)
print(chi_square_test)

freq_table <- table(mhas_data$education_quan,mhas_data$sex_mhas)
print(freq_table)
prop.table(freq_table, margin = 2)

freq_table <- table(mhas_data$Harmonized_education,mhas_data$sex_mhas)
print(freq_table)
prop.table(freq_table, margin = 2)

write.csv(mhas_data,"D:\\R project\\Cognition\\mhas\\mhas_imputed.csv")

