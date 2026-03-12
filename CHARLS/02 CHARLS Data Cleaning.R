# Load necessary libraries
library(dplyr)
library(haven)
library(tableone)
library(mice)

# Load the first dataset (charls_survey.dta)
charls_data <- read.csv("D:\\R project\\Cognition\\CHARLS\\charls_survey.csv")

# Rename variables
charls_data <- charls_data %>%
  rename(id_charls = ID)

# Convert id_charls to numeric (similar to destring in Stata)
charls_data$id_charls <- as.numeric(charls_data$id_charls)
summary(charls_data$age)

hcap <- read_stata("D:\\R project\\data\\hcap\\interim-HcapHarmoniz-305.dta")
charls_hcap <- filter(hcap, study == 9)
charls_hcap <- charls_hcap %>%
  select(id_charls, 
         age)
names(charls_hcap) <- c("id_charls", "age_hcap")

charls_data$id_charls <- as.character(charls_hcap$id_charls)
combined_data <- inner_join(charls_data, charls_hcap, by = "id_charls")

merged_data_charls <- combined_data %>%
  rename(
    yob_charls = yob, age_charls = age_hcap,Harmonized_education = raeducl, 
    Harmonized_mother_education = ramomeducl,Harmonized_father_education = radadeducl,
    sex_charls = sex, urbanicity_charls = h4rural, fgcp_charls = fgcp,
    fexf_charls = fexf, flang_charls = flang, forient_charls = forient,
    fmem_charls = fmem)

#age
merged_data_charls <- merged_data_charls[merged_data_charls$age_charls >= 65,]

#urbanicity_charls
#Generate a combined urbanicity_charls: 1.rural, 0.urban 
merged_data_charls$rural <- ifelse(merged_data_charls$urbanicity_charls == 1, 'rural',ifelse(merged_data_charls$urbanicity_charls == 0, 'urban',merged_data_charls$urbanicity_charls))
table(merged_data_charls$rural)

#sex
table(merged_data_charls$sex_charls)
merged_data_charls$sex_charls <- ifelse(merged_data_charls$sex_charls == 1, "Men" , ifelse(merged_data_charls$sex_charls == 2, "Women", merged_data_charls$sex_charls))
merged_data_charls$sex <- as.factor(merged_data_charls$sex_charls)
table(merged_data_charls$sex)

#hukou
merged_data_charls$hukou <- ifelse(merged_data_charls$hukou==1|merged_data_charls$hukou==4, "Agricultual", "Non-agricultual")

####Education####
# participant education
sum(is.na(merged_data_charls$Education))

missing_table <- table(
  is.na(merged_data_charls$Education), 
  is.na(merged_data_charls$Harmonized_education))
print(missing_table)

# parent education
sum(is.na(merged_data_charls$Father_education))
sum(is.na(merged_data_charls$Mother_education))
merged_data_charls$highest_education_parent<- ifelse(
  is.na(merged_data_charls$Father_education), merged_data_charls$Mother_education,
  ifelse(
    is.na(merged_data_charls$Mother_education), merged_data_charls$Father_education,
    ifelse(merged_data_charls$Father_education> merged_data_charls$Mother_education, merged_data_charls$Father_education, merged_data_charls$Mother_education)
  ))
sum(is.na(merged_data_charls$highest_education_parent))

####H_education####
# participant education
sum(is.na(merged_data_charls$Harmonized_education))

# parent education
sum(is.na(merged_data_charls$Harmonized_father_education))
sum(is.na(merged_data_charls$Harmonized_mother_education))
merged_data_charls$H_highest_education_parent<- ifelse(
  is.na(merged_data_charls$Harmonized_father_education), merged_data_charls$Harmonized_mother_education,
  ifelse(
    is.na(merged_data_charls$Harmonized_mother_education), merged_data_charls$Harmonized_father_education,
    ifelse(merged_data_charls$Harmonized_father_education> merged_data_charls$Harmonized_mother_education, merged_data_charls$Harmonized_father_education, merged_data_charls$Harmonized_mother_education)
  ))

sum(is.na(merged_data_charls$H_highest_education_parent))
sum(is.na(merged_data_charls$highest_education_parent))
missing_table <- table(
  is.na(merged_data_charls$highest_education_parent), 
  is.na(merged_data_charls$H_highest_education_parent))
rownames(missing_table) <- c("Raw_edu Not Missing", "Raw_edu Missing")
colnames(missing_table) <- c("Harmonized_edu Not Missing", "Harmonized_edu Missing")
print(missing_table)

charls_data <- merged_data_charls %>% 
  filter(!is.na(Education))
#n = 6538
charls_data <- charls_data %>% 
  filter(!is.na(highest_education_parent))
#n = 6480 


#missingness values (table2)

missing_values <- colSums(is.na(charls_data[, c("fgcp_charls","fexf_charls","flang_charls","forient_charls","fmem_charls", "age_charls", "sex_charls","Education" ,"Father_education", "Mother_education" , "rural", "hukou")]))
print(missing_values)

#extract complete cases
charls_complete <- charls_data[complete.cases(charls_data), ]
write.csv(charls_complete, "D:\\R project\\Cognition\\hcap_stata_data\\charls_complete.csv")


# Covariance missing value imputation
charls_data_converted <- charls_data[, c("age_charls", "sex_charls","hukou", "rural")]
classes <- sapply(charls_data_converted, class)
labelled_vars <- names(classes[classes == "labelled"])
charls_data_converted <- charls_data_converted %>%
  mutate_if(names(.) %in% c("sex_charls","hukou", "rural"), as.factor)
set.seed(1005)
mice_mod <- mice(charls_data_converted, method = "cart", m =1, maxit = 5)
imputed_data <- complete(mice_mod)
common_cols <- intersect(names(charls_data), names(imputed_data))
charls_data[common_cols] <- imputed_data[common_cols]



#percentile rank of education
## percentile ranking of respondents' education level
charls_data <- charls_data %>%
  mutate(percentile_rank_edu = rank(Education,ties.method = c("max"))/length(Education))

## percentile ranking of respondent' parent education level
charls_data <- charls_data %>%
  mutate(percentile_rank_pedu =  rank(highest_education_parent,ties.method = c("max"))/length(highest_education_parent))

#demographic summary table
## table1
#charls_data$H_highest_education_parent <- as.factor(charls_data$H_highest_education_parent)
charls_data$Harmonized_education <- as.factor(charls_data$Harmonized_education)
charls_data$H_highest_education_parent <- as.factor(charls_data$H_highest_education_parent)
charls_data$sex_charls <- as.factor(charls_data$sex_charls)
charls_data$rural <- as.factor(charls_data$rural)
base_charls <- CreateTableOne(vars = c("fgcp_charls","fexf_charls","flang_charls","forient_charls","fmem_charls", "age_charls", "sex_charls", "Harmonized_education","H_highest_education_parent","rural","percentile_rank_pedu","percentile_rank_edu", "hukou"),
                            data = charls_data, 
                            includeNA = FALSE)
print(base_charls, showAllLevels = TRUE) 
base_charls_summary <- print(base_charls, showAllLevels = TRUE, catDigits = 3, printToggle = FALSE)
base_charls_df <- as.data.frame(base_charls_summary)
base_charls_df <- cbind(Variable = rownames(base_charls_df), base_charls_df)

## table3
quantiles_r <- quantile(charls_data$percentile_rank_edu, probs = c(0.25, 0.75))
quantiles_p <- quantile(charls_data$percentile_rank_pedu, probs = c(0.25, 0.75))

charls_data$education_quan <- cut(charls_data$percentile_rank_edu,
                                breaks = c(-Inf, quantiles_r, Inf),
                                labels = c(1, 2, 3),
                                right = TRUE)
charls_data$education_p_quan <- cut(charls_data$percentile_rank_pedu,
                                  breaks = c(-Inf, quantiles_p, Inf),
                                  labels = c(1, 2, 3),
                                  right = TRUE)
table(charls_data$education_quan)
table(charls_data$education_p_quan)

df_schooling <- subset(charls_data, education_quan == 1, select = Education)
range(df_schooling$Education)
df_schooling <- subset(charls_data, education_quan == 2, select = Education)
range(df_schooling$Education)
df_schooling <- subset(charls_data, education_quan == 3, select = Education)
range(df_schooling$Education)

df_schooling <- subset(charls_data, education_p_quan == 1, select = highest_education_parent)
range(df_schooling$highest_education_parent)
df_schooling <- subset(charls_data, education_p_quan == 2, select = highest_education_parent)
range(df_schooling$highest_education_parent)
df_schooling <- subset(charls_data, education_p_quan == 3, select = highest_education_parent)
range(df_schooling$highest_education_parent)

base_charls <- CreateTableOne(vars = c("education_quan"),
                            data = charls_data, 
                            includeNA = FALSE)
print(base_charls, showAllLevels = TRUE) 

base_charls <- CreateTableOne(vars = c("education_p_quan"),
                            data = charls_data, 
                            includeNA = FALSE)
print(base_charls, showAllLevels = TRUE) 

write.csv(charls_data,"D:\\R project\\Cognition\\charls\\charls_imputed.csv")

#charls_data <- charls_data[complete.cases( charls_data[c("fgcp_charls","fexf_charls","flang_charls","forient_charls","fmem_charls")]), ]
# n = 5053

#chi-sq by gender
## pr
contingency_table <- table(charls_data$sex_charls, charls_data$education_quan)
chi_square_test <- chisq.test(contingency_table)
print(chi_square_test)

## H
contingency_table <- table(charls_data$sex_charls, charls_data$Harmonized_education)
chi_square_test <- chisq.test(contingency_table)
print(chi_square_test)

freq_table <- table(charls_data$education_quan,charls_data$sex_charls)
print(freq_table)
prop.table(freq_table, margin = 2)

freq_table <- table(charls_data$Harmonized_education,charls_data$sex_charls)
print(freq_table)
prop.table(freq_table, margin = 2)


