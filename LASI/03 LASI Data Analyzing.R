library(dplyr)
library(haven)
library(tableone)
library(mice)
library(ggplot2)
library(emmeans)
library(MCM)
library(reshape2)

setwd("D:\\R project\\Cognition\\LASI")
#----Load the datasets----
data <- read.csv("D:\\R project\\Cognition\\LASI\\lasi_imputed.csv")
data <- data %>%
  select(id_lasidad, age_lasi, sex_lasi, urbanicity_lasi,caste, Harmonized_education, 
         H_highest_education_parent, education_quan, education_p_quan, 
         fgcp_lasi, fexf_lasi, flang_lasi, forient_lasi, fmem_lasi)

#----RDM----
##----H_edu----
# Mobility in any direction
# Mobility in any direction
data$H_mobile_lasi <- ifelse(data$Harmonized_education != data$H_highest_education_parent, 1, 0)
attr(data$H_mobile_lasi, "label") <- "Mobility in any direction"
data$H_mobile_lasi <- factor(data$H_mobile_lasi, levels = c(0, 1), labels = c("Non-mobile", "Mobile"))

# Number of mobility steps
data$H_mobility_steps_lasi <- data$Harmonized_education - data$H_highest_education_parent
attr(data$H_mobility_steps_lasi, "label") <- "Inter-generational extent of mobility"

# Upwards mobility
data$H_upwards_lasi <- ifelse(data$H_mobility_steps_lasi %in% 1:2, 1, 0)
attr(data$H_upwards_lasi, "label") <- "Upwardly mobile"

# Downwards mobility
data$H_downwards_lasi <- ifelse(data$H_mobility_steps_lasi %in% -2:-1, 1, 0)
attr(data$H_downwards_lasi, "label") <- "Downwardly mobile"

# Number of steps upwardly mobile
data$H_oneup_lasi <- ifelse((data$H_highest_education_parent - data$Harmonized_education) == 1, 1, 0)
data$H_twoup_lasi <- ifelse((data$H_highest_education_parent - data$Harmonized_education) == 2, 1, 0)

# Number of steps downwardly mobile
data$H_onedown_lasi <- ifelse((data$H_highest_education_parent - data$Harmonized_education) == -1, 1, 0)
data$H_twodown_lasi <- ifelse((data$H_highest_education_parent - data$Harmonized_education) == -2, 1, 0)



# Mobility status
data$H_mobility_status_lasi <- NA
data$H_mobility_status_lasi[data$H_highest_education_parent == 1 & data$Harmonized_education == 1] <- 1
data$H_mobility_status_lasi[data$H_highest_education_parent == 2 & data$Harmonized_education == 2] <- 2
data$H_mobility_status_lasi[data$H_highest_education_parent == 3 & data$Harmonized_education == 3] <- 3
data$H_mobility_status_lasi[data$H_mobility_steps_lasi %in% -2:-1] <- 0
data$H_mobility_status_lasi[data$H_mobility_steps_lasi %in% 1:2] <- 4
attr(data$H_mobility_status_lasi, "label") <- "Mobility status"
data$H_mobility_status_lasi <- factor(data$H_mobility_status_lasi,
                                     levels = c(0, 1, 2, 4, 3),
                                     labels = c("Downwardly mobile", "Stable low","Stable mid","Upwardly mobile","Stable high" ))
data$H_mobility_status_lasi <- relevel(data$H_mobility_status_lasi, ref = "Stable low")
table(data$H_mobility_status_lasi)

# Mobility stability
data$H_mobility_stability_lasi <- data$H_mobility_status_lasi
data$H_mobility_stability_lasi <- as.character(data$H_mobility_stability_lasi)
data$H_mobility_stability_lasi <- factor(data$H_mobility_stability_lasi,
                                        levels = c("Downwardly mobile", "Stable low", "Stable mid", "Stable high", "Upwardly mobile"),labels = c("0", "1", "1", "1", "2"))
attr(data$H_mobility_stability_lasi, "label") <- "3-level mobility classification"
data$H_mobility_stability_lasi <- factor(data$H_mobility_stability_lasi,
                                        levels = c(0, 1, 2),
                                        labels = c("Downwardly mobile", "Non-mobile", "Upwardly mobile"))
table(data$H_mobility_stability_lasi)

#preparing data
data <- data  
data$edu_lasi <- factor(data$Harmonized_education)
data$pedu_lasi <- factor(data$H_highest_education_parent)
data$sex_lasi <- factor(data$sex_lasi)
data$caste <- factor(data$caste, levels = c("Scheduled caste or scheduled tribe", "Other"))
data$caste <- relevel(factor(data$caste), ref = "Other")
data$urbanicity_lasi <- relevel(factor(data$urbanicity_lasi), ref = "Urban")


##----percentile_edu----
# Mobility in any direction
data$mobile_lasi <- ifelse(data$education_quan != data$education_p_quan, 1, 0)
attr(data$mobile_lasi, "label") <- "Mobility in any direction"
data$mobile_lasi <- factor(data$mobile_lasi, levels = c(0, 1), labels = c("Non-mobile", "Mobile"))

# Number of mobility steps
data$mobility_steps_lasi <- data$education_quan - data$education_p_quan
attr(data$mobility_steps_lasi, "label") <- "Inter-generational extent of mobility"

# Upwards mobility
data$upwards_lasi <- ifelse(data$mobility_steps_lasi %in% 1:2, 1, 0)
attr(data$upwards_lasi, "label") <- "Upwardly mobile"

# Downwards mobility
data$downwards_lasi <- ifelse(data$mobility_steps_lasi %in% -2:-1, 1, 0)
attr(data$downwards_lasi, "label") <- "Downwardly mobile"

# Number of steps upwardly mobile
data$oneup_lasi <- ifelse((data$education_p_quan - data$education_quan) == 1, 1, 0)
data$twoup_lasi <- ifelse((data$education_p_quan - data$education_quan) == 2, 1, 0)

# Number of steps downwardly mobile
data$onedown_lasi <- ifelse((data$education_p_quan - data$education_quan) == -1, 1, 0)
data$twodown_lasi <- ifelse((data$education_p_quan - data$education_quan) == -2, 1, 0)

# Mobility status
data$mobility_status_lasi <- NA
data$mobility_status_lasi[data$education_p_quan == 1 & data$education_quan == 1] <- 1
data$mobility_status_lasi[data$education_p_quan == 2 & data$education_quan == 2] <- 2
data$mobility_status_lasi[data$education_p_quan == 3 & data$education_quan == 3] <- 3
data$mobility_status_lasi[data$mobility_steps_lasi %in% -2:-1] <- 0
data$mobility_status_lasi[data$mobility_steps_lasi %in% 1:2] <- 4
attr(data$mobility_status_lasi, "label") <- "Mobility status"
data$mobility_status_lasi <- factor(data$mobility_status_lasi,
                                 levels = c(0, 1, 2, 4, 3),
                                 labels = c("Downwardly mobile", "Stable low","Stable mid",
                                            "Upwardly mobile","Stable high"))
data$mobility_status_lasi <- relevel(data$mobility_status_lasi, ref = "Stable low")
table(data$mobility_status_lasi)

# Mobility stability
data$mobility_stability_lasi <- data$mobility_status_lasi
data$mobility_stability_lasi <- as.character(data$mobility_stability_lasi)
data$mobility_stability_lasi <- factor(data$mobility_stability_lasi,
                                    levels = c("Downwardly mobile", "Stable low", "Stable mid", "Stable high", "Upwardly mobile"),labels = c("0", "1", "1", "1", "2"))
attr(data$mobility_stability_lasi, "label") <- "3-level mobility classification"
data$mobility_stability_lasi <- factor(data$mobility_stability_lasi,
                                    levels = c(0, 1, 2),
                                    labels = c("Downwardly mobile", "Non-mobile", "Upwardly mobile"))
table(data$mobility_stability_lasi)

#preparing data
data <- data  
data$edu_lasi <- factor(data$education_quan)
data$pedu_lasi <- factor(data$education_p_quan)
data$sex_lasi <- factor(data$sex_lasi)
data$caste <- factor(data$caste, levels = c("Scheduled caste or scheduled tribe", "Other"))
data$urbanicity_lasi <- relevel(factor(data$urbanicity_lasi), ref = "Rural")

#extract data for stata drm
write.csv(data,"D:\\R project\\Cognition\\LASI\\stata_hcap_lasi.csv")

####add interaction####
df_lasi <- read.csv("D:\\R project\\Cognition\\hcap_stata_data\\stata_hcap_lasi.csv")
model1_fgcp <- lm(fgcp_lasi ~ H_mobility_status_lasi * sex_lasi + age_lasi  + urbanicity_lasi  + caste, data = df_lasi)
summary(model1_fgcp)

coef_table <- summary(model1_fgcp)$coefficients
conf_int <- confint(model1_fgcp)

model1_fexf <- lm(fexf_lasi ~ H_mobility_status_lasi * sex_lasi + age_lasi  + urbanicity_lasi  + caste, data = df_lasi)
model1_flang <- lm(flang_lasi ~ H_mobility_status_lasi * sex_lasi + age_lasi  + urbanicity_lasi  + caste, data = df_lasi)
model1_forient <- lm(forient_lasi ~ H_mobility_status_lasi * sex_lasi + age_lasi  + urbanicity_lasi  + caste, data = df_lasi)
model1_fmem <- lm(fmem_lasi ~ H_mobility_status_lasi * sex_lasi + age_lasi  + urbanicity_lasi  + caste, data = df_lasi)
tab_model(model1_fgcp, model1_fexf, model1_flang, model1_forient, model1_fmem, show.se = T, show.ci = F)

#percentile rank
df_lasi <- read.csv("D:\\R project\\Cognition\\hcap_stata_data\\stata_hcap_lasi.csv")
model1_fgcp <- lm(fgcp_lasi ~ mobility_status_lasi * sex_lasi + age_lasi  + urbanicity_lasi  + caste, data = df_lasi)
summary(model1_fgcp)

coef_table <- summary(model1_fgcp)$coefficients
conf_int <- confint(model1_fgcp)

model1_fexf <- lm(fexf_lasi ~ mobility_status_lasi * sex_lasi + age_lasi  + urbanicity_lasi  + caste, data = df_lasi)
model1_flang <- lm(flang_lasi ~ mobility_status_lasi * sex_lasi + age_lasi  + urbanicity_lasi  + caste, data = df_lasi)
model1_forient <- lm(forient_lasi ~ mobility_status_lasi * sex_lasi + age_lasi  + urbanicity_lasi  + caste, data = df_lasi)
model1_fmem <- lm(fmem_lasi ~ mobility_status_lasi * sex_lasi + age_lasi  + urbanicity_lasi  + caste, data = df_lasi)
tab_model(model1_fgcp, model1_fexf, model1_flang, model1_forient, model1_fmem, show.se = T, show.ci = F)
#----sensitivity analysis----

##1----complete cases----

###----drop all incomplete missing value in education----

##2----outcome in different domain----
