library(dplyr)
library(haven)
library(tableone)
library(mice)
library(ggplot2)
library(emmeans)
library(MCM)
library(reshape2)
library(gnm)

setwd("D:\\R project\\Cognition\\elsa")
#----Load the datasets----
data <- read.csv("D:\\R project\\Cognition\\elsa\\elsa_imputed.csv")
data <- data %>%
  select(id_elsa, age_elsa, sex_elsa, race, Harmonized_education, 
         H_highest_education_parent, education_quan, education_p_quan, 
         fgcp_elsa, fexf_elsa, flang_elsa, forient_elsa, fmem_elsa)

#----RDM----
##----H_edu----
# Mobility in any direction
data$H_mobile_elsa <- ifelse(data$Harmonized_education != data$H_highest_education_parent, 1, 0)
attr(data$H_mobile_elsa, "label") <- "Mobility in any direction"
data$H_mobile_elsa <- factor(data$H_mobile_elsa, levels = c(0, 1), labels = c("Non-mobile", "Mobile"))

# Number of mobility steps
data$H_mobility_steps_elsa <- data$Harmonized_education - data$H_highest_education_parent
attr(data$H_mobility_steps_elsa, "label") <- "Inter-generational extent of mobility"

# Upwards mobility
data$H_upwards_elsa <- ifelse(data$H_mobility_steps_elsa %in% 1:2, 1, 0)
attr(data$H_upwards_elsa, "label") <- "Upwardly mobile"

# Downwards mobility
data$H_downwards_elsa <- ifelse(data$H_mobility_steps_elsa %in% -2:-1, 1, 0)
attr(data$H_downwards_elsa, "label") <- "Downwardly mobile"

# Number of steps upwardly mobile
data$H_oneup_elsa <- ifelse((data$H_highest_education_parent - data$Harmonized_education) == 1, 1, 0)
data$H_twoup_elsa <- ifelse((data$H_highest_education_parent - data$Harmonized_education) == 2, 1, 0)

# Number of steps downwardly mobile
data$H_onedown_elsa <- ifelse((data$H_highest_education_parent - data$Harmonized_education) == -1, 1, 0)
data$H_twodown_elsa <- ifelse((data$H_highest_education_parent - data$Harmonized_education) == -2, 1, 0)

# Mobility status
data$H_mobility_status_elsa <- NA
data$H_mobility_status_elsa[data$H_highest_education_parent == 1 & data$Harmonized_education == 1] <- 1
data$H_mobility_status_elsa[data$H_highest_education_parent == 2 & data$Harmonized_education == 2] <- 2
data$H_mobility_status_elsa[data$H_highest_education_parent == 3 & data$Harmonized_education == 3] <- 3
data$H_mobility_status_elsa[data$H_mobility_steps_elsa %in% -2:-1] <- 0
data$H_mobility_status_elsa[data$H_mobility_steps_elsa %in% 1:2] <- 4
attr(data$H_mobility_status_elsa, "label") <- "Mobility status"
data$H_mobility_status_elsa <- factor(data$H_mobility_status_elsa,
                                   levels = c(0, 1, 2, 4, 3),
                                   labels = c("Downwardly mobile", "Stable low","Stable mid","Upwardly mobile","Stable high" ))
data$H_mobility_status_elsa <- relevel(data$H_mobility_status_elsa, ref = "Stable low")
table(data$H_mobility_status_elsa)

# Mobility stability
data$H_mobility_stability_elsa <- data$H_mobility_status_elsa
data$H_mobility_stability_elsa <- as.character(data$H_mobility_stability_elsa)
data$H_mobility_stability_elsa <- factor(data$H_mobility_stability_elsa,
                                      levels = c("Downwardly mobile", "Stable low", "Stable mid", "Stable high", "Upwardly mobile"),labels = c("0", "1", "1", "1", "2"))
attr(data$H_mobility_stability_elsa, "label") <- "3-level mobility classification"
data$H_mobility_stability_elsa <- factor(data$H_mobility_stability_elsa,
                                      levels = c(0, 1, 2),
                                      labels = c("Downwardly mobile", "Non-mobile", "Upwardly mobile"))
table(data$H_mobility_stability_elsa)

#preparing data
data <- data  
data$edu_elsa <- factor(data$Harmonized_education)
data$pedu_elsa <- factor(data$H_highest_education_parent)
data$sex_elsa <- factor(data$sex_elsa)
data$race <- factor(data$race, levels = c("White", "Non-White"))
data$race <- relevel(data$race, ref = "White")

##----percentile_edu----
# Mobility in any direction
data$mobile_elsa <- ifelse(data$education_quan != data$education_p_quan, 1, 0)
attr(data$mobile_elsa, "label") <- "Mobility in any direction"
data$mobile_elsa <- factor(data$mobile_elsa, levels = c(0, 1), labels = c("Non-mobile", "Mobile"))

# Number of mobility steps
data$mobility_steps_elsa <- data$education_quan - data$education_p_quan
attr(data$mobility_steps_elsa, "label") <- "Inter-generational extent of mobility"

# Upwards mobility
data$upwards_elsa <- ifelse(data$mobility_steps_elsa %in% 1:2, 1, 0)
attr(data$upwards_elsa, "label") <- "Upwardly mobile"

# Downwards mobility
data$downwards_elsa <- ifelse(data$mobility_steps_elsa %in% -2:-1, 1, 0)
attr(data$downwards_elsa, "label") <- "Downwardly mobile"

# Number of steps upwardly mobile
data$oneup_elsa <- ifelse((data$education_p_quan - data$education_quan) == 1, 1, 0)
data$twoup_elsa <- ifelse((data$education_p_quan - data$education_quan) == 2, 1, 0)

# Number of steps downwardly mobile
data$onedown_elsa <- ifelse((data$education_p_quan - data$education_quan) == -1, 1, 0)
data$twodown_elsa <- ifelse((data$education_p_quan - data$education_quan) == -2, 1, 0)

# Mobility status
data$mobility_status_elsa <- NA
data$mobility_status_elsa[data$education_p_quan == 1 & data$education_quan == 1] <- 1
data$mobility_status_elsa[data$education_p_quan == 2 & data$education_quan == 2] <- 2
data$mobility_status_elsa[data$education_p_quan == 3 & data$education_quan == 3] <- 3
data$mobility_status_elsa[data$mobility_steps_elsa %in% -2:-1] <- 0
data$mobility_status_elsa[data$mobility_steps_elsa %in% 1:2] <- 4
attr(data$mobility_status_elsa, "label") <- "Mobility status"
data$mobility_status_elsa <- factor(data$mobility_status_elsa,
                                  levels = c(0, 1, 2, 4, 3),
                                  labels = c("Downwardly mobile", "Stable low","Stable mid",
                                             "Upwardly mobile","Stable high"))
data$mobility_status_elsa <- relevel(data$mobility_status_elsa, ref = "Stable low")
table(data$mobility_status_elsa)

# Mobility stability
data$mobility_stability_elsa <- data$mobility_status_elsa
data$mobility_stability_elsa <- as.character(data$mobility_stability_elsa)
data$mobility_stability_elsa <- factor(data$mobility_stability_elsa,
                                     levels = c("Downwardly mobile", "Stable low", "Stable mid", "Stable high", "Upwardly mobile"),labels = c("0", "1", "1", "1", "2"))
attr(data$mobility_stability_elsa, "label") <- "3-level mobility classification"
data$mobility_stability_elsa <- factor(data$mobility_stability_elsa,
                                     levels = c(0, 1, 2),
                                     labels = c("Downwardly mobile", "Non-mobile", "Upwardly mobile"))
table(data$mobility_stability_elsa)

#preparing data
data <- data  
data$edu_elsa <- factor(data$education_quan)
data$pedu_elsa <- factor(data$education_p_quan)
data$sex <- factor(data$sex)
data$race <- factor(data$race, levels = c("White", "Non-White"))
data$race <- relevel(data$race, ref = "White")


#extract data for stata drm
write.csv(data,"D:\\R project\\Cognition\\ELSA\\stata_hcap_elsa.csv")


####add interaction####
df_elsa <- read.csv("D:\\R project\\Cognition\\hcap_stata_data\\stata_hcap_elsa.csv")
model1_fgcp <- lm(fgcp_elsa ~ H_mobility_status_elsa * sex_elsa + age_elsa  + race, data = df_elsa)
summary(model1_fgcp)

coef_table <- summary(model1_fgcp)$coefficients
conf_int <- confint(model1_fgcp)

model1_fexf <- lm(fexf_elsa ~ H_mobility_status_elsa * sex_elsa + age_elsa  + race, data = df_elsa)
model1_flang <- lm(flang_elsa ~ H_mobility_status_elsa * sex_elsa + age_elsa  + race, data = df_elsa)
model1_forient <- lm(forient_elsa ~ H_mobility_status_elsa * sex_elsa + age_elsa  + race, data = df_elsa)
model1_fmem <- lm(fmem_elsa ~ H_mobility_status_elsa * sex_elsa + age_elsa  + race, data = df_elsa)
tab_model(model1_fgcp, model1_fexf, model1_flang, model1_forient, model1_fmem, show.se = T, show.ci = F)

#percentile rank
df_elsa <- read.csv("D:\\R project\\Cognition\\hcap_stata_data\\stata_hcap_elsa.csv")
model1_fgcp <- lm(fgcp_elsa ~ mobility_status_elsa * sex_elsa + age_elsa  + race, data = df_elsa)
summary(model1_fgcp)

coef_table <- summary(model1_fgcp)$coefficients
conf_int <- confint(model1_fgcp)

model1_fexf <- lm(fexf_elsa ~ mobility_status_elsa * sex_elsa + age_elsa  + race, data = df_elsa)
model1_flang <- lm(flang_elsa ~ mobility_status_elsa * sex_elsa + age_elsa  + race, data = df_elsa)
model1_forient <- lm(forient_elsa ~ mobility_status_elsa * sex_elsa + age_elsa  + race, data = df_elsa)
model1_fmem <- lm(fmem_elsa ~ mobility_status_elsa * sex_elsa + age_elsa  + race, data = df_elsa)
tab_model(model1_fgcp, model1_fexf, model1_flang, model1_forient, model1_fmem, show.se = T, show.ci = F)


