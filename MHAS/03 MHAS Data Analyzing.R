library(dplyr)
library(haven)
library(tableone)
library(mice)
library(ggplot2)
library(emmeans)
library(MCM)
library(reshape2)
library(gnm)

setwd("D:\\R project\\Cognition\\mhas")
#----Load the datasets----
data <- read.csv("D:\\R project\\Cognition\\mhas\\mhas_imputed.csv")
data <- data %>%
  select(unhhidnp_mhas, age_mhas, sex_mhas, urbanicity_mhas, Harmonized_education, 
         H_highest_education_parent, education_quan, education_p_quan, 
         fgcp_mhas, fexf_mhas, flang_mhas, forient_mhas, fmem_mhas)

#----RDM----
##----H_edu----
# Mobility in any direction
data$H_mobile_mhas <- ifelse(data$Harmonized_education != data$H_highest_education_parent, 1, 0)
attr(data$H_mobile_mhas, "label") <- "Mobility in any direction"
data$H_mobile_mhas <- factor(data$H_mobile_mhas, levels = c(0, 1), labels = c("Non-mobile", "Mobile"))

# Number of mobility steps
data$H_mobility_steps_mhas <- data$Harmonized_education - data$H_highest_education_parent
attr(data$H_mobility_steps_mhas, "label") <- "Inter-generational extent of mobility"

# Upwards mobility
data$H_upwards_mhas <- ifelse(data$H_mobility_steps_mhas %in% 1:2, 1, 0)
attr(data$H_upwards_mhas, "label") <- "Upwardly mobile"

# Downwards mobility
data$H_downwards_mhas <- ifelse(data$H_mobility_steps_mhas %in% -2:-1, 1, 0)
attr(data$H_downwards_mhas, "label") <- "Downwardly mobile"

# Number of steps upwardly mobile
data$H_oneup_mhas <- ifelse((data$H_highest_education_parent - data$Harmonized_education) == 1, 1, 0)
data$H_twoup_mhas <- ifelse((data$H_highest_education_parent - data$Harmonized_education) == 2, 1, 0)

# Number of steps downwardly mobile
data$H_onedown_mhas <- ifelse((data$H_highest_education_parent - data$Harmonized_education) == -1, 1, 0)
data$H_twodown_mhas <- ifelse((data$H_highest_education_parent - data$Harmonized_education) == -2, 1, 0)

# Mobility status
data$H_mobility_status_mhas <- NA
data$H_mobility_status_mhas[data$H_highest_education_parent == 1 & data$Harmonized_education == 1] <- 1
data$H_mobility_status_mhas[data$H_highest_education_parent == 2 & data$Harmonized_education == 2] <- 2
data$H_mobility_status_mhas[data$H_highest_education_parent == 3 & data$Harmonized_education == 3] <- 3
data$H_mobility_status_mhas[data$H_mobility_steps_mhas %in% -2:-1] <- 0
data$H_mobility_status_mhas[data$H_mobility_steps_mhas %in% 1:2] <- 4
attr(data$H_mobility_status_mhas, "label") <- "Mobility status"
data$H_mobility_status_mhas <- factor(data$H_mobility_status_mhas,
                                    levels = c(0, 1, 2, 4, 3),
                                    labels = c("Downwardly mobile", "Stable low","Stable mid","Upwardly mobile","Stable high" ))
data$H_mobility_status_mhas <- relevel(data$H_mobility_status_mhas, ref = "Stable low")
table(data$H_mobility_status_mhas)

# Mobility stability
data$H_mobility_stability_mhas <- data$H_mobility_status_mhas
data$H_mobility_stability_mhas <- as.character(data$H_mobility_stability_mhas)
data$H_mobility_stability_mhas <- factor(data$H_mobility_stability_mhas,
                                       levels = c("Downwardly mobile", "Stable low", "Stable mid", "Stable high", "Upwardly mobile"),labels = c("0", "1", "1", "1", "2"))
attr(data$H_mobility_stability_mhas, "label") <- "3-level mobility classification"
data$H_mobility_stability_mhas <- factor(data$H_mobility_stability_mhas,
                                       levels = c(0, 1, 2),
                                       labels = c("Downwardly mobile", "Non-mobile", "Upwardly mobile"))
table(data$H_mobility_stability_mhas)

#preparing data
data <- data  
data$edu_mhas <- factor(data$Harmonized_education)
data$pedu_mhas <- factor(data$H_highest_education_parent)
data$sex_mhas <- factor(data$sex_mhas)
data$urbanicity_mhas <- ifelse(data$urbanicity_mhas == 1, "rural", "urban")
data$urbanicity_mhas <- factor(data$urbanicity_mhas, levels = c("urban","rural"))


#extract data for stata drm
#write.csv(data,"D:\\R project\\Cognition\\MHAS\\stata_hcap_mhas.csv")

##----PR_edu----
# Mobility in any direction
data$mobile_mhas <- ifelse(data$education_quan != data$education_p_quan, 1, 0)
attr(data$mobile_mhas, "label") <- "Mobility in any direction"
data$mobile_mhas <- factor(data$mobile_mhas, levels = c(0, 1), labels = c("Non-mobile", "Mobile"))

# Number of mobility steps
data$mobility_steps_mhas <- data$education_quan - data$education_p_quan
attr(data$mobility_steps_mhas, "label") <- "Inter-generational extent of mobility"

# Upwards mobility
data$upwards_mhas <- ifelse(data$mobility_steps_mhas %in% 1:2, 1, 0)
attr(data$upwards_mhas, "label") <- "Upwardly mobile"

# Downwards mobility
data$downwards_mhas <- ifelse(data$mobility_steps_mhas %in% -2:-1, 1, 0)
attr(data$downwards_mhas, "label") <- "Downwardly mobile"

# Number of steps upwardly mobile
data$oneup_mhas <- ifelse((data$education_p_quan - data$education_quan) == 1, 1, 0)
data$twoup_mhas <- ifelse((data$education_p_quan - data$education_quan) == 2, 1, 0)

# Number of steps downwardly mobile
data$onedown_mhas <- ifelse((data$education_p_quan - data$education_quan) == -1, 1, 0)
data$twodown_mhas <- ifelse((data$education_p_quan - data$education_quan) == -2, 1, 0)

# Mobility status
data$mobility_status_mhas <- NA
data$mobility_status_mhas[data$education_p_quan == 1 & data$education_quan == 1] <- 1
data$mobility_status_mhas[data$education_p_quan == 2 & data$education_quan == 2] <- 2
data$mobility_status_mhas[data$education_p_quan == 3 & data$education_quan == 3] <- 3
data$mobility_status_mhas[data$mobility_steps_mhas %in% -2:-1] <- 0
data$mobility_status_mhas[data$mobility_steps_mhas %in% 1:2] <- 4
attr(data$mobility_status_mhas, "label") <- "Mobility status"
data$mobility_status_mhas <- factor(data$mobility_status_mhas,
                                  levels = c(0, 1, 2, 4, 3),
                                  labels = c("Downwardly mobile", "Stable low","Stable mid",
                                             "Upwardly mobile","Stable high"))
data$mobility_status_mhas <- factor(data$mobility_status_mhas,levels = c("Stable low","Downwardly mobile","Stable mid","Upwardly mobile","Stable high"))
data$mobility_status_mhas <- relevel(data$mobility_status_mhas, ref = "Stable low")
table(data$mobility_status_mhas)

# Mobility stability
data$mobility_stability_mhas <- data$mobility_status_mhas
data$mobility_stability_mhas <- as.character(data$mobility_stability_mhas)
data$mobility_stability_mhas <- factor(data$mobility_stability_mhas,
                                     levels = c("Downwardly mobile", "Stable low", "Stable mid", "Stable high", "Upwardly mobile"),labels = c("0", "1", "1", "1", "2"))
attr(data$mobility_stability_mhas, "label") <- "3-level mobility classification"
data$mobility_stability_mhas <- factor(data$mobility_stability_mhas,
                                     levels = c(0, 1, 2),
                                     labels = c("Downwardly mobile", "Non-mobile", "Upwardly mobile"))
table(data$mobility_stability_mhas)

#preparing data
data <- data  
data$edu_mhas <- factor(data$education_quan)
data$pedu_mhas <- factor(data$education_p_quan)
data$sex_mhas <- factor(data$sex_mhas)

#extract data for stata drm
write.csv(data,"D:\\R project\\Cognition\\MHAS\\stata_hcap_mhas.csv")

####add interaction####
df_mhas <- read.csv("D:\\R project\\Cognition\\hcap_stata_data\\stata_hcap_mhas.csv")
model1_fgcp <- lm(fgcp_mhas ~ H_mobility_status_mhas * sex_mhas + age_mhas  + urbanicity_mhas, data = df_mhas)
summary(model1_fgcp)

coef_table <- summary(model1_fgcp)$coefficients
conf_int <- confint(model1_fgcp)

model1_fexf <- lm(fexf_mhas ~ H_mobility_status_mhas * sex_mhas + age_mhas  + urbanicity_mhas, data = df_mhas)
model1_flang <- lm(flang_mhas ~ H_mobility_status_mhas * sex_mhas + age_mhas  + urbanicity_mhas, data = df_mhas)
model1_forient <- lm(forient_mhas ~ H_mobility_status_mhas * sex_mhas + age_mhas  + urbanicity_mhas, data = df_mhas)
model1_fmem <- lm(fmem_mhas ~ H_mobility_status_mhas * sex_mhas + age_mhas  + urbanicity_mhas, data = df_mhas)
tab_model(model1_fgcp, model1_fexf, model1_flang, model1_forient, model1_fmem, show.se = T, show.ci = F)

#percentile rank
df_mhas <- read.csv("D:\\R project\\Cognition\\hcap_stata_data\\stata_hcap_mhas.csv")
model1_fgcp <- lm(fgcp_mhas ~ mobility_status_mhas * sex_mhas + age_mhas  + urbanicity_mhas, data = df_mhas)
summary(model1_fgcp)

coef_table <- summary(model1_fgcp)$coefficients
conf_int <- confint(model1_fgcp)

model1_fexf <- lm(fexf_mhas ~ mobility_status_mhas * sex_mhas + age_mhas  + urbanicity_mhas, data = df_mhas)
model1_flang <- lm(flang_mhas ~ mobility_status_mhas * sex_mhas + age_mhas  + urbanicity_mhas, data = df_mhas)
model1_forient <- lm(forient_mhas ~ mobility_status_mhas * sex_mhas + age_mhas  + urbanicity_mhas, data = df_mhas)
model1_fmem <- lm(fmem_mhas ~ mobility_status_mhas * sex_mhas + age_mhas  + urbanicity_mhas, data = df_mhas)
tab_model(model1_fgcp, model1_fexf, model1_flang, model1_forient, model1_fmem, show.se = T, show.ci = F)


#----sensitivity analysis----

##1----complete cases----

###----drop all incomplete missing value in education----

##2----outcome in different domain----
