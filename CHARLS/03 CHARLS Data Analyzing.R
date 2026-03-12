library(dplyr)
library(haven)
library(tableone)
library(mice)
library(ggplot2)
library(emmeans)
library(MCM)
library(reshape2)
library(gnm)

setwd("D:\\R project\\Cognition\\charls")
#----Load the datasets----
data <- read.csv("D:\\R project\\Cognition\\charls\\charls_imputed.csv")
data <- data %>%
  select(id_charls, age_charls, sex_charls, hukou, rural, Harmonized_education, 
         H_highest_education_parent, education_quan, education_p_quan, 
         fgcp_charls, fexf_charls, flang_charls, forient_charls, fmem_charls)
data$urbanicity_charls <- data$rural
#----RDM----
##----H_edu----
# Mobility in any direction
data$H_mobile_charls <- ifelse(data$Harmonized_education != data$H_highest_education_parent, 1, 0)
attr(data$H_mobile_charls, "label") <- "Mobility in any direction"
data$H_mobile_charls <- factor(data$H_mobile_charls, levels = c(0, 1), labels = c("Non-mobile", "Mobile"))

# Number of mobility steps
data$H_mobility_steps_charls <- data$Harmonized_education - data$H_highest_education_parent
attr(data$H_mobility_steps_charls, "label") <- "Inter-generational extent of mobility"

# Upwards mobility
data$H_upwards_charls <- ifelse(data$H_mobility_steps_charls %in% 1:2, 1, 0)
attr(data$H_upwards_charls, "label") <- "Upwardly mobile"

# Downwards mobility
data$H_downwards_charls <- ifelse(data$H_mobility_steps_charls %in% -2:-1, 1, 0)
attr(data$H_downwards_charls, "label") <- "Downwardly mobile"

# Number of steps upwardly mobile
data$H_oneup_charls <- ifelse((data$H_highest_education_parent - data$Harmonized_education) == 1, 1, 0)
data$H_twoup_charls <- ifelse((data$H_highest_education_parent - data$Harmonized_education) == 2, 1, 0)

# Number of steps downwardly mobile
data$H_onedown_charls <- ifelse((data$H_highest_education_parent - data$Harmonized_education) == -1, 1, 0)
data$H_twodown_charls <- ifelse((data$H_highest_education_parent - data$Harmonized_education) == -2, 1, 0)

# Mobility status
data$H_mobility_status_charls <- NA
data$H_mobility_status_charls[data$H_highest_education_parent == 1 & data$Harmonized_education == 1] <- 1
data$H_mobility_status_charls[data$H_highest_education_parent == 2 & data$Harmonized_education == 2] <- 2
data$H_mobility_status_charls[data$H_highest_education_parent == 3 & data$Harmonized_education == 3] <- 3
data$H_mobility_status_charls[data$H_mobility_steps_charls %in% -2:-1] <- 0
data$H_mobility_status_charls[data$H_mobility_steps_charls %in% 1:2] <- 4
attr(data$H_mobility_status_charls, "label") <- "Mobility status"
data$H_mobility_status_charls <- factor(data$H_mobility_status_charls,
                                      levels = c(0, 1, 2, 4, 3),
                                      labels = c("Downwardly mobile", "Stable low","Stable mid","Upwardly mobile","Stable high" ))
data$H_mobility_status_charls <- relevel(data$H_mobility_status_charls, ref = "Stable low")
table(data$H_mobility_status_charls)

# Mobility stability
data$H_mobility_stability_charls <- data$H_mobility_status_charls
data$H_mobility_stability_charls <- as.character(data$H_mobility_stability_charls)
data$H_mobility_stability_charls <- factor(data$H_mobility_stability_charls,
                                         levels = c("Downwardly mobile", "Stable low", "Stable mid", "Stable high", "Upwardly mobile"),labels = c("0", "1", "1", "1", "2"))
attr(data$H_mobility_stability_charls, "label") <- "3-level mobility classification"
data$H_mobility_stability_charls <- factor(data$H_mobility_stability_charls,
                                         levels = c(0, 1, 2),
                                         labels = c("Downwardly mobile", "Non-mobile", "Upwardly mobile"))
table(data$H_mobility_stability_charls)

#preparing data
data <- data  
data$edu_charls <- factor(data$Harmonized_education)
data$pedu_charls <- factor(data$H_highest_education_parent)
data$sex_charls <- factor(data$sex_charls)



##----percentile_edu----
# Mobility in any direction
data$mobile_charls <- ifelse(data$education_quan != data$education_p_quan, 1, 0)
attr(data$mobile_charls, "label") <- "Mobility in any direction"
data$mobile_charls <- factor(data$mobile_charls, levels = c(0, 1), labels = c("Non-mobile", "Mobile"))

# Number of mobility steps
data$mobility_steps_charls <- data$education_quan - data$education_p_quan
attr(data$mobility_steps_charls, "label") <- "Inter-generational extent of mobility"

# Upwards mobility
data$upwards_charls <- ifelse(data$mobility_steps_charls %in% 1:2, 1, 0)
attr(data$upwards_charls, "label") <- "Upwardly mobile"

# Downwards mobility
data$downwards_charls <- ifelse(data$mobility_steps_charls %in% -2:-1, 1, 0)
attr(data$downwards_charls, "label") <- "Downwardly mobile"

# Number of steps upwardly mobile
data$oneup_charls <- ifelse((data$education_p_quan - data$education_quan) == 1, 1, 0)
data$twoup_charls <- ifelse((data$education_p_quan - data$education_quan) == 2, 1, 0)

# Number of steps downwardly mobile
data$onedown_charls <- ifelse((data$education_p_quan - data$education_quan) == -1, 1, 0)
data$twodown_charls <- ifelse((data$education_p_quan - data$education_quan) == -2, 1, 0)

# Mobility status
data$mobility_status_charls <- NA
data$mobility_status_charls[data$education_p_quan == 1 & data$education_quan == 1] <- 1
data$mobility_status_charls[data$education_p_quan == 2 & data$education_quan == 2] <- 2
data$mobility_status_charls[data$education_p_quan == 3 & data$education_quan == 3] <- 3
data$mobility_status_charls[data$mobility_steps_charls %in% -2:-1] <- 0
data$mobility_status_charls[data$mobility_steps_charls %in% 1:2] <- 4
attr(data$mobility_status_charls, "label") <- "Mobility status"
data$mobility_status_charls <- factor(data$mobility_status_charls,
                                    levels = c(0, 1, 2, 4, 3),
                                    labels = c("Downwardly mobile", "Stable low","Stable mid",
                                               "Upwardly mobile","Stable high"))
data$mobility_status_charls <- relevel(data$mobility_status_charls, ref = "Stable low")
table(data$mobility_status_charls)

# Mobility stability
data$mobility_stability_charls <- data$mobility_status_charls
data$mobility_stability_charls <- as.character(data$mobility_stability_charls)
data$mobility_stability_charls <- factor(data$mobility_stability_charls,
                                       levels = c("Downwardly mobile", "Stable low", "Stable mid", "Stable high", "Upwardly mobile"),labels = c("0", "1", "1", "1", "2"))
attr(data$mobility_stability_charls, "label") <- "3-level mobility classification"
data$mobility_stability_charls <- factor(data$mobility_stability_charls,
                                       levels = c(0, 1, 2),
                                       labels = c("Downwardly mobile", "Non-mobile", "Upwardly mobile"))
table(data$mobility_stability_charls)

#preparing data
data <- data  
data$edu_charls <- factor(data$education_quan)
data$pedu_charls <- factor(data$education_p_quan)
data$sex <- factor(data$sex)



#extract data for stata drm
write.csv(data,"D:\\R project\\Cognition\\charls\\stata_hcap_charls.csv")


####add interaction####
df_charls <- read.csv("D:\\R project\\Cognition\\hcap_stata_data\\stata_hcap_charls.csv")
model1_fgcp <- lm(fgcp_charls ~ H_mobility_status_charls * sex_charls + age_charls  + race, data = df_charls)
summary(model1_fgcp)

coef_table <- summary(model1_fgcp)$coefficients
conf_int <- confint(model1_fgcp)

model1_fexf <- lm(fexf_charls ~ H_mobility_status_charls * sex_charls + age_charls  + race, data = df_charls)
model1_flang <- lm(flang_charls ~ H_mobility_status_charls * sex_charls + age_charls  + race, data = df_charls)
model1_forient <- lm(forient_charls ~ H_mobility_status_charls * sex_charls + age_charls  + race, data = df_charls)
model1_fmem <- lm(fmem_charls ~ H_mobility_status_charls * sex_charls + age_charls  + race, data = df_charls)
tab_model(model1_fgcp, model1_fexf, model1_flang, model1_forient, model1_fmem, show.se = T, show.ci = F)

#percentile rank
df_charls <- read.csv("D:\\R project\\Cognition\\hcap_stata_data\\stata_hcap_charls.csv")
model1_fgcp <- lm(fgcp_charls ~ mobility_status_charls * sex_charls + age_charls  + race, data = df_charls)
summary(model1_fgcp)

coef_table <- summary(model1_fgcp)$coefficients
conf_int <- confint(model1_fgcp)

model1_fexf <- lm(fexf_charls ~ mobility_status_charls * sex_charls + age_charls  + race, data = df_charls)
model1_flang <- lm(flang_charls ~ mobility_status_charls * sex_charls + age_charls  + race, data = df_charls)
model1_forient <- lm(forient_charls ~ mobility_status_charls * sex_charls + age_charls  + race, data = df_charls)
model1_fmem <- lm(fmem_charls ~ mobility_status_charls * sex_charls + age_charls  + race, data = df_charls)
tab_model(model1_fgcp, model1_fexf, model1_flang, model1_forient, model1_fmem, show.se = T, show.ci = F)


#----sensitivity analysis----

##1----complete cases----

###----drop all incomplete missing value in education----

##2----outcome in different domain----
