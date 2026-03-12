library(dplyr)
library(haven)
library(tableone)
library(mice)
library(ggplot2)
library(emmeans)
library(MCM)
library(reshape2)
library(gnm)
library(sjPlot)

setwd("D:\\R project\\Cognition\\HRS")
#----Load the datasets----
data <- read.csv("D:\\R project\\Cognition\\HRS\\hrs_imputed.csv")
data$urbanicity_hrs <- ifelse(data$urbanicity_hrs == "Rual", "Rural", data$urbanicity_hrs)

data <- data %>%
  select(id_hrs, age_hrs, sex_hrs, Race,urbanicity_hrs, Harmonized_education, 
         H_highest_education_parent, education_quan, education_p_quan, 
         fgcp_hrs, fexf_hrs, flang_hrs, forient_hrs, fmem_hrs)

#conditional means
conditional_means <- data %>%
  group_by(Harmonized_education, H_highest_education_parent) %>%
  summarise(mean_fgcp_hrs = mean(fgcp_hrs, na.rm = TRUE))

#----RDM----
##----H_edu----
# Mobility in any direction
data$H_mobile_hrs <- ifelse(data$Harmonized_education != data$H_highest_education_parent, 1, 0)
attr(data$H_mobile_hrs, "label") <- "Mobility in any direction"
data$H_mobile_hrs <- factor(data$H_mobile_hrs, levels = c(0, 1), labels = c("Non-mobile", "Mobile"))

# Number of mobility steps
data$H_mobility_steps_hrs <- data$Harmonized_education - data$H_highest_education_parent
attr(data$H_mobility_steps_hrs, "label") <- "Inter-generational extent of mobility"

# Upwards mobility
data$H_upwards_hrs <- ifelse(data$H_mobility_steps_hrs %in% 1:2, 1, 0)
attr(data$H_upwards_hrs, "label") <- "Upwardly mobile"

# Downwards mobility
data$H_downwards_hrs <- ifelse(data$H_mobility_steps_hrs %in% -2:-1, 1, 0)
attr(data$H_downwards_hrs, "label") <- "Downwardly mobile"

# Number of steps upwardly mobile
data$H_oneup_hrs <- ifelse((data$H_highest_education_parent - data$Harmonized_education) == 1, 1, 0)
data$H_twoup_hrs <- ifelse((data$H_highest_education_parent - data$Harmonized_education) == 2, 1, 0)

# Number of steps downwardly mobile
data$H_onedown_hrs <- ifelse((data$H_highest_education_parent - data$Harmonized_education) == -1, 1, 0)
data$H_twodown_hrs <- ifelse((data$H_highest_education_parent - data$Harmonized_education) == -2, 1, 0)

# Mobility status
data$H_mobility_status_hrs <- NA
data$H_mobility_status_hrs[data$H_highest_education_parent == 1 & data$Harmonized_education == 1] <- 1
data$H_mobility_status_hrs[data$H_highest_education_parent == 2 & data$Harmonized_education == 2] <- 2
data$H_mobility_status_hrs[data$H_highest_education_parent == 3 & data$Harmonized_education == 3] <- 3
data$H_mobility_status_hrs[data$H_mobility_steps_hrs %in% -2:-1] <- 0
data$H_mobility_status_hrs[data$H_mobility_steps_hrs %in% 1:2] <- 4
attr(data$H_mobility_status_hrs, "label") <- "Mobility status"
data$H_mobility_status_hrs <- factor(data$H_mobility_status_hrs,
                                 levels = c(0, 1, 2, 4, 3),
                                 labels = c("Downwardly mobile", "Stable low","Stable mid","Upwardly mobile","Stable high" ))
data$H_mobility_status_hrs <- relevel(data$H_mobility_status_hrs, ref = "Stable low")
table(data$H_mobility_status_hrs)

# Mobility stability
data$H_mobility_stability_hrs <- data$H_mobility_status_hrs
data$H_mobility_stability_hrs <- as.character(data$H_mobility_stability_hrs)
data$H_mobility_stability_hrs <- factor(data$H_mobility_stability_hrs,
                                    levels = c("Downwardly mobile", "Stable low", "Stable mid", "Stable high", "Upwardly mobile"),labels = c("0", "1", "1", "1", "2"))
attr(data$H_mobility_stability_hrs, "label") <- "3-level mobility classification"
data$H_mobility_stability_hrs <- factor(data$H_mobility_stability_hrs,
                                    levels = c(0, 1, 2),
                                    labels = c("Downwardly mobile", "Non-mobile", "Upwardly mobile"))
table(data$H_mobility_stability_hrs)

#preparing data
data <- data  
#data$edu_hrs <- factor(data$Harmonized_education)
data$pedu_hrs <- factor(data$H_highest_education_parent)
data$sex_hrs <- factor(data$sex_hrs)
data$Race <- factor(data$Race, levels = c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Other"))
data$Race <- relevel(data$Race, ref = "Non-Hispanic White")
data$urbanicity_hrs <- factor(data$urbanicity_hrs) 
data$urbanicity_hrs <- relevel(data$urbanicity_hrs, ref = "Urban")  

#conditional means
model <- lm(fgcp_hrs ~ H_highest_education_parent + Harmonized_education + age_hrs + sex_hrs + Race + urbanicity_hrs, data = data)
data$adjusted_fgcp_hrs <- predict(model, newdata = data)
conditional_means_adjusted <- data %>%
  group_by(H_highest_education_parent, Harmonized_education) %>%
  summarise(mean_adjusted_fgcp_hrs = mean(adjusted_fgcp_hrs, na.rm = TRUE))
print(conditional_means_adjusted)

#check sample size for each country
sample_size <- data %>% 
  group_by(H_highest_education_parent, Harmonized_education)  %>% count()

sample_size <- data %>% 
  group_by(mobility_status_hrs)  %>% count()

##----percentile_edu----
# Mobility in any direction
data$mobile_hrs <- ifelse(data$education_quan != data$education_p_quan, 1, 0)
attr(data$mobile_hrs, "label") <- "Mobility in any direction"
data$mobile_hrs <- factor(data$mobile_hrs, levels = c(0, 1), labels = c("Non-mobile", "Mobile"))

# Number of mobility steps
data$mobility_steps_hrs <- data$education_quan - data$education_p_quan
attr(data$mobility_steps_hrs, "label") <- "Inter-generational extent of mobility"

# Upwards mobility
data$upwards_hrs <- ifelse(data$mobility_steps_hrs %in% 1:2, 1, 0)
attr(data$upwards_hrs, "label") <- "Upwardly mobile"

# Downwards mobility
data$downwards_hrs <- ifelse(data$mobility_steps_hrs %in% -2:-1, 1, 0)
attr(data$downwards_hrs, "label") <- "Downwardly mobile"

# Number of steps upwardly mobile
data$oneup_hrs <- ifelse((data$education_p_quan - data$education_quan) == 1, 1, 0)
data$twoup_hrs <- ifelse((data$education_p_quan - data$education_quan) == 2, 1, 0)

# Number of steps downwardly mobile
data$onedown_hrs <- ifelse((data$education_p_quan - data$education_quan) == -1, 1, 0)
data$twodown_hrs <- ifelse((data$education_p_quan - data$education_quan) == -2, 1, 0)

# Mobility status
data$mobility_status_hrs <- NA
data$mobility_status_hrs[data$education_p_quan == 1 & data$education_quan == 1] <- 1
data$mobility_status_hrs[data$education_p_quan == 2 & data$education_quan == 2] <- 2
data$mobility_status_hrs[data$education_p_quan == 3 & data$education_quan == 3] <- 3
data$mobility_status_hrs[data$mobility_steps_hrs %in% -2:-1] <- 0
data$mobility_status_hrs[data$mobility_steps_hrs %in% 1:2] <- 4
attr(data$mobility_status_hrs, "label") <- "Mobility status"
data$mobility_status_hrs <- factor(data$mobility_status_hrs,
                                    levels = c(0, 1, 2, 4, 3),
                                    labels = c("Downwardly mobile", "Stable low","Stable mid",
                                               "Upwardly mobile","Stable high"))

data$mobility_status_hrs <- relevel(data$mobility_status_hrs, ref = "Stable low")
table(data$mobility_status_hrs)

# Mobility stability
data$mobility_stability_hrs <- data$mobility_status_hrs
data$mobility_stability_hrs <- as.character(data$mobility_stability_hrs)
data$mobility_stability_hrs <- factor(data$mobility_stability_hrs,
                                       levels = c("Downwardly mobile", "Stable low", "Stable mid", "Stable high", "Upwardly mobile"),labels = c("0", "1", "1", "1", "2"))
attr(data$mobility_stability_hrs, "label") <- "3-level mobility classification"
data$mobility_stability_hrs <- factor(data$mobility_stability_hrs,
                                       levels = c(0, 1, 2),
                                       labels = c("Downwardly mobile", "Non-mobile", "Upwardly mobile"))
table(data$mobility_stability_hrs)
#linear regression
##fgcp
model1_fgcp <- lm(fgcp_hrs ~ mobility_status_hrs + age_hrs + sex_hrs+ Race + urbanicity_hrs, data = data)
summary(model1_fgcp)

coef_table <- summary(model1_fgcp)$coefficients
conf_int <- confint(model1_fgcp)

model1_fexf <- lm(fexf_hrs ~ mobility_status_hrs + age_hrs + sex_hrs + Race + urbanicity_hrs, data = data)
model1_flang <- lm(flang_hrs ~ mobility_status_hrs + age_hrs + sex_hrs + Race + urbanicity_hrs, data = data)
model1_forient <- lm(forient_hrs ~ mobility_status_hrs + age_hrs + sex_hrs + Race + urbanicity_hrs, data = data)
model1_fmem <- lm(fmem_hrs ~ mobility_status_hrs + age_hrs + sex_hrs + Race + urbanicity_hrs, data = data)

##add interaction
#harmonized
model1_fgcp <- lm(fgcp_hrs ~ mobility_status_hrs * sex_hrs + age_hrs  + Race + urbanicity_hrs, data = data)
summary(model1_fgcp)

coef_table <- summary(model1_fgcp)$coefficients
conf_int <- confint(model1_fgcp)

model1_fexf <- lm(fexf_hrs ~ mobility_status_hrs * sex_hrs + age_hrs  + Race + urbanicity_hrs, data = data)
model1_flang <- lm(flang_hrs ~ mobility_status_hrs * sex_hrs + age_hrs  + Race + urbanicity_hrs, data = data)
model1_forient <- lm(forient_hrs ~ mobility_status_hrs * sex_hrs + age_hrs  + Race + urbanicity_hrs, data = data)
model1_fmem <- lm(fmem_hrs ~ mobility_status_hrs * sex_hrs + age_hrs  + Race + urbanicity_hrs, data = data)
tab_model(model1_fgcp, model1_fexf, model1_flang, model1_forient, model1_fmem, show.se = T, show.ci = F)

#percentile rank
model1_fgcp <- lm(fgcp_hrs ~ mobility_status_hrs * sex_hrs + age_hrs  + Race + urbanicity_hrs, data = data)
summary(model1_fgcp)

coef_table <- summary(model1_fgcp)$coefficients
conf_int <- confint(model1_fgcp)

model1_fexf <- lm(fexf_hrs ~ mobility_status_hrs * sex_hrs + age_hrs  + Race + urbanicity_hrs, data = data)
model1_flang <- lm(flang_hrs ~ mobility_status_hrs * sex_hrs + age_hrs  + Race + urbanicity_hrs, data = data)
model1_forient <- lm(forient_hrs ~ mobility_status_hrs * sex_hrs + age_hrs  + Race + urbanicity_hrs, data = data)
model1_fmem <- lm(fmem_hrs ~ mobility_status_hrs * sex_hrs + age_hrs  + Race + urbanicity_hrs, data = data)
tab_model(model1_fgcp, model1_fexf, model1_flang, model1_forient, model1_fmem, show.se = T, show.ci = F)
write.csv(data,"D:\\R project\\Cognition\\HRS\\stata_hcap_hrs.csv")

