library(patchwork)
library(ggplot2)
library(emmeans)
library(dplyr)
library(sjPlot)
df_hrs <- read.csv("D:\\R project\\Cognition\\hcap_stata_data\\stata_hcap_hrs.csv")
df_elsa <- read.csv("D:\\R project\\Cognition\\hcap_stata_data\\stata_hcap_elsa.csv")
df_mhas <- read.csv("D:\\R project\\Cognition\\hcap_stata_data\\stata_hcap_mhas.csv")
df_lasi <- read.csv("D:\\R project\\Cognition\\hcap_stata_data\\stata_hcap_lasi.csv")
df_charls <- read.csv("D:\\R project\\Cognition\\hcap_stata_data\\stata_hcap_charls.csv")

df_elsa$H_mobility_status_elsa <- ifelse(df_elsa$H_mobility_status_elsa == "Stable mid" , "Stable middle", df_elsa$H_mobility_status_elsa)
df_mhas$H_mobility_status_mhas <- ifelse(df_mhas$H_mobility_status_mhas == "Stable mid" , "Stable middle", df_mhas$H_mobility_status_mhas)
df_lasi$H_mobility_status_lasi <- ifelse(df_lasi$H_mobility_status_lasi == "Stable mid" , "Stable middle", df_lasi$H_mobility_status_lasi)
df_charls$H_mobility_status_charls <- ifelse(df_charls$H_mobility_status_charls == "Stable mid" , "Stable middle", df_charls$H_mobility_status_charls)
df_hrs$H_mobility_status_hrs <- ifelse(df_hrs$H_mobility_status_hrs == "Stable mid" , "Stable middle", df_hrs$H_mobility_status_hrs)


#prepare the data
df_hrs$H_mobility_status_hrs <- factor(df_hrs$H_mobility_status_hrs,
                                     levels = c("Stable low","Downwardly mobile", "Stable middle","Upwardly mobile","Stable high"))
df_hrs$Race <- factor(df_hrs$Race,
                      levels = c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Other"))
df_elsa$H_mobility_status_elsa <- factor(df_elsa$H_mobility_status_elsa,
                                       levels = c("Stable low","Downwardly mobile", "Stable middle","Upwardly mobile","Stable high"))
df_elsa$race <- factor(df_elsa$race,
                       levels = c("White", "Non-White"))
df_mhas$H_mobility_status_mhas <- factor(df_mhas$H_mobility_status_mhas,
                                       levels = c("Stable low","Downwardly mobile", "Stable middle","Upwardly mobile","Stable high"))
df_mhas$urbanicity_mhas <- factor(df_mhas$urbanicity_mhas,
                                  levels = c("rural", "urban"))
df_lasi$H_mobility_status_lasi <- factor(df_lasi$H_mobility_status_lasi,
                                       levels = c("Stable low","Downwardly mobile", "Stable middle","Upwardly mobile","Stable high"))
df_lasi$urbanicity_lasi <- factor(df_lasi$urbanicity_lasi,
                                  levels = c("Rural", "Urban"))
df_lasi$caste <- factor(df_lasi$caste,
                        levels = c("Scheduled caste or scheduled tribe", "Other"))


df_charls$H_mobility_status_charls <- factor(df_charls$H_mobility_status_charls,
                                             levels = c("Stable low","Downwardly mobile", "Stable middle","Upwardly mobile","Stable high"))
df_charls$urbanicity_charls <- factor(df_charls$urbanicity_charls,
                                      levels = c("rural", "urban"))
df_charls$hukou <- factor(df_charls$hukou,
                          levels = c("Agricultual", "Non-agricultual"))
table(df_hrs$H_mobility_status_hrs)
table(df_elsa$H_mobility_status_elsa)
table(df_mhas$H_mobility_status_mhas)
table(df_lasi$H_mobility_status_lasi)
table(df_charls$H_mobility_status_charls)

df_hrs <- df_hrs %>%
  mutate(mobility_status_hrs = case_when(
    mobility_status_hrs == "Stable low" ~ "Stably low",
    mobility_status_hrs == "Stable middle" ~ "Stably middle",
    mobility_status_hrs == "Stable high" ~ "Stably high",
    TRUE ~ mobility_status_hrs  
  ))

df_hrs <- df_hrs %>%
  mutate(H_mobility_status_hrs = case_when(
    H_mobility_status_hrs == "Stable low" ~ "Stably low",
    H_mobility_status_hrs == "Stable middle" ~ "Stably middle",
    H_mobility_status_hrs == "Stable high" ~ "Stably high",
    TRUE ~ H_mobility_status_hrs  
  ))

df_elsa <- df_elsa %>%
  mutate(mobility_status_elsa = case_when(
    mobility_status_elsa == "Stable low" ~ "Stably low",
    mobility_status_elsa == "Stable middle" ~ "Stably middle",
    mobility_status_elsa == "Stable high" ~ "Stably high",
    TRUE ~ mobility_status_elsa  
  ))

df_elsa <- df_elsa %>%
  mutate(H_mobility_status_elsa = case_when(
    H_mobility_status_elsa == "Stable low" ~ "Stably low",
    H_mobility_status_elsa == "Stable middle" ~ "Stably middle",
    H_mobility_status_elsa == "Stable high" ~ "Stably high",
    TRUE ~ H_mobility_status_elsa  
  ))

df_mhas <- df_mhas %>%
  mutate(mobility_status_mhas = case_when(
    mobility_status_mhas == "Stable low" ~ "Stably low",
    mobility_status_mhas == "Stable middle" ~ "Stably middle",
    mobility_status_mhas == "Stable high" ~ "Stably high",
    TRUE ~ mobility_status_mhas  
  ))

df_mhas <- df_mhas %>%
  mutate(H_mobility_status_mhas = case_when(
    H_mobility_status_mhas == "Stable low" ~ "Stably low",
    H_mobility_status_mhas == "Stable middle" ~ "Stably middle",
    H_mobility_status_mhas == "Stable high" ~ "Stably high",
    TRUE ~ H_mobility_status_mhas  
  ))

df_charls <- df_charls %>%
  mutate(mobility_status_charls = case_when(
    mobility_status_charls == "Stable low" ~ "Stably low",
    mobility_status_charls == "Stable middle" ~ "Stably middle",
    mobility_status_charls == "Stable high" ~ "Stably high",
    TRUE ~ mobility_status_charls  
  ))

df_charls <- df_charls %>%
  mutate(H_mobility_status_charls = case_when(
    H_mobility_status_charls == "Stable low" ~ "Stably low",
    H_mobility_status_charls == "Stable middle" ~ "Stably middle",
    H_mobility_status_charls == "Stable high" ~ "Stably high",
    TRUE ~ H_mobility_status_charls  
  ))

df_lasi <- df_lasi %>%
  mutate(mobility_status_lasi = case_when(
    mobility_status_lasi == "Stable low" ~ "Stably low",
    mobility_status_lasi == "Stable middle" ~ "Stably middle",
    mobility_status_lasi == "Stable high" ~ "Stably high",
    TRUE ~ mobility_status_lasi  
  ))

df_lasi <- df_lasi %>%
  mutate(H_mobility_status_lasi = case_when(
    H_mobility_status_lasi == "Stable low" ~ "Stably low",
    H_mobility_status_lasi == "Stable middle" ~ "Stably middle",
    H_mobility_status_lasi == "Stable high" ~ "Stably high",
    TRUE ~ H_mobility_status_lasi  
  ))

df_hrs$H_mobility_status_hrs <- factor(df_hrs$H_mobility_status_hrs,
                                       levels = c("Stably low","Downwardly mobile", "Stably middle","Upwardly mobile","Stably high"))
df_elsa$H_mobility_status_elsa <- factor(df_elsa$H_mobility_status_elsa,
                                         levels = c("Stably low","Downwardly mobile", "Stably middle","Upwardly mobile","Stably high"))
df_mhas$H_mobility_status_mhas <- factor(df_mhas$H_mobility_status_mhas,
                                         levels = c("Stably low","Downwardly mobile", "Stably middle","Upwardly mobile","Stably high"))
df_lasi$H_mobility_status_lasi <- factor(df_lasi$H_mobility_status_lasi,
                                         levels = c("Stably low","Downwardly mobile", "Stably middle","Upwardly mobile","Stably high"))
df_charls$H_mobility_status_charls <- factor(df_charls$H_mobility_status_charls,
                                             levels = c("Stably low","Downwardly mobile", "Stably middle","Upwardly mobile","Stably high"))

####linear regression####
#linear regression
#forest plot function
#forest plot function
create_forest_plot_hrs <- function(model, title) {
  coef_table <- summary(model)$coefficients
  conf_int <- confint(model)
  
  # Find mobility status terms dynamically
  mobility_terms <- rownames(coef_table)[grep("^H_mobility_status_hrs", rownames(coef_table))]
  
  forest_data <- data.frame(
    Term = mobility_terms,
    Estimate = coef_table[mobility_terms, "Estimate"],
    LowerCI = conf_int[mobility_terms, 1],
    UpperCI = conf_int[mobility_terms, 2],
    PValue = coef_table[mobility_terms, "Pr(>|t|)"]
  )
  
  # Add the reference group (with Estimate = 0)
  reference_data <- data.frame(
    Term = "Stably low (ref)",
    Estimate = 0,
    LowerCI = 0,
    UpperCI = 0,
    PValue = NA
  )
  
  forest_data <- rbind(reference_data, forest_data)
  
  # Modify the labels
  forest_data$Label <- recode(forest_data$Term,
                              "H_mobility_status_hrsDownwardly mobile" = "Downwardly mobile",
                              "H_mobility_status_hrsStably middle" = "Stably middle",
                              "H_mobility_status_hrsUpwardly mobile" = "Upwardly mobile",
                              "H_mobility_status_hrsStably high" = "Stably high",
                              "Stable low (ref)" = "Stably low (ref)"  
  )
  
  # Ensure correct ordering of factors
  forest_data$Label <- factor(
    forest_data$Label,
    levels = c(
      "Stably low (ref)", "Downwardly mobile", "Stably middle", 
      "Upwardly mobile", "Stably high"
    )
  )
  
  ggplot(forest_data, aes(x = Label, y = Estimate)) +
    geom_point(color = "black", size = 3) +
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), color = "black", width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    scale_y_continuous(
      breaks = c(0, 1, 2), 
      labels = c("0", "1", "2"), 
      limits = c(-0.5, 3)
    ) +
    labs(
      title = title,
      y = "Coefficient Estimate (95% CI)",
      x = ""
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      plot.title = element_text(size = 16),
      panel.grid = element_blank()
    )
}
####us####
model1_fgcp <- lm(fgcp_hrs ~ H_mobility_status_hrs + age_hrs + sex_hrs + Race + urbanicity_hrs,data = df_hrs)
hrs_fgcp <- create_forest_plot_hrs(model1_fgcp, " ")
print(hrs_fgcp)
summary(model1_fgcp)

model1_fexf <- lm(fexf_hrs ~ H_mobility_status_hrs + age_hrs + sex_hrs + Race + urbanicity_hrs, data = df_hrs)
hrs_fexf <- create_forest_plot_hrs(model1_fexf, " ")
print(hrs_fexf)

model1_flang <- lm(flang_hrs ~ H_mobility_status_hrs + age_hrs + sex_hrs + Race + urbanicity_hrs, data = df_hrs)
hrs_flang <- create_forest_plot_hrs(model1_flang, " ")
print(hrs_flang)

model1_forient <- lm(forient_hrs ~ H_mobility_status_hrs + age_hrs + sex_hrs + Race + urbanicity_hrs, data = df_hrs)
hrs_forient <- create_forest_plot_hrs(model1_forient, " ")
print(hrs_forient)

model1_fmem <- lm(fmem_hrs ~ H_mobility_status_hrs + age_hrs + sex_hrs + Race + urbanicity_hrs, data = df_hrs)
hrs_fmem <- create_forest_plot_hrs(model1_fmem, " ")
print(hrs_fmem)
tab_model(model1_fgcp,model1_fexf, model1_flang,model1_forient,model1_fmem)

####ENGLAND####
create_forest_plot_elsa <- function(model, title) {
  coef_table <- summary(model)$coefficients
  conf_int <- confint(model)
  
  # Find mobility status terms dynamically
  mobility_terms <- rownames(coef_table)[grep("^H_mobility_status_elsa", rownames(coef_table))]
  
  forest_data <- data.frame(
    Term = mobility_terms,
    Estimate = coef_table[mobility_terms, "Estimate"],
    LowerCI = conf_int[mobility_terms, 1],
    UpperCI = conf_int[mobility_terms, 2],
    PValue = coef_table[mobility_terms, "Pr(>|t|)"]
  )
  
  # Add the reference group (with Estimate = 0)
  reference_data <- data.frame(
    Term = "Stable low (ref)",
    Estimate = 0,
    LowerCI = 0,
    UpperCI = 0,
    PValue = NA
  )
  
  forest_data <- rbind(reference_data, forest_data)
  
  # Modify the labels
  forest_data$Label <- recode(forest_data$Term,
                              "H_mobility_status_elsaDownwardly mobile" = "Downwardly mobile",
                              "H_mobility_status_elsaStably middle" = "Stably middle",
                              "H_mobility_status_elsaUpwardly mobile" = "Upwardly mobile",
                              "H_mobility_status_elsaStably high" = "Stably high",
                              "Stable low (ref)" = "Stably low (ref)"  
  )
  
  # Ensure correct ordering of factors
  forest_data$Label <- factor(
    forest_data$Label,
    levels = c(
      "Stably low (ref)", "Downwardly mobile", "Stably middle", 
      "Upwardly mobile", "Stably high"
    )
  )
  
  ggplot(forest_data, aes(x = Label, y = Estimate)) +
    geom_point(color = "black", size = 3) +
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), color = "black", width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    scale_y_continuous(
      breaks = c(0, 1, 2), 
      labels = c("0", "1", "2"), 
      limits = c(-0.5, 3)
    ) +
    labs(
      title = title,
      y = "Coefficient Estimate (95% CI)",
      x = ""
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      plot.title = element_text(size = 16),
      panel.grid = element_blank()
    )
}

model1_fgcp <- lm(fgcp_elsa ~ H_mobility_status_elsa + age_elsa + sex_elsa + race,data = df_elsa)
elsa_fgcp <- create_forest_plot_elsa(model1_fgcp, " ")
print(elsa_fgcp)
summary(model1_fgcp)

model1_fexf <- lm(fexf_elsa ~ H_mobility_status_elsa + age_elsa + sex_elsa + race, data = df_elsa)
elsa_fexf <- create_forest_plot_elsa(model1_fexf, " ")
print(elsa_fexf)

model1_flang <- lm(flang_elsa ~ H_mobility_status_elsa + age_elsa + sex_elsa + race, data = df_elsa)
elsa_flang <- create_forest_plot_elsa(model1_flang, " ")
print(elsa_flang)

model1_forient <- lm(forient_elsa ~ H_mobility_status_elsa + age_elsa + sex_elsa + race, data = df_elsa)
elsa_forient <- create_forest_plot_elsa(model1_forient, " ")
print(elsa_forient)

model1_fmem <- lm(fmem_elsa ~ H_mobility_status_elsa + age_elsa + sex_elsa + race, data = df_elsa)
elsa_fmem <- create_forest_plot_elsa(model1_fmem, " ")
print(elsa_fmem)
tab_model(model1_fgcp,model1_fexf, model1_flang,model1_forient,model1_fmem)

####MEXICO####
create_forest_plot_mhas <- function(model, title) {
  coef_table <- summary(model)$coefficients
  conf_int <- confint(model)
  
  # Find mobility status terms dynamically
  mobility_terms <- rownames(coef_table)[grep("^H_mobility_status_mhas", rownames(coef_table))]
  
  forest_data <- data.frame(
    Term = mobility_terms,
    Estimate = coef_table[mobility_terms, "Estimate"],
    LowerCI = conf_int[mobility_terms, 1],
    UpperCI = conf_int[mobility_terms, 2],
    PValue = coef_table[mobility_terms, "Pr(>|t|)"]
  )
  
  # Add the reference group (with Estimate = 0)
  reference_data <- data.frame(
    Term = "Stably low (ref)",
    Estimate = 0,
    LowerCI = 0,
    UpperCI = 0,
    PValue = NA
  )
  
  forest_data <- rbind(reference_data, forest_data)
  
  # Modify the labels
  forest_data$Label <- recode(forest_data$Term,
                              "H_mobility_status_mhasDownwardly mobile" = "Downwardly mobile",
                              "H_mobility_status_mhasStably middle" = "Stably middle",
                              "H_mobility_status_mhasUpwardly mobile" = "Upwardly mobile",
                              "H_mobility_status_mhasStably high" = "Stably high",
                              "Stably Low (ref)" = "Stably low (ref)"  
  )
  
  # Ensure correct ordering of factors
  forest_data$Label <- factor(
    forest_data$Label,
    levels = c(
      "Stably low (ref)", "Downwardly mobile", "Stably middle", 
      "Upwardly mobile", "Stably high"
    )
  )
  
  ggplot(forest_data, aes(x = Label, y = Estimate)) +
    geom_point(color = "black", size = 3) +
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), color = "black", width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    scale_y_continuous(
      breaks = c(0, 1, 2), 
      labels = c("0", "1", "2"), 
      limits = c(-0.5, 3)
    ) +
    labs(
      title = title,
      y = "Coefficient Estimate (95% CI)",
      x = ""
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      plot.title = element_text(size = 16),
      panel.grid = element_blank()
    )
}

model1_fgcp <- lm(fgcp_mhas ~ H_mobility_status_mhas + age_mhas + sex_mhas + urbanicity_mhas,data = df_mhas)
mhas_fgcp <- create_forest_plot_mhas(model1_fgcp, " ")
print(mhas_fgcp)
summary(model1_fgcp)

model1_fexf <- lm(fexf_mhas ~ H_mobility_status_mhas + age_mhas + sex_mhas + urbanicity_mhas, data = df_mhas)
mhas_fexf <- create_forest_plot_mhas(model1_fexf, " ")
print(mhas_fexf)

model1_flang <- lm(flang_mhas ~ H_mobility_status_mhas + age_mhas + sex_mhas + urbanicity_mhas, data = df_mhas)
mhas_flang <- create_forest_plot_mhas(model1_flang, " ")
print(mhas_flang)

model1_forient <- lm(forient_mhas ~ H_mobility_status_mhas + age_mhas + sex_mhas + urbanicity_mhas, data = df_mhas)
mhas_forient <- create_forest_plot_mhas(model1_forient, " ")
print(mhas_forient)

model1_fmem <- lm(fmem_mhas ~ H_mobility_status_mhas + age_mhas + sex_mhas + urbanicity_mhas, data = df_mhas)
mhas_fmem <- create_forest_plot_mhas(model1_fmem, " ")
print(mhas_fmem)
tab_model(model1_fgcp,model1_fexf, model1_flang,model1_forient,model1_fmem)

####CHINA####
create_forest_plot_charls <- function(model, title) {
  coef_table <- summary(model)$coefficients
  conf_int <- confint(model)
  
  # Find mobility status terms dynamically
  mobility_terms <- rownames(coef_table)[grep("^H_mobility_status_charls", rownames(coef_table))]
  
  forest_data <- data.frame(
    Term = mobility_terms,
    Estimate = coef_table[mobility_terms, "Estimate"],
    LowerCI = conf_int[mobility_terms, 1],
    UpperCI = conf_int[mobility_terms, 2],
    PValue = coef_table[mobility_terms, "Pr(>|t|)"]
  )
  
  # Add the reference group (with Estimate = 0)
  reference_data <- data.frame(
    Term = "Stably low (ref)",
    Estimate = 0,
    LowerCI = 0,
    UpperCI = 0,
    PValue = NA
  )
  
  forest_data <- rbind(reference_data, forest_data)
  
  # Modify the labels
  forest_data$Label <- recode(forest_data$Term,
                              "H_mobility_status_charlsDownwardly mobile" = "Downwardly mobile",
                              "H_mobility_status_charlsStably middle" = "Stably middle",
                              "H_mobility_status_charlsUpwardly mobile" = "Upwardly mobile",
                              "H_mobility_status_charlsStably high" = "Stably high",
                              "Stable low (ref)" = "Stably low (ref)"  
  )
  
  # Ensure correct ordering of factors
  forest_data$Label <- factor(
    forest_data$Label,
    levels = c(
      "Stably low (ref)", "Downwardly mobile", "Stably middle", 
      "Upwardly mobile", "Stably high"
    )
  )
  
  ggplot(forest_data, aes(x = Label, y = Estimate)) +
    geom_point(color = "black", size = 3) +
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), color = "black", width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    scale_y_continuous(
      breaks = c(0, 1, 2), 
      labels = c("0", "1", "2"), 
      limits = c(-0.5, 3)
    ) +
    labs(
      title = title,
      y = "Coefficient Estimate (95% CI)",
      x = ""
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      plot.title = element_text(size = 16),
      panel.grid = element_blank()
    )
}

model1_fgcp <- lm(fgcp_charls ~ H_mobility_status_charls + age_charls + sex_charls + urbanicity_charls + hukou,data = df_charls)
charls_fgcp <- create_forest_plot_charls(model1_fgcp, " ")
print(charls_fgcp)
summary(model1_fgcp)

model1_fexf <- lm(fexf_charls ~ H_mobility_status_charls + age_charls + sex_charls + urbanicity_charls + hukou,data = df_charls)
charls_fexf <- create_forest_plot_charls(model1_fexf, " ")
print(charls_fexf)

model1_flang <- lm(flang_charls ~ H_mobility_status_charls + age_charls + sex_charls + urbanicity_charls + hukou,data = df_charls)
charls_flang <- create_forest_plot_charls(model1_flang, " ")
print(charls_flang)

model1_forient <- lm(forient_charls ~ H_mobility_status_charls + age_charls + sex_charls + urbanicity_charls + hukou,data = df_charls)
charls_forient <- create_forest_plot_charls(model1_forient, " ")
print(charls_forient)

model1_fmem <- lm(fmem_charls ~ H_mobility_status_charls + age_charls + sex_charls + urbanicity_charls + hukou,data = df_charls)
charls_fmem <- create_forest_plot_charls(model1_fmem, " ")
print(charls_fmem)
tab_model(model1_fgcp,model1_fexf, model1_flang,model1_forient,model1_fmem)

####INDIA####
create_forest_plot_lasi <- function(model, title) {
  coef_table <- summary(model)$coefficients
  conf_int <- confint(model)
  
  # Find mobility status terms dynamically
  mobility_terms <- rownames(coef_table)[grep("^H_mobility_status_lasi", rownames(coef_table))]
  
  forest_data <- data.frame(
    Term = mobility_terms,
    Estimate = coef_table[mobility_terms, "Estimate"],
    LowerCI = conf_int[mobility_terms, 1],
    UpperCI = conf_int[mobility_terms, 2],
    PValue = coef_table[mobility_terms, "Pr(>|t|)"]
  )
  
  # Add the reference group (with Estimate = 0)
  reference_data <- data.frame(
    Term = "Stably low (ref)",
    Estimate = 0,
    LowerCI = 0,
    UpperCI = 0,
    PValue = NA
  )
  
  forest_data <- rbind(reference_data, forest_data)
  
  # Modify the labels
  forest_data$Label <- recode(forest_data$Term,
                              "H_mobility_status_lasiDownwardly mobile" = "Downwardly mobile",
                              "H_mobility_status_lasiStably middle" = "Stably middle",
                              "H_mobility_status_lasiUpwardly mobile" = "Upwardly mobile",
                              "H_mobility_status_lasiStably high" = "Stably high",
                              "Stable low (ref)" = "Stably low (ref)"  
  )
  
  # Ensure correct ordering of factors
  forest_data$Label <- factor(
    forest_data$Label,
    levels = c(
      "Stably low (ref)", "Downwardly mobile", "Stably middle", 
      "Upwardly mobile", "Stably high"
    )
  )
  
  ggplot(forest_data, aes(x = Label, y = Estimate)) +
    geom_point(color = "black", size = 3) +
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), color = "black", width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    scale_y_continuous(
      breaks = c(0, 1, 2), 
      labels = c("0", "1", "2"), 
      limits = c(-0.5, 3)
    ) +
    labs(
      title = title,
      y = "Coefficient Estimate (95% CI)",
      x = ""
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      plot.title = element_text(size = 16),
      panel.grid = element_blank()
    )
}

model1_fgcp <- lm(fgcp_lasi ~ H_mobility_status_lasi + sex_lasi + age_lasi + caste + urbanicity_lasi,data = df_lasi)
lasi_fgcp <- create_forest_plot_lasi(model1_fgcp, " ")
print(lasi_fgcp)
summary(model1_fgcp)

model1_fexf <- lm(fexf_lasi ~ H_mobility_status_lasi + age_lasi + sex_lasi + caste + urbanicity_lasi, data = df_lasi)
lasi_fexf <- create_forest_plot_lasi(model1_fexf, " ")
print(lasi_fexf)

model1_flang <- lm(flang_lasi ~ H_mobility_status_lasi + age_lasi + sex_lasi + caste + urbanicity_lasi, data = df_lasi)
lasi_flang <- create_forest_plot_lasi(model1_flang, " ")
print(lasi_flang)

model1_forient <- lm(forient_lasi ~ H_mobility_status_lasi + age_lasi + sex_lasi + caste + urbanicity_lasi, data = df_lasi)
lasi_forient <- create_forest_plot_lasi(model1_forient, " ")
print(lasi_forient)

model1_fmem <- lm(fmem_lasi ~ H_mobility_status_lasi + age_lasi + caste + sex_lasi + urbanicity_lasi, data = df_lasi)
lasi_fmem <- create_forest_plot_lasi(model1_fmem, " ")
print(lasi_fmem)
tab_model(model1_fgcp,model1_fexf, model1_flang,model1_forient,model1_fmem)

# jpeg("D:\\R project\\Cognition\\lr_cognition_H.jpg",res=250,w=6000,h=7000)
# hrs_fgcp + hrs_fexf + hrs_flang + hrs_forient + hrs_fmem + 
#   elsa_fgcp + elsa_fexf + elsa_flang + elsa_forient + elsa_fmem + 
#   mhas_fgcp + mhas_fexf + mhas_flang + mhas_forient + mhas_fmem+ 
#   charls_fgcp + charls_fexf + charls_flang + charls_forient + charls_fmem+ 
#   lasi_fgcp + lasi_fexf + lasi_flang + lasi_forient + lasi_fmem +
#   plot_layout(ncol =  5)
# dev.off()

####Stratified analysis####
#men
# Filter data for Men in each dataset
df_hrs_men <- df_hrs[df_hrs$sex_hrs == "Men", ]
df_elsa_men <- df_elsa[df_elsa$sex_elsa == "Men", ]
df_mhas_men <- df_mhas[df_mhas$sex_mhas == "Men", ]
df_lasi_men <- df_lasi[df_lasi$sex_lasi == "Men", ]
df_charls_men <- df_charls[df_charls$sex_charls == "Men", ]

#hrs
model1_fgcp_hrs_men <- lm(fgcp_hrs ~ H_mobility_status_hrs + age_hrs   + Race + urbanicity_hrs, data = df_hrs_men)
hrs_fgcp_men <- create_forest_plot_hrs(model1_fgcp_hrs_men, "Global cognition")
print(hrs_fgcp_men)
summary(model1_fgcp_hrs_men)

model1_fexf_hrs_men <- lm(fexf_hrs ~ H_mobility_status_hrs + age_hrs   + Race + urbanicity_hrs, data = df_hrs_men)
hrs_fexf_men <- create_forest_plot_hrs(model1_fexf_hrs_men, "Executive function")
print(hrs_fexf_men)

model1_flang_hrs_men <- lm(flang_hrs ~ H_mobility_status_hrs + age_hrs   + Race + urbanicity_hrs, data = df_hrs_men)
hrs_flang_men <- create_forest_plot_hrs(model1_flang_hrs_men, "Language")
print(hrs_flang_men)

model1_forient_hrs_men <- lm(forient_hrs ~ H_mobility_status_hrs + age_hrs   + Race + urbanicity_hrs, data = df_hrs_men)
hrs_forient_men <- create_forest_plot_hrs(model1_forient_hrs_men, "Orientation")
print(hrs_forient_men)

model1_fmem_hrs_men <- lm(fmem_hrs ~ H_mobility_status_hrs + age_hrs  + Race + urbanicity_hrs, data = df_hrs_men)
hrs_fmem_men <- create_forest_plot_hrs(model1_fmem_hrs_men, "Memory")
print(hrs_fmem_men)
tab_model(model1_fgcp_hrs_men,model1_fexf_hrs_men,model1_flang_hrs_men,model1_forient_hrs_men,model1_fmem_hrs_men)

#### ANALYSIS FOR ELSA ####

model1_fgcp_elsa_men <- lm(fgcp_elsa  ~ H_mobility_status_elsa + age_elsa   + race, data = df_elsa_men)
elsa_fgcp_men <- create_forest_plot_elsa(model1_fgcp_elsa_men, " ")
print(elsa_fgcp_men)
summary(model1_fgcp_elsa_men)

model1_fexf_elsa_men <- lm(fexf_elsa  ~ H_mobility_status_elsa + age_elsa   + race, data = df_elsa_men)
elsa_fexf_men <- create_forest_plot_elsa(model1_fexf_elsa_men, " ")
print(elsa_fexf_men)

model1_flang_elsa_men <- lm(flang_elsa  ~ H_mobility_status_elsa + age_elsa + race, data = df_elsa_men)
elsa_flang_men <- create_forest_plot_elsa(model1_flang_elsa_men, " ")
print(elsa_flang_men)

model1_forient_elsa_men <- lm(forient_elsa  ~ H_mobility_status_elsa + age_elsa + race, data = df_elsa_men)
elsa_forient_men <- create_forest_plot_elsa(model1_forient_elsa_men, " ")
print(elsa_forient_men)

model1_fmem_elsa_men <- lm(fmem_elsa  ~ H_mobility_status_elsa + age_elsa  + race, data = df_elsa_men)
elsa_fmem_men <- create_forest_plot_elsa(model1_fmem_elsa_men, " ")
print(elsa_fmem_men)
tab_model(model1_fgcp_elsa_men,model1_fexf_elsa_men,model1_flang_elsa_men,model1_forient_elsa_men,model1_fmem_elsa_men)

#### ANALYSIS FOR MHAS ####

model1_fgcp_mhas_men <- lm(fgcp_mhas ~ H_mobility_status_mhas + age_mhas + urbanicity_mhas, data = df_mhas_men)
mhas_fgcp_men <- create_forest_plot_mhas(model1_fgcp_mhas_men, " ")
print(mhas_fgcp_men)
summary(model1_fgcp_mhas_men)

model1_fexf_mhas_men <- lm(fexf_mhas ~ H_mobility_status_mhas + age_mhas  + urbanicity_mhas, data = df_mhas_men)
mhas_fexf_men <- create_forest_plot_mhas(model1_fexf_mhas_men, " ")
print(mhas_fexf_men)

model1_flang_mhas_men <- lm(flang_mhas ~ H_mobility_status_mhas + age_mhas + urbanicity_mhas, data = df_mhas_men)
mhas_flang_men <- create_forest_plot_mhas(model1_flang_mhas_men, " ")
print(mhas_flang_men)

model1_forient_mhas_men <- lm(forient_mhas ~ H_mobility_status_mhas + age_mhas  + urbanicity_mhas, data = df_mhas_men)
mhas_forient_men <- create_forest_plot_mhas(model1_forient_mhas_men, " ")
print(mhas_forient_men)

model1_fmem_mhas_men <- lm(fmem_mhas ~ H_mobility_status_mhas + age_mhas  + urbanicity_mhas, data = df_mhas_men)
mhas_fmem_men <- create_forest_plot_mhas(model1_fmem_mhas_men, " ")
print(mhas_fmem_men)
tab_model(model1_fgcp_mhas_men,model1_fexf_mhas_men,model1_flang_mhas_men,model1_forient_mhas_men,model1_fmem_mhas_men)

#### ANALYSIS FOR CHALRS ####

model1_fgcp_charls_men <- lm(fgcp_charls ~ H_mobility_status_charls + age_charls + urbanicity_charls + hukou, data = df_charls_men)
charls_fgcp_men <- create_forest_plot_charls(model1_fgcp_charls_men, " ")
print(charls_fgcp_men)
summary(model1_fgcp_charls_men)

model1_fexf_charls_men <- lm(fexf_charls ~ H_mobility_status_charls + age_charls  + urbanicity_charls+ hukou, data = df_charls_men)
charls_fexf_men <- create_forest_plot_charls(model1_fexf_charls_men, " ")
print(charls_fexf_men)

model1_flang_charls_men <- lm(flang_charls ~ H_mobility_status_charls + age_charls + urbanicity_charls+ hukou, data = df_charls_men)
charls_flang_men <- create_forest_plot_charls(model1_flang_charls_men, " ")
print(charls_flang_men)

model1_forient_charls_men <- lm(forient_charls ~ H_mobility_status_charls + age_charls  + urbanicity_charls+ hukou, data = df_charls_men)
charls_forient_men <- create_forest_plot_charls(model1_forient_charls_men, " ")
print(charls_forient_men)

model1_fmem_charls_men <- lm(fmem_charls ~ H_mobility_status_charls + age_charls  + urbanicity_charls+ hukou, data = df_charls_men)
charls_fmem_men <- create_forest_plot_charls(model1_fmem_charls_men, " ")
print(charls_fmem_men)
tab_model(model1_fgcp_charls_men,model1_fexf_charls_men,model1_flang_charls_men,model1_forient_charls_men,model1_fmem_charls_men)

#### ANALYSIS FOR LASI ####

model1_fgcp_lasi_men <- lm(fgcp_lasi ~ H_mobility_status_lasi + age_lasi + caste + urbanicity_lasi, data = df_lasi_men)
lasi_fgcp_men <- create_forest_plot_lasi(model1_fgcp_lasi_men, " ")
print(lasi_fgcp_men)
summary(model1_fgcp_lasi_men)

model1_fexf_lasi_men <- lm(fexf_lasi ~ H_mobility_status_lasi + age_lasi + caste + urbanicity_lasi, data = df_lasi_men)
lasi_fexf_men <- create_forest_plot_lasi(model1_fexf_lasi_men, " ")
print(lasi_fexf_men)

model1_flang_lasi_men <- lm(flang_lasi ~ H_mobility_status_lasi + age_lasi + caste + urbanicity_lasi, data = df_lasi_men)
lasi_flang_men <- create_forest_plot_lasi(model1_flang_lasi_men, " ")
print(lasi_flang_men)

model1_forient_lasi_men <- lm(forient_lasi ~ H_mobility_status_lasi + age_lasi + caste + urbanicity_lasi, data = df_lasi_men)
lasi_forient_men <- create_forest_plot_lasi(model1_forient_lasi_men, " ")
print(lasi_forient_men)

model1_fmem_lasi_men <- lm(fmem_lasi ~ H_mobility_status_lasi + age_lasi + caste + urbanicity_lasi, data = df_lasi_men)
lasi_fmem_men <- create_forest_plot_lasi(model1_fmem_lasi_men, " ")
print(lasi_fmem_men)
tab_model(model1_fgcp_lasi_men,model1_fexf_lasi_men,model1_flang_lasi_men,model1_forient_lasi_men,model1_fmem_lasi_men)

#### EXPORT PLOTS ####
# jpeg("D:\\R project\\Cognition\\linear regression_harmonized_education_men.jpg", res = 250, w = 6000, h = 7000)
# hrs_fgcp_men + hrs_fexf_men + hrs_flang_men + hrs_forient_men + hrs_fmem_men + 
#   elsa_fgcp_men + elsa_fexf_men + elsa_flang_men + elsa_forient_men + elsa_fmem_men + 
#   mhas_fgcp_men + mhas_fexf_men + mhas_flang_men + mhas_forient_men + mhas_fmem_men +
#   charls_fgcp_men + charls_fexf_men + charls_flang_men + charls_forient_men + charls_fmem_men + 
#   lasi_fgcp_men + lasi_fexf_men + lasi_flang_men + lasi_forient_men + lasi_fmem_men + 
#   plot_layout(ncol = 5)
# dev.off()

#women
# Filter data for Men in each dataset
df_hrs_women <- df_hrs[df_hrs$sex_hrs == "Women", ]
df_elsa_women <- df_elsa[df_elsa$sex_elsa == "Women", ]
df_mhas_women <- df_mhas[df_mhas$sex_mhas == "Women", ]
df_lasi_women <- df_lasi[df_lasi$sex_lasi == "Women", ]
df_charls_women <- df_charls[df_charls$sex_charls == "Women", ]
#hrs

model1_fgcp_hrs_women <- lm(fgcp_hrs ~ H_mobility_status_hrs + age_hrs   + Race + urbanicity_hrs, data = df_hrs_women)
hrs_fgcp_women <- create_forest_plot_hrs(model1_fgcp_hrs_women, " ")
print(hrs_fgcp_women)
summary(model1_fgcp_hrs_women)

model1_fexf_hrs_women <- lm(fexf_hrs ~ H_mobility_status_hrs + age_hrs   + Race + urbanicity_hrs, data = df_hrs_women)
hrs_fexf_women <- create_forest_plot_hrs(model1_fexf_hrs_women, " ")
print(hrs_fexf_women)

model1_flang_hrs_women <- lm(flang_hrs ~ H_mobility_status_hrs + age_hrs   + Race + urbanicity_hrs, data = df_hrs_women)
hrs_flang_women <- create_forest_plot_hrs(model1_flang_hrs_women, " ")
print(hrs_flang_women)

model1_forient_hrs_women <- lm(forient_hrs ~ H_mobility_status_hrs + age_hrs   + Race + urbanicity_hrs, data = df_hrs_women)
hrs_forient_women <- create_forest_plot_hrs(model1_forient_hrs_women, " ")
print(hrs_forient_women)

model1_fmem_hrs_women <- lm(fmem_hrs ~ H_mobility_status_hrs + age_hrs  + Race + urbanicity_hrs, data = df_hrs_women)
hrs_fmem_women <- create_forest_plot_hrs(model1_fmem_hrs_women, " ")
print(hrs_fmem_women)
tab_model(model1_fgcp_hrs_women,model1_fexf_hrs_women,model1_flang_hrs_women,model1_forient_hrs_women,model1_fmem_hrs_women)

#### ANALYSIS FOR ELSA ####

model1_fgcp_elsa_women <- lm(fgcp_elsa  ~ H_mobility_status_elsa + age_elsa   + race, data = df_elsa_women)
elsa_fgcp_women <- create_forest_plot_elsa(model1_fgcp_elsa_women, " ")
print(elsa_fgcp_women)
summary(model1_fgcp_elsa_women)

model1_fexf_elsa_women <- lm(fexf_elsa  ~ H_mobility_status_elsa + age_elsa   + race, data = df_elsa_women)
elsa_fexf_women <- create_forest_plot_elsa(model1_fexf_elsa_women, " ")
print(elsa_fexf_women)

model1_flang_elsa_women <- lm(flang_elsa  ~ H_mobility_status_elsa + age_elsa + race, data = df_elsa_women)
elsa_flang_women <- create_forest_plot_elsa(model1_flang_elsa_women, " ")
print(elsa_flang_women)

model1_forient_elsa_women <- lm(forient_elsa  ~ H_mobility_status_elsa + age_elsa + race, data = df_elsa_women)
elsa_forient_women <- create_forest_plot_elsa(model1_forient_elsa_women, " ")
print(elsa_forient_women)

model1_fmem_elsa_women <- lm(fmem_elsa  ~ H_mobility_status_elsa + age_elsa  + race, data = df_elsa_women)
elsa_fmem_women <- create_forest_plot_elsa(model1_fmem_elsa_women, " ")
print(elsa_fmem_women)
tab_model(model1_fgcp_elsa_women,model1_fexf_elsa_women,model1_flang_elsa_women,model1_forient_elsa_women,model1_fmem_elsa_women)

#### ANALYSIS FOR MHAS ####

model1_fgcp_mhas_women <- lm(fgcp_mhas ~ H_mobility_status_mhas + age_mhas + urbanicity_mhas, data = df_mhas_women)
mhas_fgcp_women <- create_forest_plot_mhas(model1_fgcp_mhas_women, " ")
print(mhas_fgcp_women)
summary(model1_fgcp_mhas_women)

model1_fexf_mhas_women <- lm(fexf_mhas ~ H_mobility_status_mhas + age_mhas  + urbanicity_mhas, data = df_mhas_women)
mhas_fexf_women <- create_forest_plot_mhas(model1_fexf_mhas_women, " ")
print(mhas_fexf_women)

model1_flang_mhas_women <- lm(flang_mhas ~ H_mobility_status_mhas + age_mhas + urbanicity_mhas, data = df_mhas_women)
mhas_flang_women <- create_forest_plot_mhas(model1_flang_mhas_women, " ")
print(mhas_flang_women)

model1_forient_mhas_women <- lm(forient_mhas ~ H_mobility_status_mhas + age_mhas  + urbanicity_mhas, data = df_mhas_women)
mhas_forient_women <- create_forest_plot_mhas(model1_forient_mhas_women, " ")
print(mhas_forient_women)

model1_fmem_mhas_women <- lm(fmem_mhas ~ H_mobility_status_mhas + age_mhas  + urbanicity_mhas, data = df_mhas_women)
mhas_fmem_women <- create_forest_plot_mhas(model1_fmem_mhas_women, " ")
print(mhas_fmem_women)
tab_model(model1_fgcp_mhas_women,model1_fexf_mhas_women,model1_flang_mhas_women,model1_forient_mhas_women,model1_fmem_mhas_women)

#### ANALYSIS FOR CHARLS ####

model1_fgcp_charls_women <- lm(fgcp_charls ~ H_mobility_status_charls + age_charls + urbanicity_charls +hukou, data = df_charls_women)
charls_fgcp_women <- create_forest_plot_charls(model1_fgcp_charls_women, " ")
print(charls_fgcp_women)
summary(model1_fgcp_charls_women)

model1_fexf_charls_women <- lm(fexf_charls ~ H_mobility_status_charls + age_charls  + urbanicity_charls+hukou, data = df_charls_women)
charls_fexf_women <- create_forest_plot_charls(model1_fexf_charls_women, " ")
print(charls_fexf_women)

model1_flang_charls_women <- lm(flang_charls ~ H_mobility_status_charls + age_charls + urbanicity_charls+hukou, data = df_charls_women)
charls_flang_women <- create_forest_plot_charls(model1_flang_charls_women, " ")
print(charls_flang_women)

model1_forient_charls_women <- lm(forient_charls ~ H_mobility_status_charls + age_charls  + urbanicity_charls+hukou, data = df_charls_women)
charls_forient_women <- create_forest_plot_charls(model1_forient_charls_women, " ")
print(charls_forient_women)

model1_fmem_charls_women <- lm(fmem_charls ~ H_mobility_status_charls + age_charls  + urbanicity_charls+hukou, data = df_charls_women)
charls_fmem_women <- create_forest_plot_charls(model1_fmem_charls_women, " ")
print(charls_fmem_women)
tab_model(model1_fgcp_charls_women,model1_fexf_charls_women,model1_flang_charls_women,model1_forient_charls_women,model1_fmem_charls_women)

#### ANALYSIS FOR LASI ####

model1_fgcp_lasi_women <- lm(fgcp_lasi ~ H_mobility_status_lasi + age_lasi + caste + urbanicity_lasi, data = df_lasi_women)
lasi_fgcp_women <- create_forest_plot_lasi(model1_fgcp_lasi_women, " ")
print(lasi_fgcp_women)
summary(model1_fgcp_lasi_women)

model1_fexf_lasi_women <- lm(fexf_lasi ~ H_mobility_status_lasi + age_lasi + caste + urbanicity_lasi, data = df_lasi_women)
lasi_fexf_women <- create_forest_plot_lasi(model1_fexf_lasi_women, " ")
print(lasi_fexf_women)

model1_flang_lasi_women <- lm(flang_lasi ~ H_mobility_status_lasi + age_lasi + caste + urbanicity_lasi, data = df_lasi_women)
lasi_flang_women <- create_forest_plot_lasi(model1_flang_lasi_women, " ")
print(lasi_flang_women)

model1_forient_lasi_women <- lm(forient_lasi ~ H_mobility_status_lasi + age_lasi + caste + urbanicity_lasi, data = df_lasi_women)
lasi_forient_women <- create_forest_plot_lasi(model1_forient_lasi_women, " ")
print(lasi_forient_women)

model1_fmem_lasi_women <- lm(fmem_lasi ~ H_mobility_status_lasi + age_lasi + caste + urbanicity_lasi, data = df_lasi_women)
lasi_fmem_women <- create_forest_plot_lasi(model1_fmem_lasi_women, " ")
print(lasi_fmem_women)
tab_model(model1_fgcp_lasi_women,model1_fexf_lasi_women,model1_flang_lasi_women,model1_forient_lasi_women,model1_fmem_lasi_women)

#### EXPORT PLOTS ####
# jpeg("D:\\R project\\Cognition\\linear regression_harmonized_education_women.jpg", res = 250, w = 6000, h = 7000)
# hrs_fgcp_women + hrs_fexf_women + hrs_flang_women + hrs_forient_women + hrs_fmem_women + 
#   elsa_fgcp_women + elsa_fexf_women + elsa_flang_women + elsa_forient_women + elsa_fmem_women + 
#   mhas_fgcp_women + mhas_fexf_women + mhas_flang_women + mhas_forient_women + mhas_fmem_women + 
#   charls_fgcp_women + charls_fexf_women + charls_flang_women + charls_forient_women + charls_fmem_women + 
#   lasi_fgcp_women + lasi_fexf_women + lasi_flang_women + lasi_forient_women + lasi_fmem_women + 
#   plot_layout(ncol = 5)
# dev.off()

####EXPORT UPDATED PLOTS
#1.global cognition-overall/men/women
jpeg("D:\\R project\\Cognition\\Sfigure10.jpg",res=350,w=8800,h=8000)
hrs_fgcp + elsa_fgcp + mhas_fgcp + charls_fgcp + lasi_fgcp +
  hrs_fexf + elsa_fexf + mhas_fexf + charls_fexf + lasi_fexf +
  hrs_flang + elsa_flang + mhas_flang + charls_flang + lasi_flang +
  hrs_forient + elsa_forient + mhas_forient + charls_forient + lasi_forient +
  hrs_fmem + elsa_fmem + mhas_fmem + charls_fmem + lasi_fmem +
  plot_layout(ncol =  5)
dev.off()

####Harmonized####
#chi-square test hrs
contingency_table <- table(
  df_hrs$H_mobility_status_hrs,
  df_hrs$sex_hrs
)

combined_table <- paste(
  contingency_table, 
  " (", 
  round(prop.table(contingency_table, margin = 2) * 100, 1), 
  "%)", 
  sep = ""
)

dim(combined_table) <- dim(contingency_table)
rownames(combined_table) <- rownames(contingency_table)
colnames(combined_table) <- colnames(contingency_table)

print(combined_table)
chi_test <- chisq.test(contingency_table)
print(chi_test)

#chi-square test elsa
contingency_table <- table(
  df_elsa$H_mobility_status_elsa,
  df_elsa$sex_elsa
)

combined_table <- paste(
  contingency_table, 
  " (", 
  round(prop.table(contingency_table, margin = 2) * 100, 1), 
  "%)", 
  sep = ""
)

dim(combined_table) <- dim(contingency_table)
rownames(combined_table) <- rownames(contingency_table)
colnames(combined_table) <- colnames(contingency_table)

print(combined_table)
chi_test <- chisq.test(contingency_table)
print(chi_test)

#chi-square test mhas
contingency_table <- table(
  df_mhas$H_mobility_status_mhas,
  df_mhas$sex_mhas
)

combined_table <- paste(
  contingency_table, 
  " (", 
  round(prop.table(contingency_table, margin = 2) * 100, 1), 
  "%)", 
  sep = ""
)

dim(combined_table) <- dim(contingency_table)
rownames(combined_table) <- rownames(contingency_table)
colnames(combined_table) <- colnames(contingency_table)

print(combined_table)
chi_test <- chisq.test(contingency_table)
print(chi_test)

#chi-square test china
contingency_table <- table(
  df_charls$H_mobility_status_charls,
  df_charls$sex_charls
)

combined_table <- paste(
  contingency_table, 
  " (", 
  round(prop.table(contingency_table, margin = 2) * 100, 1), 
  "%)", 
  sep = ""
)

dim(combined_table) <- dim(contingency_table)
rownames(combined_table) <- rownames(contingency_table)
colnames(combined_table) <- colnames(contingency_table)

print(combined_table)
chi_test <- chisq.test(contingency_table)
print(chi_test)

#chi-square test lasi
contingency_table <- table(
  df_lasi$H_mobility_status_lasi,
  df_lasi$sex_lasi
)

combined_table <- paste(
  contingency_table, 
  " (", 
  round(prop.table(contingency_table, margin = 2) * 100, 1), 
  "%)", 
  sep = ""
)

dim(combined_table) <- dim(contingency_table)
rownames(combined_table) <- rownames(contingency_table)
colnames(combined_table) <- colnames(contingency_table)

print(combined_table)
chi_test <- chisq.test(contingency_table)
print(chi_test)




####new plot
# hrs
create_forest_plot_hrs_combined <- function(model_men, model_women, title) {
  coef_table_men <- summary(model_men)$coefficients
  conf_int_men <- confint(model_men)
  mobility_terms_men <- rownames(coef_table_men)[grep("^H_mobility_status_hrs", rownames(coef_table_men))]
  
  forest_data_men <- data.frame(
    Term = mobility_terms_men,
    Estimate = coef_table_men[mobility_terms_men, "Estimate"],
    LowerCI = conf_int_men[mobility_terms_men, 1],
    UpperCI = conf_int_men[mobility_terms_men, 2],
    PValue = coef_table_men[mobility_terms_men, "Pr(>|t|)"],
    Gender = "Men"
  )
  
  coef_table_women <- summary(model_women)$coefficients
  conf_int_women <- confint(model_women)
  mobility_terms_women <- rownames(coef_table_women)[grep("^H_mobility_status_hrs", rownames(coef_table_women))]
  
  forest_data_women <- data.frame(
    Term = mobility_terms_women,
    Estimate = coef_table_women[mobility_terms_women, "Estimate"],
    LowerCI = conf_int_women[mobility_terms_women, 1],
    UpperCI = conf_int_women[mobility_terms_women, 2],
    PValue = coef_table_women[mobility_terms_women, "Pr(>|t|)"],
    Gender = "Women"
  )
  
  forest_data <- rbind(forest_data_men, forest_data_women)
  
  reference_data <- data.frame(
    Term = rep("Stably low (ref)", 2),
    Estimate = c(0, 0),
    LowerCI = c(0, 0),
    UpperCI = c(0, 0),
    PValue = c(NA, NA),
    Gender = c("Men", "Women")
  )
  
  forest_data <- rbind(reference_data, forest_data)
  
  forest_data$Label <- recode(forest_data$Term,
                              "H_mobility_status_hrsDownwardly mobile" = "Downwardly mobile",
                              "H_mobility_status_hrsStably middle" = "Stably middle",
                              "H_mobility_status_hrsUpwardly mobile" = "Upwardly mobile",
                              "H_mobility_status_hrsStably high" = "Stably high",
                              "Stably low (ref)" = "Stably low (ref)"  
  )
  
  forest_data$Label <- factor(
    forest_data$Label,
    levels = c(
      "Stably low (ref)", "Downwardly mobile", "Stably middle", 
      "Upwardly mobile", "Stably high"
    )
  )
  
  forest_data$x_pos <- as.numeric(forest_data$Label)
  forest_data$x_pos[forest_data$Gender == "Men"] <- forest_data$x_pos[forest_data$Gender == "Men"] - 0.2
  forest_data$x_pos[forest_data$Gender == "Women"] <- forest_data$x_pos[forest_data$Gender == "Women"] + 0.2
  
  ggplot(forest_data, aes(x = x_pos, y = Estimate, color = Gender)) +
    geom_point(size = 3, position = position_identity()) +
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.15, position = position_identity()) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    scale_color_manual(values = c("Men" = "#00BFC4", "Women" = "#F8766D")) +
    scale_x_continuous(
      breaks = 1:5,
      labels = levels(forest_data$Label)
    ) +
    scale_y_continuous(
      breaks = c(0, 1, 2), 
      labels = c("0", "1", "2"), 
      limits = c(-0.5, 3)
    ) +
    labs(
      title = title,
      y = "Coefficient Estimate (95% CI)",
      x = ""
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      plot.title = element_text(size = 16),
      legend.position = "none",
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      panel.grid.minor.x = element_blank(),
      panel.grid = element_blank()
    )
}


# elsa
create_forest_plot_elsa_combined <- function(model_men, model_women, title) {
  coef_table_men <- summary(model_men)$coefficients
  conf_int_men <- confint(model_men)
  mobility_terms_men <- rownames(coef_table_men)[grep("^H_mobility_status_elsa", rownames(coef_table_men))]
  
  forest_data_men <- data.frame(
    Term = mobility_terms_men,
    Estimate = coef_table_men[mobility_terms_men, "Estimate"],
    LowerCI = conf_int_men[mobility_terms_men, 1],
    UpperCI = conf_int_men[mobility_terms_men, 2],
    PValue = coef_table_men[mobility_terms_men, "Pr(>|t|)"],
    Gender = "Men"
  )
  
  coef_table_women <- summary(model_women)$coefficients
  conf_int_women <- confint(model_women)
  mobility_terms_women <- rownames(coef_table_women)[grep("^H_mobility_status_elsa", rownames(coef_table_women))]
  
  forest_data_women <- data.frame(
    Term = mobility_terms_women,
    Estimate = coef_table_women[mobility_terms_women, "Estimate"],
    LowerCI = conf_int_women[mobility_terms_women, 1],
    UpperCI = conf_int_women[mobility_terms_women, 2],
    PValue = coef_table_women[mobility_terms_women, "Pr(>|t|)"],
    Gender = "Women"
  )
  
  forest_data <- rbind(forest_data_men, forest_data_women)
  
  reference_data <- data.frame(
    Term = rep("Stably low (ref)", 2),
    Estimate = c(0, 0),
    LowerCI = c(0, 0),
    UpperCI = c(0, 0),
    PValue = c(NA, NA),
    Gender = c("Men", "Women")
  )
  
  forest_data <- rbind(reference_data, forest_data)
  
  forest_data$Label <- recode(forest_data$Term,
                              "H_mobility_status_elsaDownwardly mobile" = "Downwardly mobile",
                              "H_mobility_status_elsaStably middle" = "Stably middle",
                              "H_mobility_status_elsaUpwardly mobile" = "Upwardly mobile",
                              "H_mobility_status_elsaStably high" = "Stably high",
                              "Stably low (ref)" = "Stably low (ref)"  
  )
  
  forest_data$Label <- factor(
    forest_data$Label,
    levels = c(
      "Stably low (ref)", "Downwardly mobile", "Stably middle", 
      "Upwardly mobile", "Stably high"
    )
  )
  
  forest_data$x_pos <- as.numeric(forest_data$Label)
  forest_data$x_pos[forest_data$Gender == "Men"] <- forest_data$x_pos[forest_data$Gender == "Men"] - 0.2
  forest_data$x_pos[forest_data$Gender == "Women"] <- forest_data$x_pos[forest_data$Gender == "Women"] + 0.2
  
  ggplot(forest_data, aes(x = x_pos, y = Estimate, color = Gender)) +
    geom_point(size = 3, position = position_identity()) +
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.15, position = position_identity()) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    scale_color_manual(values = c("Men" = "#00BFC4", "Women" = "#F8766D")) +
    scale_x_continuous(
      breaks = 1:5,
      labels = levels(forest_data$Label)
    ) +
    scale_y_continuous(
      breaks = c(0, 1, 2), 
      labels = c("0", "1", "2"), 
      limits = c(-0.5, 3)
    ) +
    labs(
      title = title,
      y = "Coefficient Estimate (95% CI)",
      x = ""
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      plot.title = element_text(size = 16),
      legend.position = "none",
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      panel.grid.minor.x = element_blank(),
      panel.grid = element_blank()
    )
}

# mhas
create_forest_plot_mhas_combined <- function(model_men, model_women, title) {
  coef_table_men <- summary(model_men)$coefficients
  conf_int_men <- confint(model_men)
  mobility_terms_men <- rownames(coef_table_men)[grep("^H_mobility_status_mhas", rownames(coef_table_men))]
  
  forest_data_men <- data.frame(
    Term = mobility_terms_men,
    Estimate = coef_table_men[mobility_terms_men, "Estimate"],
    LowerCI = conf_int_men[mobility_terms_men, 1],
    UpperCI = conf_int_men[mobility_terms_men, 2],
    PValue = coef_table_men[mobility_terms_men, "Pr(>|t|)"],
    Gender = "Men"
  )
  
  coef_table_women <- summary(model_women)$coefficients
  conf_int_women <- confint(model_women)
  mobility_terms_women <- rownames(coef_table_women)[grep("^H_mobility_status_mhas", rownames(coef_table_women))]
  
  forest_data_women <- data.frame(
    Term = mobility_terms_women,
    Estimate = coef_table_women[mobility_terms_women, "Estimate"],
    LowerCI = conf_int_women[mobility_terms_women, 1],
    UpperCI = conf_int_women[mobility_terms_women, 2],
    PValue = coef_table_women[mobility_terms_women, "Pr(>|t|)"],
    Gender = "Women"
  )
  
  forest_data <- rbind(forest_data_men, forest_data_women)
  
  reference_data <- data.frame(
    Term = rep("Stably low (ref)", 2),
    Estimate = c(0, 0),
    LowerCI = c(0, 0),
    UpperCI = c(0, 0),
    PValue = c(NA, NA),
    Gender = c("Men", "Women")
  )
  
  forest_data <- rbind(reference_data, forest_data)
  
  forest_data$Label <- recode(forest_data$Term,
                              "H_mobility_status_mhasDownwardly mobile" = "Downwardly mobile",
                              "H_mobility_status_mhasStably middle" = "Stably middle",
                              "H_mobility_status_mhasUpwardly mobile" = "Upwardly mobile",
                              "H_mobility_status_mhasStably high" = "Stably high",
                              "Stably low (ref)" = "Stably low (ref)"  
  )
  
  forest_data$Label <- factor(
    forest_data$Label,
    levels = c(
      "Stably low (ref)", "Downwardly mobile", "Stably middle", 
      "Upwardly mobile", "Stably high"
    )
  )
  
  forest_data$x_pos <- as.numeric(forest_data$Label)
  forest_data$x_pos[forest_data$Gender == "Men"] <- forest_data$x_pos[forest_data$Gender == "Men"] - 0.2
  forest_data$x_pos[forest_data$Gender == "Women"] <- forest_data$x_pos[forest_data$Gender == "Women"] + 0.2
  
  ggplot(forest_data, aes(x = x_pos, y = Estimate, color = Gender)) +
    geom_point(size = 3, position = position_identity()) +
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.15, position = position_identity()) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    scale_color_manual(values = c("Men" = "#00BFC4", "Women" = "#F8766D")) +
    scale_x_continuous(
      breaks = 1:5,
      labels = levels(forest_data$Label)
    ) +
    scale_y_continuous(
      breaks = c(0, 1, 2), 
      labels = c("0", "1", "2"), 
      limits = c(-0.5, 3)
    ) +
    labs(
      title = title,
      y = "Coefficient Estimate (95% CI)",
      x = ""
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      plot.title = element_text(size = 16),
      legend.position = "bottom",
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      panel.grid.minor.x = element_blank(),
      panel.grid = element_blank()
    )
}

# charls
create_forest_plot_charls_combined <- function(model_men, model_women, title) {
  coef_table_men <- summary(model_men)$coefficients
  conf_int_men <- confint(model_men)
  mobility_terms_men <- rownames(coef_table_men)[grep("^H_mobility_status_charls", rownames(coef_table_men))]
  
  forest_data_men <- data.frame(
    Term = mobility_terms_men,
    Estimate = coef_table_men[mobility_terms_men, "Estimate"],
    LowerCI = conf_int_men[mobility_terms_men, 1],
    UpperCI = conf_int_men[mobility_terms_men, 2],
    PValue = coef_table_men[mobility_terms_men, "Pr(>|t|)"],
    Gender = "Men"
  )
  
  coef_table_women <- summary(model_women)$coefficients
  conf_int_women <- confint(model_women)
  mobility_terms_women <- rownames(coef_table_women)[grep("^H_mobility_status_charls", rownames(coef_table_women))]
  
  forest_data_women <- data.frame(
    Term = mobility_terms_women,
    Estimate = coef_table_women[mobility_terms_women, "Estimate"],
    LowerCI = conf_int_women[mobility_terms_women, 1],
    UpperCI = conf_int_women[mobility_terms_women, 2],
    PValue = coef_table_women[mobility_terms_women, "Pr(>|t|)"],
    Gender = "Women"
  )
  
  forest_data <- rbind(forest_data_men, forest_data_women)
  
  reference_data <- data.frame(
    Term = rep("Stably low (ref)", 2),
    Estimate = c(0, 0),
    LowerCI = c(0, 0),
    UpperCI = c(0, 0),
    PValue = c(NA, NA),
    Gender = c("Men", "Women")
  )
  
  forest_data <- rbind(reference_data, forest_data)
  
  forest_data$Label <- recode(forest_data$Term,
                              "H_mobility_status_charlsDownwardly mobile" = "Downwardly mobile",
                              "H_mobility_status_charlsStably middle" = "Stably middle",
                              "H_mobility_status_charlsUpwardly mobile" = "Upwardly mobile",
                              "H_mobility_status_charlsStably high" = "Stably high",
                              "Stably low (ref)" = "Stably low (ref)"  
  )
  
  forest_data$Label <- factor(
    forest_data$Label,
    levels = c(
      "Stably low (ref)", "Downwardly mobile", "Stably middle", 
      "Upwardly mobile", "Stably high"
    )
  )
  
  forest_data$x_pos <- as.numeric(forest_data$Label)
  forest_data$x_pos[forest_data$Gender == "Men"] <- forest_data$x_pos[forest_data$Gender == "Men"] - 0.2
  forest_data$x_pos[forest_data$Gender == "Women"] <- forest_data$x_pos[forest_data$Gender == "Women"] + 0.2
  
  ggplot(forest_data, aes(x = x_pos, y = Estimate, color = Gender)) +
    geom_point(size = 3, position = position_identity()) +
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.15, position = position_identity()) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    scale_color_manual(values = c("Men" = "#00BFC4", "Women" = "#F8766D")) +
    scale_x_continuous(
      breaks = 1:5,
      labels = levels(forest_data$Label)
    ) +
    scale_y_continuous(
      breaks = c(0, 1, 2), 
      labels = c("0", "1", "2"), 
      limits = c(-0.5, 3)
    ) +
    labs(
      title = title,
      y = "Coefficient Estimate (95% CI)",
      x = ""
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      plot.title = element_text(size = 16),
      legend.position = "none",
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      panel.grid.minor.x = element_blank(),
      panel.grid = element_blank()
    )
}

# lasi
create_forest_plot_lasi_combined <- function(model_men, model_women, title) {
  coef_table_men <- summary(model_men)$coefficients
  conf_int_men <- confint(model_men)
  mobility_terms_men <- rownames(coef_table_men)[grep("^H_mobility_status_lasi", rownames(coef_table_men))]
  
  forest_data_men <- data.frame(
    Term = mobility_terms_men,
    Estimate = coef_table_men[mobility_terms_men, "Estimate"],
    LowerCI = conf_int_men[mobility_terms_men, 1],
    UpperCI = conf_int_men[mobility_terms_men, 2],
    PValue = coef_table_men[mobility_terms_men, "Pr(>|t|)"],
    Gender = "Men"
  )
  
  coef_table_women <- summary(model_women)$coefficients
  conf_int_women <- confint(model_women)
  mobility_terms_women <- rownames(coef_table_women)[grep("^H_mobility_status_lasi", rownames(coef_table_women))]
  
  forest_data_women <- data.frame(
    Term = mobility_terms_women,
    Estimate = coef_table_women[mobility_terms_women, "Estimate"],
    LowerCI = conf_int_women[mobility_terms_women, 1],
    UpperCI = conf_int_women[mobility_terms_women, 2],
    PValue = coef_table_women[mobility_terms_women, "Pr(>|t|)"],
    Gender = "Women"
  )
  
  forest_data <- rbind(forest_data_men, forest_data_women)
  
  reference_data <- data.frame(
    Term = rep("Stably low (ref)", 2),
    Estimate = c(0, 0),
    LowerCI = c(0, 0),
    UpperCI = c(0, 0),
    PValue = c(NA, NA),
    Gender = c("Men", "Women")
  )
  
  forest_data <- rbind(reference_data, forest_data)
  
  forest_data$Label <- recode(forest_data$Term,
                              "H_mobility_status_lasiDownwardly mobile" = "Downwardly mobile",
                              "H_mobility_status_lasiStably middle" = "Stably middle",
                              "H_mobility_status_lasiUpwardly mobile" = "Upwardly mobile",
                              "H_mobility_status_lasiStably high" = "Stably high",
                              "Stably low (ref)" = "Stably low (ref)"  
  )
  
  forest_data$Label <- factor(
    forest_data$Label,
    levels = c(
      "Stably low (ref)", "Downwardly mobile", "Stably middle", 
      "Upwardly mobile", "Stably high"
    )
  )
  
  forest_data$x_pos <- as.numeric(forest_data$Label)
  forest_data$x_pos[forest_data$Gender == "Men"] <- forest_data$x_pos[forest_data$Gender == "Men"] - 0.2
  forest_data$x_pos[forest_data$Gender == "Women"] <- forest_data$x_pos[forest_data$Gender == "Women"] + 0.2
  
  ggplot(forest_data, aes(x = x_pos, y = Estimate, color = Gender)) +
    geom_point(size = 3, position = position_identity()) +
    geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.15, position = position_identity()) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    scale_color_manual(values = c("Men" = "#00BFC4", "Women" = "#F8766D")) +
    scale_x_continuous(
      breaks = 1:5,
      labels = levels(forest_data$Label)
    ) +
    scale_y_continuous(
      breaks = c(0, 1, 2), 
      labels = c("0", "1", "2"), 
      limits = c(-0.5, 3)
    ) +
    labs(
      title = title,
      y = "Coefficient Estimate (95% CI)",
      x = ""
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      plot.title = element_text(size = 16),
      legend.position = "none",
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      panel.grid.minor.x = element_blank(),
      panel.grid = element_blank()
    )
}

# HRS
hrs_fgcp_combined <- create_forest_plot_hrs_combined(model1_fgcp_hrs_men, model1_fgcp_hrs_women, " ")
print(hrs_fgcp_combined)

# elsa 
elsa_fgcp_combined <- create_forest_plot_elsa_combined(model1_fgcp_elsa_men, model1_fgcp_elsa_women, " ")

# MHAS
mhas_fgcp_combined <- create_forest_plot_mhas_combined(model1_fgcp_mhas_men, model1_fgcp_mhas_women, " ")

# CHARLS
charls_fgcp_combined <- create_forest_plot_charls_combined(model1_fgcp_charls_men, model1_fgcp_charls_women, " ")

# LASI
lasi_fgcp_combined <- create_forest_plot_lasi_combined(model1_fgcp_lasi_men, model1_fgcp_lasi_women, " ")

jpeg("D:\\R project\\Cognition\\lr_cognition_H_0219.jpg", res=350, w=9000, h=5600)
hrs_fgcp + elsa_fgcp + mhas_fgcp + charls_fgcp + lasi_fgcp +
  hrs_fgcp_combined + elsa_fgcp_combined + mhas_fgcp_combined + charls_fgcp_combined + lasi_fgcp_combined +
  plot_layout(ncol = 5, nrow = 2)
dev.off()

