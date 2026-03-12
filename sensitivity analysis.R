####sensitivity analysis####
library(dplyr)
library(data.table)
library(sjPlot)
library(patchwork)
library(haven)
df_hrs <- read.csv("D:\\R project\\Cognition\\hcap_stata_data\\stata_hcap_hrs.csv")
df_elsa <- read.csv("D:\\R project\\Cognition\\hcap_stata_data\\stata_hcap_elsa.csv")
df_mhas <- read.csv("D:\\R project\\Cognition\\hcap_stata_data\\stata_hcap_mhas.csv")
df_lasi <- read.csv("D:\\R project\\Cognition\\hcap_stata_data\\stata_hcap_lasi.csv")
df_charls <- read.csv("D:\\R project\\Cognition\\hcap_stata_data\\stata_hcap_charls.csv")

hrs_rand <- read_stata('D:\\R project\\data\\randhrs1992_2022v1.dta')
elsa <- read_stata("D:\\R project\\data\\H_ELSA_g3.dta")
mhas <- read_stata("D:\\R project\\data\\H_MHAS_c.dta")
charls <- read_stata("D:\\R project\\data\\charls\\H_CHARLS_D_Data.dta")
lasi <- read_stata("D:\\R project\\data\\H_LASI_a3.dta")

hrs_weight <- subset(hrs_rand,select = c(r13wtresp,hhidpn))
elsa_weight <- subset(elsa,select = c(r8wtresp,idauniq))
mhas_weight <- subset(mhas,select = c(r4wtresp,unhhidnp))
charls_weight <- subset(charls,select = c(r4wtresp,ID))
lasi_weight <- subset(lasi, select = c(r1wtresp, prim_key))

hrs_weight <- rename(hrs_weight, id_hrs = hhidpn)
df_hrs <- left_join(df_hrs, hrs_weight,  by = c("id_hrs"))
elsa_weight <- rename(elsa_weight, id_elsa = idauniq)
df_elsa <- left_join(df_elsa, elsa_weight,  by = c("id_elsa"))
mhas_weight <- rename(mhas_weight, unhhidnp_mhas = unhhidnp)
df_mhas <- left_join(df_mhas, mhas_weight,  by = c("unhhidnp_mhas"))
df_charls$id_charls <- as.character(df_charls$id_charls)
charls_weight$r4wtresp <- as.numeric(charls_weight$r4wtresp)
charls_weight <- rename(charls_weight, id_charls = ID)
df_charls <- left_join(df_charls, charls_weight,  by = c("id_charls"))
df_lasi$id_lasidad <- as.character(df_lasi$id_lasidad)
lasi_weight <- rename(lasi_weight, id_lasidad = prim_key)
df_lasi <- left_join(df_lasi, lasi_weight,  by = c("id_lasidad"))

df_hrs$mobility_status_hrs <- ifelse(df_hrs$mobility_status_hrs == "Stable mid" , "Stable middle", df_hrs$mobility_status_hrs)
df_elsa$mobility_status_elsa <- ifelse(df_elsa$mobility_status_elsa == "Stable mid" , "Stable middle", df_elsa$mobility_status_elsa)
df_mhas$mobility_status_mhas <- ifelse(df_mhas$mobility_status_mhas == "Stable mid" , "Stable middle", df_mhas$mobility_status_mhas)
df_lasi$mobility_status_lasi <- ifelse(df_lasi$mobility_status_lasi == "Stable mid" , "Stable middle", df_lasi$mobility_status_lasi)
df_charls$mobility_status_charls <- ifelse(df_charls$mobility_status_charls == "Stable mid" , "Stable middle", df_charls$mobility_status_charls)

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


df_hrs$mobility_status_hrs <- factor(df_hrs$mobility_status_hrs,
                                     levels = c("Stably low","Downwardly mobile", "Stably middle","Upwardly mobile","Stably high"))
df_hrs$Race <- factor(df_hrs$Race,
                      levels = c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Other"))
df_elsa$mobility_status_elsa <- factor(df_elsa$mobility_status_elsa,
                                       levels = c("Stably low","Downwardly mobile", "Stably middle","Upwardly mobile","Stably high"))
df_elsa$race <- factor(df_elsa$race,
                       levels = c("White", "Non-White"))
df_mhas$mobility_status_mhas <- factor(df_mhas$mobility_status_mhas,
                                       levels = c("Stably low","Downwardly mobile", "Stably middle","Upwardly mobile","Stably high"))
df_mhas$urbanicity_mhas <- factor(df_mhas$urbanicity_mhas,
                                  levels = c("rural", "urban"))
df_lasi$mobility_status_lasi <- factor(df_lasi$mobility_status_lasi,
                                       levels = c("Stably low","Downwardly mobile", "Stably middle","Upwardly mobile","Stably high"))
df_lasi$urbanicity_lasi <- factor(df_lasi$urbanicity_lasi,
                                  levels = c("Rural", "Urban"))
df_lasi$caste <- factor(df_lasi$caste,
                        levels = c("Scheduled caste or scheduled tribe", "Other"))

df_hrs$H_mobility_status_hrs <- factor(df_hrs$H_mobility_status_hrs,
                                       levels = c("Stably low","Downwardly mobile", "Stably middle","Upwardly mobile","Stably high"))
df_elsa$H_mobility_status_elsa <- factor(df_elsa$H_mobility_status_elsa,
                                         levels = c("Stably low","Downwardly mobile", "Stably middle","Upwardly mobile","Stably high"))
df_mhas$H_mobility_status_mhas <- factor(df_mhas$H_mobility_status_mhas,
                                         levels = c("Stably low","Downwardly mobile", "Stably middle","Upwardly mobile","Stably high"))
df_lasi$H_mobility_status_lasi <- factor(df_lasi$H_mobility_status_lasi,
                                         levels = c("Stably low","Downwardly mobile", "Stably middle","Upwardly mobile","Stably high"))
df_charls$mobility_status_charls <- factor(df_charls$mobility_status_charls,
                                           levels = c("Stably low","Downwardly mobile", "Stably middle","Upwardly mobile","Stably high"))
df_charls$H_mobility_status_charls <- factor(df_charls$H_mobility_status_charls,
                                             levels = c("Stably low","Downwardly mobile", "Stably mid","Upwardly mobile","Stably high"))
df_charls$urbanicity_charls <- factor(df_charls$urbanicity_charls,
                                      levels = c("rural", "urban"))
df_charls$hukou <- factor(df_charls$hukou,
                          levels = c("Agricultual", "Non-agricultual"))

#convert weight
df_hrs$WEIGHT <- df_hrs$r13wtresp/sum(df_hrs[, 'r13wtresp'])*3143 # n = 3143
df_elsa$r8wtresp <- ifelse(is.na(df_elsa$r8wtresp), 0, df_elsa$r8wtresp)
numeric_r8wtresp <- as.numeric(df_elsa$r8wtresp)
df_elsa$WEIGHT <- numeric_r8wtresp / sum(numeric_r8wtresp, na.rm = TRUE) * 1244
df_mhas$WEIGHT <- df_mhas$r4wtresp/sum(df_mhas[, 'r4wtresp'])*1118 # n = 1118
df_charls$r4wtresp <- ifelse(is.na(df_charls$r4wtresp), 0, df_charls$r4wtresp)
df_charls$WEIGHT <- df_charls$r4wtresp/sum(df_charls[, 'r4wtresp'])*6480 # n = 6480
df_lasi$WEIGHT <- df_lasi$r1wtresp/sum(df_lasi[, 'r1wtresp'])*2864 # n = 2864

#sampling weight
#executive plotting function in main analysis file
#fgcp
model_hrs<- lm(fgcp_hrs ~ mobility_status_hrs + age_hrs + sex_hrs + Race + urbanicity_hrs,weights =df_hrs$WEIGHT, data = df_hrs)
summary(model_hrs)
hrs_model_fgcp <- create_forest_plot_hrs(model_hrs, " ")
print(hrs_model_fgcp)

model_elsa<- lm(fgcp_elsa ~ mobility_status_elsa + sex_elsa + age_elsa  + race ,weights =df_elsa$WEIGHT, data = df_elsa)
elsa_model_fgcp <- create_forest_plot_elsa(model_elsa, " ")

model_mhas<- lm(fgcp_mhas ~ mobility_status_mhas + age_mhas + sex_mhas + urbanicity_mhas,weights =df_mhas$WEIGHT, data = df_mhas)
mhas_model_fgcp <- create_forest_plot_mhas(model_mhas, " ")

model_charls<- lm(fgcp_charls ~ mobility_status_charls + age_charls + sex_charls + hukou+ urbanicity_charls,weights =df_charls$WEIGHT, data = df_charls)
charls_model_fgcp <- create_forest_plot_charls(model_charls, " ")

model_lasi<- lm(fgcp_lasi ~ mobility_status_lasi + age_lasi + sex_lasi+ caste + urbanicity_lasi,weights =df_lasi$WEIGHT, data = df_lasi)
lasi_model_fgcp <- create_forest_plot_lasi(model_lasi, " ")

#fexf
model_hrs<- lm(fexf_hrs ~ mobility_status_hrs + age_hrs + sex_hrs + Race + urbanicity_hrs,weights =df_hrs$WEIGHT, data = df_hrs)
hrs_model_fexf <- create_forest_plot_hrs(model_hrs, " ")

model_elsa<- lm(fexf_elsa ~ mobility_status_elsa + sex_elsa + age_elsa  + race ,weights =df_elsa$WEIGHT, data = df_elsa)
elsa_model_fexf <- create_forest_plot_elsa(model_elsa, " ")

model_mhas<- lm(fexf_mhas ~ mobility_status_mhas + age_mhas + sex_mhas + urbanicity_mhas,weights =df_mhas$WEIGHT, data = df_mhas)
mhas_model_fexf <- create_forest_plot_mhas(model_mhas, " ")

model_charls<- lm(fexf_charls ~ mobility_status_charls + age_charls + sex_charls + hukou+ urbanicity_charls,weights =df_charls$WEIGHT, data = df_charls)
charls_model_fexf <- create_forest_plot_charls(model_charls, " ")

model_lasi<- lm(fexf_lasi ~ mobility_status_lasi + age_lasi + sex_lasi+ caste + urbanicity_lasi,weights =df_lasi$WEIGHT, data = df_lasi)
lasi_model_fexf <- create_forest_plot_lasi(model_lasi, " ")

model_hrs<- lm(flang_hrs ~ mobility_status_hrs + age_hrs + sex_hrs + Race + urbanicity_hrs,weights =df_hrs$WEIGHT, data = df_hrs)
hrs_model_flang <- create_forest_plot_hrs(model_hrs, " ")
print(hrs_model)

model_elsa<- lm(flang_elsa ~ mobility_status_elsa + sex_elsa + age_elsa  + race ,weights =df_elsa$WEIGHT, data = df_elsa)
elsa_model_flang <- create_forest_plot_elsa(model_elsa, " ")
print(elsa_model)

model_mhas<- lm(flang_mhas ~ mobility_status_mhas + age_mhas + sex_mhas + urbanicity_mhas,weights =df_mhas$WEIGHT, data = df_mhas)
mhas_model_flang <- create_forest_plot_mhas(model_mhas, " ")
print(mhas_model)

model_charls<- lm(flang_charls ~ mobility_status_charls + age_charls + sex_charls + hukou+ urbanicity_charls,weights =df_charls$WEIGHT, data = df_charls)
charls_model_flang <- create_forest_plot_charls(model_charls, " ")
print(charls_model)

model_lasi<- lm(flang_lasi ~ mobility_status_lasi + age_lasi + sex_lasi+ caste + urbanicity_lasi,weights =df_lasi$WEIGHT, data = df_lasi)
lasi_model_flang <- create_forest_plot_lasi(model_lasi, " ")
print(lasi_model)

model_hrs<- lm(forient_hrs ~ mobility_status_hrs + age_hrs + sex_hrs + Race + urbanicity_hrs,weights =df_hrs$WEIGHT, data = df_hrs)
hrs_model_forient <- create_forest_plot_hrs(model_hrs, " ")
print(hrs_model)

model_elsa<- lm(forient_elsa ~ mobility_status_elsa + sex_elsa + age_elsa  + race ,weights =df_elsa$WEIGHT, data = df_elsa)
elsa_model_forient <- create_forest_plot_elsa(model_elsa, " ")
print(elsa_model)

model_mhas<- lm(forient_mhas ~ mobility_status_mhas + age_mhas + sex_mhas + urbanicity_mhas,weights =df_mhas$WEIGHT, data = df_mhas)
mhas_model_forient <- create_forest_plot_mhas(model_mhas, " ")
print(mhas_model)

model_charls<- lm(forient_charls ~ mobility_status_charls + age_charls + sex_charls + hukou+ urbanicity_charls,weights =df_charls$WEIGHT, data = df_charls)
charls_model_forient <- create_forest_plot_charls(model_charls, " ")
print(charls_model)

model_lasi<- lm(forient_lasi ~ mobility_status_lasi + age_lasi + sex_lasi+ caste + urbanicity_lasi,weights =df_lasi$WEIGHT, data = df_lasi)
lasi_model_forient <- create_forest_plot_lasi(model_lasi, " ")
print(lasi_model)

model_hrs<- lm(fmem_hrs ~ mobility_status_hrs + age_hrs + sex_hrs + Race + urbanicity_hrs,weights =df_hrs$WEIGHT, data = df_hrs)
hrs_model_fmem <- create_forest_plot_hrs(model_hrs, " ")
print(hrs_model)

model_elsa<- lm(fmem_elsa ~ mobility_status_elsa + sex_elsa + age_elsa  + race ,weights =df_elsa$WEIGHT, data = df_elsa)
elsa_model_fmem <- create_forest_plot_elsa(model_elsa, " ")
print(elsa_model)

model_mhas<- lm(fmem_mhas ~ mobility_status_mhas + age_mhas + sex_mhas + urbanicity_mhas,weights =df_mhas$WEIGHT, data = df_mhas)
mhas_model_fmem <- create_forest_plot_mhas(model_mhas, " ")
print(mhas_model)

model_charls<- lm(fmem_charls ~ mobility_status_charls + age_charls + sex_charls + hukou+ urbanicity_charls,weights =df_charls$WEIGHT, data = df_charls)
charls_model_fmem <- create_forest_plot_charls(model_charls, " ")

model_lasi<- lm(fmem_lasi ~ mobility_status_lasi + age_lasi + sex_lasi+ caste + urbanicity_lasi,weights =df_lasi$WEIGHT, data = df_lasi)
lasi_model_fmem <- create_forest_plot_lasi(model_lasi, " ")

#extract plot
jpeg("D:\\R project\\Cognition\\Sfigure9.jpg",res=330,w=8800,h=9000)
hrs_model_fgcp + elsa_model_fgcp + mhas_model_fgcp + charls_model_fgcp + lasi_model_fgcp+
  hrs_model_fexf + elsa_model_fexf + mhas_model_fexf + charls_model_fexf + lasi_model_fexf+
  hrs_model_flang + elsa_model_flang + mhas_model_flang + charls_model_flang + lasi_model_flang+
  hrs_model_forient + elsa_model_forient + mhas_model_forient + charls_model_forient + lasi_model_forient+
  hrs_model_fmem + elsa_model_fmem + mhas_model_fmem + charls_model_fmem + lasi_model_fmem+plot_layout(ncol =  5)
dev.off()


####complete cases####
df_hrs_complete <- read.csv("D:\\R project\\Cognition\\hcap_stata_data\\hrs_complete.csv")
df_elsa_complete <- read.csv("D:\\R project\\Cognition\\hcap_stata_data\\elsa_complete.csv")
df_mhas_complete <- read.csv("D:\\R project\\Cognition\\hcap_stata_data\\mhas_complete.csv")
df_charls_complete <- read.csv("D:\\R project\\Cognition\\hcap_stata_data\\charls_complete.csv")
df_lasi_complete <- read.csv("D:\\R project\\Cognition\\hcap_stata_data\\lasi_complete.csv")

df_hrs_complete <- df_hrs_complete %>%
  mutate(percentile_rank_edu = rank(Education,ties.method = c("max"))/length(Education))

## percentile ranking of respondent' parent education level
df_hrs_complete <- df_hrs_complete %>%
  mutate(percentile_rank_pedu =  rank(highest_education_parent,ties.method = c("max"))/length(highest_education_parent))

quantiles_r <- quantile(df_hrs_complete$percentile_rank_edu, probs = c(0.25, 0.75))
quantiles_p <- quantile(df_hrs_complete$percentile_rank_pedu, probs = c(0.25, 0.75))

df_hrs_complete$education_quan <- cut(df_hrs_complete$percentile_rank_edu,
                                breaks = c(-Inf, quantiles_r, Inf),
                                labels = c(1, 2, 3),
                                right = TRUE)
df_hrs_complete$education_p_quan <- cut(df_hrs_complete$percentile_rank_pedu,
                                  breaks = c(-Inf, quantiles_p, Inf),
                                  labels = c(1, 2, 3),
                                  right = TRUE)
table(df_hrs_complete$education_quan)
table(df_hrs_complete$education_p_quan)

df_elsa_complete <- df_elsa_complete %>%
  mutate(percentile_rank_edu = rank(Education,ties.method = c("max"))/length(Education))

## percentile ranking of respondent' parent education level
df_elsa_complete <- df_elsa_complete %>%
  mutate(percentile_rank_pedu =  rank(highest_education_parent,ties.method = c("max"))/length(highest_education_parent))

quantiles_r <- quantile(df_elsa_complete$percentile_rank_edu, probs = c(0.25, 0.75))
quantiles_p <- quantile(df_elsa_complete$percentile_rank_pedu, probs = c(0.25, 0.75))

df_elsa_complete$education_quan <- cut(df_elsa_complete$percentile_rank_edu,
                                      breaks = c(-Inf, quantiles_r, Inf),
                                      labels = c(1, 2, 3),
                                      right = TRUE)
df_elsa_complete$education_p_quan <- cut(df_elsa_complete$percentile_rank_pedu,
                                        breaks = c(-Inf, quantiles_p, Inf),
                                        labels = c(1, 2, 3),
                                        right = TRUE)
table(df_elsa_complete$education_quan)
table(df_elsa_complete$education_p_quan)

df_mhas_complete <- df_mhas_complete %>%
  mutate(percentile_rank_edu = rank(Education,ties.method = c("max"))/length(Education))

## percentile ranking of respondent' parent education level
df_mhas_complete <- df_mhas_complete %>%
  mutate(percentile_rank_pedu =  rank(highest_education_parent,ties.method = c("max"))/length(highest_education_parent))

quantiles_r <- quantile(df_mhas_complete$percentile_rank_edu, probs = c(0.25, 0.75))
quantiles_p <- quantile(df_mhas_complete$percentile_rank_pedu, probs = c(0.25, 0.75))

df_mhas_complete$education_quan <- cut(df_mhas_complete$percentile_rank_edu,
                                       breaks = c(-Inf, quantiles_r, Inf),
                                       labels = c(1, 2, 3),
                                       right = TRUE)
df_mhas_complete$education_p_quan <- cut(df_mhas_complete$percentile_rank_pedu,
                                         breaks = c(-Inf, quantiles_p, Inf),
                                         labels = c(1, 2, 3),
                                         right = TRUE)
table(df_mhas_complete$education_quan)
table(df_mhas_complete$education_p_quan)

df_charls_complete <- df_charls_complete %>%
  mutate(percentile_rank_edu = rank(Education,ties.method = c("max"))/length(Education))

## percentile ranking of respondent' parent education level
df_charls_complete <- df_charls_complete %>%
  mutate(percentile_rank_pedu =  rank(highest_education_parent,ties.method = c("max"))/length(highest_education_parent))

quantiles_r <- quantile(df_charls_complete$percentile_rank_edu, probs = c(0.25, 0.75))
quantiles_p <- quantile(df_charls_complete$percentile_rank_pedu, probs = c(0.25, 0.75))

df_charls_complete$education_quan <- cut(df_charls_complete$percentile_rank_edu,
                                       breaks = c(-Inf, quantiles_r, Inf),
                                       labels = c(1, 2, 3),
                                       right = TRUE)
df_charls_complete$education_p_quan <- cut(df_charls_complete$percentile_rank_pedu,
                                         breaks = c(-Inf, quantiles_p, Inf),
                                         labels = c(1, 2, 3),
                                         right = TRUE)
table(df_charls_complete$education_quan)
table(df_charls_complete$education_p_quan)

df_lasi_complete <- df_lasi_complete %>%
  mutate(percentile_rank_edu = rank(Education,ties.method = c("max"))/length(Education))

## percentile ranking of respondent' parent education level
df_lasi_complete <- df_lasi_complete %>%
  mutate(percentile_rank_pedu =  rank(highest_education_parent,ties.method = c("max"))/length(highest_education_parent))

quantiles_r <- quantile(df_lasi_complete$percentile_rank_edu, probs = c(0.25, 0.75))
quantiles_p <- quantile(df_lasi_complete$percentile_rank_pedu, probs = c(0.25, 0.75))

df_lasi_complete$education_quan <- cut(df_lasi_complete$percentile_rank_edu,
                                         breaks = c(-Inf, quantiles_r, Inf),
                                         labels = c(1, 2, 3),
                                         right = TRUE)
df_lasi_complete$education_p_quan <- cut(df_lasi_complete$percentile_rank_pedu,
                                           breaks = c(-Inf, quantiles_p, Inf),
                                           labels = c(1, 2, 3),
                                           right = TRUE)
table(df_lasi_complete$education_quan)
table(df_lasi_complete$education_p_quan)

# Mobility status-hrs
df_hrs_complete$education_p_quan <- as.numeric(df_hrs_complete$education_p_quan)
df_hrs_complete$education_quan <- as.numeric(df_hrs_complete$education_quan)
df_hrs_complete$mobility_status_hrs <- NA
df_hrs_complete$mobility_status_hrs[df_hrs_complete$education_p_quan == 1 & df_hrs_complete$education_quan == 1] <- 1
df_hrs_complete$mobility_status_hrs[df_hrs_complete$education_p_quan == 2 & df_hrs_complete$education_quan == 2] <- 2
df_hrs_complete$mobility_status_hrs[df_hrs_complete$education_p_quan == 3 & df_hrs_complete$education_quan == 3] <- 3
df_hrs_complete$mobility_status_hrs <- ifelse(as.numeric(df_hrs_complete$education_p_quan) > as.numeric(df_hrs_complete$education_quan), 0, df_hrs_complete$mobility_status_hr)
df_hrs_complete$mobility_status_hrs <- ifelse(as.numeric(df_hrs_complete$education_p_quan) < as.numeric(df_hrs_complete$education_quan), 4, df_hrs_complete$mobility_status_hr)
attr(df_hrs_complete$mobility_status_hrs, "label") <- "Mobility status"
df_hrs_complete$mobility_status_hrs <- factor(df_hrs_complete$mobility_status_hrs,
                                    levels = c(0, 1, 2, 4, 3),
                                    labels = c("Downwardly mobile", "Stably low","Stably middle",
                                               "Upwardly mobile","Stably high"))
df_hrs_complete$mobility_status_hrs <- relevel(df_hrs_complete$mobility_status_hrs, ref = "Stably low")
table(df_hrs_complete$mobility_status_hrs)

# Mobility status-elsa
df_elsa_complete$education_p_quan <- as.numeric(df_elsa_complete$education_p_quan)
df_elsa_complete$education_quan <- as.numeric(df_elsa_complete$education_quan)
df_elsa_complete$mobility_status_elsa <- NA
df_elsa_complete$mobility_status_elsa[df_elsa_complete$education_p_quan == 1 & df_elsa_complete$education_quan == 1] <- 1
df_elsa_complete$mobility_status_elsa[df_elsa_complete$education_p_quan == 2 & df_elsa_complete$education_quan == 2] <- 2
df_elsa_complete$mobility_status_elsa[df_elsa_complete$education_p_quan == 3 & df_elsa_complete$education_quan == 3] <- 3
df_elsa_complete$mobility_status_elsa <- ifelse(as.numeric(df_elsa_complete$education_p_quan) > as.numeric(df_elsa_complete$education_quan), 0, df_elsa_complete$mobility_status_elsa)
df_elsa_complete$mobility_status_elsa <- ifelse(as.numeric(df_elsa_complete$education_p_quan) < as.numeric(df_elsa_complete$education_quan), 4, df_elsa_complete$mobility_status_elsa)
attr(df_elsa_complete$mobility_status_elsa, "label") <- "Mobility status"
df_elsa_complete$mobility_status_elsa <- factor(df_elsa_complete$mobility_status_elsa,
                                              levels = c(0, 1, 2, 4, 3),
                                              labels = c("Downwardly mobile", "Stably low","Stably middle",
                                                         "Upwardly mobile","Stably high"))
df_elsa_complete$mobility_status_elsa <- relevel(df_elsa_complete$mobility_status_elsa, ref = "Stably low")
table(df_elsa_complete$mobility_status_elsa)

# Mobility status-mhas
df_mhas_complete$education_p_quan <- as.numeric(df_mhas_complete$education_p_quan)
df_mhas_complete$education_quan <- as.numeric(df_mhas_complete$education_quan)
df_mhas_complete$mobility_status_mhas <- NA
df_mhas_complete$mobility_status_mhas[df_mhas_complete$education_p_quan == 1 & df_mhas_complete$education_quan == 1] <- 1
df_mhas_complete$mobility_status_mhas[df_mhas_complete$education_p_quan == 2 & df_mhas_complete$education_quan == 2] <- 2
df_mhas_complete$mobility_status_mhas[df_mhas_complete$education_p_quan == 3 & df_mhas_complete$education_quan == 3] <- 3
df_mhas_complete$mobility_status_mhas <- ifelse(as.numeric(df_mhas_complete$education_p_quan) > as.numeric(df_mhas_complete$education_quan), 0, df_mhas_complete$mobility_status_mhas)
df_mhas_complete$mobility_status_mhas <- ifelse(as.numeric(df_mhas_complete$education_p_quan) < as.numeric(df_mhas_complete$education_quan), 4, df_mhas_complete$mobility_status_mhas)
attr(df_mhas_complete$mobility_status_mhas, "label") <- "Mobility status"
df_mhas_complete$mobility_status_mhas <- factor(df_mhas_complete$mobility_status_mhas,
                                                levels = c(0, 1, 2, 4, 3),
                                                labels = c("Downwardly mobile", "Stably low","Stably middle",
                                                           "Upwardly mobile","Stably high"))
df_mhas_complete$mobility_status_mhas <- relevel(df_mhas_complete$mobility_status_mhas, ref = "Stably low")
table(df_mhas_complete$mobility_status_mhas)

# Mobility status-charls
df_charls_complete$education_p_quan <- as.numeric(df_charls_complete$education_p_quan)
df_charls_complete$education_quan <- as.numeric(df_charls_complete$education_quan)
df_charls_complete$mobility_status_charls <- NA
df_charls_complete$mobility_status_charls[df_charls_complete$education_p_quan == 1 & df_charls_complete$education_quan == 1] <- 1
df_charls_complete$mobility_status_charls[df_charls_complete$education_p_quan == 2 & df_charls_complete$education_quan == 2] <- 2
df_charls_complete$mobility_status_charls[df_charls_complete$education_p_quan == 3 & df_charls_complete$education_quan == 3] <- 3
df_charls_complete$mobility_status_charls <- ifelse(as.numeric(df_charls_complete$education_p_quan) > as.numeric(df_charls_complete$education_quan), 0, df_charls_complete$mobility_status_charls)
df_charls_complete$mobility_status_charls <- ifelse(as.numeric(df_charls_complete$education_p_quan) < as.numeric(df_charls_complete$education_quan), 4, df_charls_complete$mobility_status_charls)
attr(df_charls_complete$mobility_status_charls, "label") <- "Mobility status"
df_charls_complete$mobility_status_charls <- factor(df_charls_complete$mobility_status_charls,
                                                levels = c(0, 1, 2, 4, 3),
                                                labels = c("Downwardly mobile", "Stably low","Stably middle",
                                                           "Upwardly mobile","Stably high"))
df_charls_complete$mobility_status_charls <- relevel(df_charls_complete$mobility_status_charls, ref = "Stably low")
table(df_charls_complete$mobility_status_charls)

# Mobility status-lasi
df_lasi_complete$education_p_quan <- as.numeric(df_lasi_complete$education_p_quan)
df_lasi_complete$education_quan <- as.numeric(df_lasi_complete$education_quan)
df_lasi_complete$mobility_status_lasi <- NA
df_lasi_complete$mobility_status_lasi[df_lasi_complete$education_p_quan == 1 & df_lasi_complete$education_quan == 1] <- 1
df_lasi_complete$mobility_status_lasi[df_lasi_complete$education_p_quan == 2 & df_lasi_complete$education_quan == 2] <- 2
df_lasi_complete$mobility_status_lasi[df_lasi_complete$education_p_quan == 3 & df_lasi_complete$education_quan == 3] <- 3
df_lasi_complete$mobility_status_lasi <- ifelse(as.numeric(df_lasi_complete$education_p_quan) > as.numeric(df_lasi_complete$education_quan), 0, df_lasi_complete$mobility_status_lasi)
df_lasi_complete$mobility_status_lasi <- ifelse(as.numeric(df_lasi_complete$education_p_quan) < as.numeric(df_lasi_complete$education_quan), 4, df_lasi_complete$mobility_status_lasi)
attr(df_lasi_complete$mobility_status_lasi, "label") <- "Mobility status"
df_lasi_complete$mobility_status_lasi <- factor(df_lasi_complete$mobility_status_lasi,
                                                levels = c(0, 1, 2, 4, 3),
                                                labels = c("Downwardly mobile", "Stably low","Stably middle",
                                                           "Upwardly mobile","Stably high"))
df_lasi_complete$mobility_status_lasi <- relevel(df_lasi_complete$mobility_status_lasi, ref = "Stably low")
table(df_lasi_complete$mobility_status_lasi)

#executive plotting function in main analysis file
#fgcp
model_hrs<- lm(fgcp_hrs ~ mobility_status_hrs + age_hrs + sex_hrs + Race + urbanicity_hrs, data = df_hrs_complete)
hrs_model <- create_forest_plot_hrs(model_hrs, " ")
print(hrs_model)

model_elsa<- lm(fgcp_elsa ~ mobility_status_elsa + sex_elsa + age_elsa  + race , data = df_elsa_complete)
elsa_model <- create_forest_plot_elsa(model_elsa, " ")
print(elsa_model)

model_mhas<- lm(fgcp_mhas ~ mobility_status_mhas + age_mhas + sex_mhas + urbanicity_mhas, data = df_mhas_complete)
mhas_model <- create_forest_plot_mhas(model_mhas, " ")
print(mhas_model)

model_charls<- lm(fgcp_charls ~ mobility_status_charls + age_charls + sex_charls + hukou+ urbanicity_charls, data = df_charls_complete)
charls_model <- create_forest_plot_charls(model_charls, " ")
print(charls_model)

model_lasi<- lm(fgcp_lasi ~ mobility_status_lasi + age_lasi + sex_lasi+ caste + urbanicity_lasi, data = df_lasi_complete)
lasi_model <- create_forest_plot_lasi(model_lasi, " ")
print(lasi_model)

#fexf
model_hrs<- lm(fexf_hrs ~ mobility_status_hrs + age_hrs + sex_hrs + Race + urbanicity_hrs, data = df_hrs_complete)
hrs_model_fexf <- create_forest_plot_hrs(model_hrs, " ")
print(hrs_model)

model_elsa<- lm(fexf_elsa ~ mobility_status_elsa + sex_elsa + age_elsa  + race , data = df_elsa_complete)
elsa_model_fexf <- create_forest_plot_elsa(model_elsa, " ")
print(elsa_model)

model_mhas<- lm(fexf_mhas ~ mobility_status_mhas + age_mhas + sex_mhas + urbanicity_mhas, data = df_mhas_complete)
mhas_model_fexf <- create_forest_plot_mhas(model_mhas, " ")
print(mhas_model)

model_charls<- lm(fexf_charls ~ mobility_status_charls + age_charls + sex_charls + hukou+ urbanicity_charls, data = df_charls_complete)
charls_model_fexf <- create_forest_plot_charls(model_charls, " ")
print(charls_model)

model_lasi<- lm(fexf_lasi ~ mobility_status_lasi + age_lasi + sex_lasi+ caste + urbanicity_lasi, data = df_lasi_complete)
lasi_model_fexf <- create_forest_plot_lasi(model_lasi, " ")
print(lasi_model)

#flang
model_hrs<- lm(flang_hrs ~ mobility_status_hrs + age_hrs + sex_hrs + Race + urbanicity_hrs, data = df_hrs_complete)
hrs_model_flang <- create_forest_plot_hrs(model_hrs, " ")
print(hrs_model)

model_elsa<- lm(flang_elsa ~ mobility_status_elsa + sex_elsa + age_elsa  + race , data = df_elsa_complete)
elsa_model_flang <- create_forest_plot_elsa(model_elsa, " ")
print(elsa_model)

model_mhas<- lm(flang_mhas ~ mobility_status_mhas + age_mhas + sex_mhas + urbanicity_mhas, data = df_mhas_complete)
mhas_model_flang <- create_forest_plot_mhas(model_mhas, " ")
print(mhas_model)

model_charls<- lm(flang_charls ~ mobility_status_charls + age_charls + sex_charls + hukou+ urbanicity_charls, data = df_charls_complete)
charls_model_flang <- create_forest_plot_charls(model_charls, " ")
print(charls_model)

model_lasi<- lm(flang_lasi ~ mobility_status_lasi + age_lasi + sex_lasi+ caste + urbanicity_lasi, data = df_lasi_complete)
lasi_model_flang <- create_forest_plot_lasi(model_lasi, " ")
print(lasi_model)

#forient
model_hrs<- lm(forient_hrs ~ mobility_status_hrs + age_hrs + sex_hrs + Race + urbanicity_hrs, data = df_hrs_complete)
hrs_model_forient <- create_forest_plot_hrs(model_hrs, " ")
print(hrs_model)

model_elsa<- lm(forient_elsa ~ mobility_status_elsa + sex_elsa + age_elsa  + race , data = df_elsa_complete)
elsa_model_forient <- create_forest_plot_elsa(model_elsa, " ")
print(elsa_model)

model_mhas<- lm(forient_mhas ~ mobility_status_mhas + age_mhas + sex_mhas + urbanicity_mhas, data = df_mhas_complete)
mhas_model_forient <- create_forest_plot_mhas(model_mhas, " ")
print(mhas_model)

model_charls<- lm(forient_charls ~ mobility_status_charls + age_charls + sex_charls + hukou+ urbanicity_charls, data = df_charls_complete)
charls_model_forient <- create_forest_plot_charls(model_charls, " ")
print(charls_model)

model_lasi<- lm(forient_lasi ~ mobility_status_lasi + age_lasi + sex_lasi+ caste + urbanicity_lasi, data = df_lasi_complete)
lasi_model_forient <- create_forest_plot_lasi(model_lasi, " ")
print(lasi_model)

#fmem
model_hrs<- lm(fmem_hrs ~ mobility_status_hrs + age_hrs + sex_hrs + Race + urbanicity_hrs, data = df_hrs_complete)
hrs_model_fmem <- create_forest_plot_hrs(model_hrs, " ")
print(hrs_model)

model_elsa<- lm(fmem_elsa ~ mobility_status_elsa + sex_elsa + age_elsa  + race , data = df_elsa_complete)
elsa_model_fmem <- create_forest_plot_elsa(model_elsa, " ")
print(elsa_model)

model_mhas<- lm(fmem_mhas ~ mobility_status_mhas + age_mhas + sex_mhas + urbanicity_mhas, data = df_mhas_complete)
mhas_model_fmem <- create_forest_plot_mhas(model_mhas, " ")
print(mhas_model)

model_charls<- lm(fmem_charls ~ mobility_status_charls + age_charls + sex_charls + hukou+ urbanicity_charls, data = df_charls_complete)
charls_model_fmem <- create_forest_plot_charls(model_charls, " ")
print(charls_model)

model_lasi<- lm(fmem_lasi ~ mobility_status_lasi + age_lasi + sex_lasi+ caste + urbanicity_lasi, data = df_lasi_complete)
lasi_model_fmem <- create_forest_plot_lasi(model_lasi, " ")
print(lasi_model)

#extract plot
jpeg("D:\\R project\\Cognition\\Sfigure11.jpg",res=330,w=8800,h=9000)
hrs_model + elsa_model + mhas_model + charls_model + lasi_model+
  hrs_model_fexf + elsa_model_fexf + mhas_model_fexf + charls_model_fexf + lasi_model_fexf+
  hrs_model_flang + elsa_model_flang + mhas_model_flang + charls_model_flang + lasi_model_flang+
  hrs_model_forient + elsa_model_forient + mhas_model_forient + charls_model_forient + lasi_model_forient+
  hrs_model_fmem + elsa_model_fmem + mhas_model_fmem + charls_model_fmem + lasi_model_fmem+
  plot_layout(ncol =  5)
dev.off()


