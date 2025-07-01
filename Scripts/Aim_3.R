# ===================================
# AIM 3 – Health Score Linear Modeling
# ===================================

rm(list = ls())
library(car)
library(dplyr)
library(tidyr)
library(ggplot2)
library(survival)
library(survminer)
library(kableExtra)

# Load data
dataset <- read.csv("stroke_rtw.csv")

# Format date columns
dataset$rtw_dte <- as.Date(dataset$rtw_dte)
dataset$randomisation_dte <- as.Date(dataset$randomisation_dte)
dataset$exit_assessment_dte <- as.Date(dataset$exit_assessment_dte)

# Survival time in months
dataset$surv_time <- as.numeric(difftime(dataset$exit_assessment_dte, dataset$randomisation_dte, units = "days")) / 30.44

# One-hot encode work status
dataset$work_status_pre_Casual <- ifelse(dataset$work_status_pre == "Casual", 1, 0)
dataset$work_status_pre_Contractor <- ifelse(dataset$work_status_pre == "Contractor", 1, 0)
dataset$work_status_pre_FixedTerm <- ifelse(dataset$work_status_pre == "FixedTerm", 1, 0)
dataset$work_status_pre_Permanent <- ifelse(dataset$work_status_pre == "Permanent", 1, 0)
dataset$work_status_pre_SelfEmployed <- ifelse(dataset$work_status_pre == "SelfEmployed", 1, 0)

# One-hot encode stroke severity
dataset$stroke_severity_Mild <- ifelse(dataset$stroke_severity == "Mild", 1, 0)
dataset$stroke_severity_Moderate <- ifelse(dataset$stroke_severity == "Moderate", 1, 0)
dataset$stroke_severity_Severe <- ifelse(dataset$stroke_severity == "Severe", 1, 0)

# Convert alloc to factor
dataset$alloc <- factor(ifelse(dataset$alloc == "ESSVR", 1, 0), levels = c(0, 1), labels = c("Usual", "ESSVR"))

# Compute survival time to RTW
dataset$surv_time_rtw <- as.numeric(difftime(dataset$rtw_dte, dataset$randomisation_dte, units = "days")) / 30.44
dataset$surv_time_rtw <- pmin(dataset$surv_time_rtw, 12)

# Relevel factors
dataset$work_status_pre <- relevel(factor(dataset$work_status_pre), ref = "Casual")
dataset$alloc <- relevel(factor(dataset$alloc), ref = "Usual Care")

# Clean RTW flag logic
dataset <- dataset %>%
  mutate(
    rtw_flg = ifelse(rtw_flg == 0 & !is.na(rtw_dte), 1, rtw_flg),
    surv_time_rtw = ifelse(rtw_flg == 0 & is.na(rtw_dte),
                           as.numeric(difftime(exit_assessment_dte, randomisation_dte, units = "days")) / 30.44,
                           surv_time_rtw),
    rtw_flg = ifelse(rtw_flg == 1 & is.na(rtw_dte), 0, rtw_flg)
  )

# -------------------------------
# Linear model: Health Score (Aim 3)
# -------------------------------
fitt_lm <- lm(health_score ~ age + alloc + stroke_severity + factor(sex), data = dataset)
summary(fitt_lm)

fitt_lm_male <- lm(health_score ~ age + alloc + stroke_severity, data = subset(dataset, sex == "Male"))
summary(fitt_lm_male)

fitt_lm_female <- lm(health_score ~ age + alloc + stroke_severity, data = subset(dataset, sex == "Female"))
summary(fitt_lm_female)

fitt_lm_interaction <- lm(health_score ~ age * factor(sex) * stroke_severity + alloc * factor(sex), data = dataset)
summary(fitt_lm_interaction)

# Predictions
dataset$fitted_values <- predict(fitt_lm)

# Plot: Predicted health score by age and sex
newdata <- with(dataset, expand.grid(
  age = seq(min(age), max(age)),
  sex = c("Male", "Female"),
  stroke_severity = "Mild",
  alloc = "Usual Care"
))
newdata$pred <- predict(fitt_lm, newdata = newdata)

ggplot(newdata, aes(x = age, y = pred, color = sex)) +
  geom_line() +
  labs(x = "Age", y = "Predicted Health Score", color = "Sex") +
  theme_minimal()

# -------------------------------
# Advanced visualizations
# -------------------------------
ggplot(dataset, aes(x = age, y = health_score, color = sex)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", aes(fill = sex), alpha = 0.2) +
  labs(title = "Effect of Age on Health Score by Sex", x = "Age", y = "Health Score") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold")
  )

# -------------------------------
# Final model with interaction terms
# -------------------------------
dataset$sex <- relevel(factor(dataset$sex), ref = "Female")
dataset$age <- dataset$age - mean(dataset$age)

final_fitt <- lm(health_score ~ alloc * sex + age * sex + stroke_severity, data = dataset)
summary(final_fitt)
plot(final_fitt)

# -------------------------------
# Summary tables
# -------------------------------
data_summary <- dataset %>%
  group_by(alloc) %>%
  summarise(
    Median_Age = median(age, na.rm = TRUE),
    IQR_Age = IQR(age, na.rm = TRUE),
    Median_HPW = median(hpw_pre, na.rm = TRUE),
    IQR_HPW = IQR(hpw_pre, na.rm = TRUE)
  )

sex_summary <- dataset %>%
  group_by(alloc, sex) %>%
  summarise(Count = n(), Percentage = n() / sum(n()) * 100)

region_summary <- dataset %>%
  group_by(alloc, region) %>%
  summarise(Count = n(), Percentage = n() / sum(n()) * 100)

# Combine and save table
final_table <- bind_rows(data_summary, sex_summary, region_summary)
kable_output <- kable(final_table, format = "html", booktabs = TRUE) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)
save_kable(kable_output, "summary_table.html")
