# ==============================
# AIM 1 – ESSVR Completion Model
# ==============================

rm(list = ls())
library(car)
library(dplyr)
library(tidyr)
library(ggplot2)
library(survival)
library(survminer)
library(kableExtra)

# ------------------------
# Load and prepare dataset
# ------------------------
dataset <- read.csv("stroke_rtw.csv")

dataset$rtw_dte <- as.Date(dataset$rtw_dte)
dataset$exit_assessment_dte <- as.Date(dataset$exit_assessment_dte)
dataset$randomisation_dte <- as.Date(dataset$randomisation_dte)

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

# Treatment group
dataset$alloc <- factor(ifelse(dataset$alloc == "ESSVR", 1, 0), levels = c(0, 1), labels = c("Usual", "ESSVR"))

# -------------------
# Encode Work Status
# -------------------
dataset$work_status_pre <- ifelse(dataset$work_status_pre == "Permanent", 1,
                           ifelse(dataset$work_status_pre == "FixedTerm", 2,
                           ifelse(dataset$work_status_pre == "Casual", 3,
                           ifelse(dataset$work_status_pre == "SelfEmployed", 4, 5))))

# -------------------------------
# Filter to those with ESSVR data
# -------------------------------
subset_dataset <- dataset %>% filter(!is.na(essvr_complete_flg))

# -----------------------
# Chi-squared test (crude)
# -----------------------
contingency_table <- table(subset_dataset$essvr_complete_flg, subset_dataset$work_status_pre)
chi_squared_result <- chisq.test(contingency_table)

# --------------------------------------
# Add survival time to RTW (if not done)
# --------------------------------------
dataset$surv_time_rtw <- as.numeric(difftime(dataset$rtw_dte, dataset$randomisation_dte, units = "days")) / 30.44

# =============================
# Logistic Regression Approaches
# =============================

# Work Status
subset_dataset$work_status_pre <- factor(subset_dataset$work_status_pre)
subset_dataset$work_status_pre <- relevel(subset_dataset$work_status_pre, ref = "Casual")

full_log <- glm(essvr_complete_flg ~ work_status_pre, family = binomial(link = "logit"), data = subset_dataset)
summary(full_log)
plot(full_log)

# HPW
full_log <- glm(essvr_complete_flg ~ hpw_pre, family = binomial(link = "logit"), data = subset_dataset)
summary(full_log)

# HPW + Age interaction
full_log1 <- glm(essvr_complete_flg ~ hpw_pre + age, family = binomial(link = "logit"), data = subset_dataset)
full_log <- glm(essvr_complete_flg ~ hpw_pre * age, family = binomial(link = "logit"), data = subset_dataset)
anova(full_log1, full_log, test = "LRT")

# Stroke severity
full_log_stroke <- glm(essvr_complete_flg ~ stroke_severity, family = binomial(link = "logit"), data = subset_dataset)
exp(summary(full_log_stroke)$coefficients)
plot(full_log_stroke)

# Interaction: stroke severity x age
full_log_stroke1 <- glm(essvr_complete_flg ~ stroke_severity + age, family = binomial(link = "logit"), data = subset_dataset)
glm_interaction <- glm(essvr_complete_flg ~ stroke_severity * age, family = binomial(link = "logit"), data = subset_dataset)
anova(full_log_stroke1, glm_interaction, test = "LRT")

# Full multivariable model
full_log_model <- glm(essvr_complete_flg ~ factor(work_status_pre) + stroke_severity + age + region + sex, family = binomial(link = "logit"), data = subset_dataset)
summary(full_log_model)
vif(full_log_model)

# Age x Region interaction
full_log <- glm(essvr_complete_flg ~ age * region, family = binomial(link = "logit"), data = subset_dataset)
summary(full_log)

# ----------------------------
# Export OR Table for Forest Plot
# ----------------------------
full_log_model2 <- summary(full_log_model)

or_results <- data.frame(
  Variable = rownames(full_log_model2$coefficients),
  exp_coef = exp(full_log_model2$coefficients[, "Estimate"]),
  se_coef = full_log_model2$coefficients[, "Std. Error"],
  p_value = full_log_model2$coefficients[, "Pr(>|z|)"]
)

forest_data <- data.frame(
  Variable = rownames(full_log_model2$coefficients),
  HR = exp(full_log_model2$coefficients[, "Estimate"]),
  p_value = full_log_model2$coefficients[, "Pr(>|z|)"]
)

forest_data$CI_lower <- or_results$exp_coef - 1.96 * or_results$se_coef
forest_data$CI_upper <- or_results$exp_coef + 1.96 * or_results$se_coef

write.csv(forest_data, "forest_aim1.csv", row.names = FALSE)
