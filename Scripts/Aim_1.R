rm(list = ls())
library(car)
library(dplyr)
library(tidyr)
library(ggplot2)
library(survival)
library(survminer)
library(kableExtra)

## Load dataset ##
dataset <- read.csv("/Users/mele/Documents/Term 1/Introduction to Statistical Thinking and Data Analysis/Homework/Project/stroke_rtw.csv")

## Format the dataset ##
dataset$rtw_dte <- as.Date(dataset$rtw_dte)
dataset$exit_assessment_dte <- as.Date(dataset$exit_assessment_dte)
dataset$randomisation_dte <- as.Date(dataset$randomisation_dte)

dataset$surv_time <- as.numeric(difftime(
  dataset$exit_assessment_dte, 
  dataset$randomisation_dte, 
  units = "days"
)) / 30.44

dataset$work_status_pre_Casual <- ifelse(dataset$work_status_pre == "Casual", 1, 0)
dataset$work_status_pre_Contractor <- ifelse(dataset$work_status_pre == "Contractor", 1, 0)
dataset$work_status_pre_FixedTerm <- ifelse(dataset$work_status_pre == "FixedTerm", 1, 0)
dataset$work_status_pre_Permanent <- ifelse(dataset$work_status_pre == "Permanent", 1, 0)
dataset$work_status_pre_SelfEmployed <- ifelse(dataset$work_status_pre == "SelfEmployed", 1, 0)

dataset$stroke_severity_Mild <- ifelse(dataset$stroke_severity == "Mild", 1, 0)
dataset$stroke_severity_Moderate <- ifelse(dataset$stroke_severity == "Moderate", 1, 0)
dataset$stroke_severity_Severe <- ifelse(dataset$stroke_severity == "Severe", 1, 0)

dataset$alloc <- ifelse(dataset$alloc == "ESSVR", 1, 0)
dataset$alloc <- factor(dataset$alloc, levels = c(0,1), labels = c("Usual", "ESSVR"))

###############
#### AIM 1 ####
###############

# Aim 1: Factors Contributing to the successful completion of the ESSVR program.
# Variables to use: essvr_complete_flg (complete ESSVR)
dataset$work_status_pre <- ifelse(dataset$work_status_pre == "Permanent", 1, 
                            ifelse(dataset$work_status_pre == "FixedTerm", 2, 
                            ifelse(dataset$work_status_pre == "Casual", 3, 
                            ifelse(dataset$work_status_pre == "SelfEmployed", 4, 5))))

subset_ESSR <- dataset %>% filter(!is.na(dataset$essvr_complete_flg))

contingency_table <- table(subset_ESSR$essvr_complete_flg, subset_ESSR$work_status_pre)
chi_squared_result <- chisq.test(contingency_table)

dataset$surv_time_rtw <- as.numeric(difftime(
  as.Date(dataset$rtw_dte), 
  dataset$randomisation_dte, 
  units = "days"
)) / 30.44


########################
## Aproach #1: LogReg ##
########################

# Work Status
subset_ESSR$work_status_pre <- factor(subset_ESSR$work_status_pre)
subset_ESSR$work_status_pre <- relevel(subset_ESSR$work_status_pre, ref = "Casual")
full_log <- glm(essvr_complete_flg ~ work_status_pre, family = binomial(link = "logit"), data = subset_ESSR)
summary(full_log)
plot(full_log)

# HPW
full_log <- glm(essvr_complete_flg ~ hpw_pre, family = binomial(link = "logit"), data = subset_ESSR)
summary(full_log)

full_log1 <- glm(essvr_complete_flg ~ hpw_pre + age, family = binomial(link = "logit"), data = subset_ESSR)
full_log <- glm(essvr_complete_flg ~ hpw_pre * age, family = binomial(link = "logit"), data = subset_ESSR)
anova(full_log1, full_log, test = "LRT")  # Interaction between age and hpw

full_log_stroke <- glm(essvr_complete_flg ~ stroke_severity, family = binomial(link = "logit"), data = subset_ESSR)
(exp(summary(full_log_stroke)$coefficients))
plot(full_log_stroke)

# Testing if Age interacts with stroke severity
full_log_stroke1 <- glm(essvr_complete_flg ~ stroke_severity + age, family = binomial(link = "logit"), data = subset_ESSR)
glm_interaction <- glm(essvr_complete_flg ~ stroke_severity * age, family = binomial(link = "logit"), data = subset_ESSR)
anova(full_log_stroke1, glm_interaction, test = "LRT")

# From the Residuals vs Fitted plot, we see indices of non-linearity: can be addressed by applying a transformation on the predictors.
full_log_model <- glm(essvr_complete_flg ~ factor(work_status_pre) + stroke_severity + age + region + sex, family = binomial(link = "logit"), data = subset_ESSR)
summary(full_log_model)
vif(full_log_model)  # Assess Multicollinearity

full_log <- glm(essvr_complete_flg ~ age*region, family = binomial(link = "logit"), data = subset_ESSR)
summary(full_log)

full_log_model2 <- summary(full_log_model)

or_results <- data.frame(
  Variable = rownames(full_log_model2$coefficients),
  exp_coef = exp(full_log_model2$coefficients[, "Estimate"]),  # Hazard ratio (exp(coef))
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
