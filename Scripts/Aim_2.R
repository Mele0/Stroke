rm(list = ls())
library(car)
library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)
library(survival)
library(survminer)
library(kableExtra)

## Load Data set
t <- read.csv("/Users/mele/Documents/Term 1/Introduction to Statistical Thinking and Data Analysis/Homework/Project/stroke_rtw.csv")#, row.names = "id")
setwd("/Users/mele/Documents/Term 1/Introduction to Statistical Thinking and Data Analysis/Homework/Project/Plots")
## Randomisation Date ##
t$rtw_dte <- as.Date(t$rtw_dte)
t$randomisation_dte <- as.Date(t$randomisation_dte)
t$exit_assessment_dte <- as.Date(t$exit_assessment_dte)

t$surv_time <- as.numeric(difftime(t$exit_assessment_dte, t$randomisation_dte, units = "days")) / 30

t$work_status_pre_Casual <- ifelse(t$work_status_pre == "Casual", 1, 0)
t$work_status_pre_Contractor <- ifelse(t$work_status_pre == "Contractor", 1, 0)
t$work_status_pre_FixedTerm <- ifelse(t$work_status_pre == "FixedTerm", 1, 0)
t$work_status_pre_Permanent <- ifelse(t$work_status_pre == "Permanent", 1, 0)
t$work_status_pre_SelfEmployed <- ifelse(t$work_status_pre == "SelfEmployed", 1, 0)

t$stroke_severity_Mild <- ifelse(t$stroke_severity == "Mild", 1, 0)
t$stroke_severity_Moderate <- ifelse(t$stroke_severity == "Moderate", 1, 0)
t$stroke_severity_Severe <- ifelse(t$stroke_severity == "Severe", 1, 0)

t$alloc <- ifelse(t$alloc == "ESSVR", 1, 0)
t$alloc <- factor(t$alloc, levels = c(0,1), labels = c("Usual", "ESSVR"))

t$work_status_pre <- factor(t$work_status_pre)
t$work_status_pre <- relevel(t$work_status_pre, ref = "Casual")

t <- t %>% filter(!(rtw_flg == 0 & !is.na(rtw_dte)), !(rtw_flg == 1 & is.na(rtw_dte)))

#### AIM 2 #####
# Recode to 1 if RTW date exists
#t <- t %>% mutate(rtw_flg = ifelse(rtw_flg == 0 & !is.na(rtw_dte), 1, rtw_flg))

# Initialize the `surv_time_rtw` variable based on the return-to-work date where available
t$surv_time_rtw <- ifelse(!is.na(t$rtw_dte), as.numeric(difftime(t$rtw_dte, t$randomisation_dte, units = "days")) / 30.44, NA)

# For individuals with rtw_flg == 0 and missing RTW date, calculate time from randomisation to exit_assessment_dte
# Calculate time difference for missing RTW date
t <- t %>% mutate(surv_time_rtw = ifelse(rtw_flg == 0 & is.na(rtw_dte), as.numeric(difftime(exit_assessment_dte, 
                                                                        randomisation_dte, units = "days")) / 30.44, surv_time_rtw))

# For individuals with rtw_flg == 1 and missing RTW date, recode rtw_flg to 0 and calculate the time difference
# Calculate time difference for missing RTW date
# Recode rtw_flg to 0 for missing RTW date
#t <- t %>% mutate(rtw_flg = ifelse(rtw_flg == 1 & is.na(rtw_dte), 0, rtw_flg), surv_time_rtw = ifelse(rtw_flg == 0 & is.na(rtw_dte), 
#                                   as.numeric(difftime(exit_assessment_dte, randomisation_dte, units = "days")) / 30.44, surv_time_rtw))

## Do log rank between allocs

rtw_log <- glm(rtw_flg ~ alloc + work_status_pre + hpw_pre + age + stroke_severity + factor(sex), family = binomial(link = "logit"), data = t)
summary(rtw_log) ### ADD THIS TO THE RESULTS
vif(rtw_log)
exp(summary(rtw_log)$coefficients)  ## Say that based on these results, hpw_pre wasn't significant and only the significant were added to cox

(rtw_log_tidy <- tidy(
  rtw_cox,
  exponentiate = TRUE,    # Exponentiate the coefficients
  conf.int = TRUE,       # Include confidence intervals
  conf.level = 0.95      # 95% confidence intervals (default)
))

(rtw_log_tidy <- rtw_log_tidy %>%
  select(
    Term = term,
    OR = estimate,
    CI_Lower = conf.low,
    CI_Upper = conf.high,
    p_value = p.value
  ))

write.csv(rtw_log_tidy, "aim_2_forest_v4.csv", row.names = FALSE)


###

df_split <- survSplit(
  Surv(surv_time_rtw, rtw_flg) ~ .,
  data = t,
  start = "tstart",
  end = "tstop",
  event  = "rtw_flg",
  cut = 6,       # split follow-up at 6 months
  episode = "tgroup"  # new variable indicating which interval (1=0-6, 2=6+)
)

rtw_cox_men <- coxph(Surv(time = tstart, time2 = tstop, event = rtw_flg) ~ alloc + work_status_pre * strata(tgroup) + 
                       stroke_severity, data = subset(df_split, sex == "Male"))
summary(rtw_cox_men)
vif(rtw_cox_men) ## Highly colinear

rtw_cox_men <- coxph(Surv(time = surv_time_rtw, event = rtw_flg) ~ alloc + work_status_pre + stroke_severity + age, data = subset(t, sex == "Male"))
rtw_cox_women <- coxph(Surv(time = surv_time_rtw, event = rtw_flg) ~ alloc + work_status_pre + stroke_severity + age, data = subset(t, sex == "Female"))
summary(rtw_cox_men)
summary(rtw_cox_women)

plot(cox.zph(rtw_cox_women, terms = FALSE), col = "red")
print(cox.zph(rtw_cox_men, terms = FALSE))

#df_split <- survSplit(
#  Surv(surv_time_rtw, rtw_flg) ~ .,
#  data = t,
#  start = "tstart",
#  end = "tstop",
#  event  = "rtw_flg",
#  cut = c(6,12),       # split follow-up at 6 months
#  episode = "interval"  # new variable indicating which interval (1=0-6, 2=6+)
#)

#t_split <- survSplit(Surv(surv_time_rtw, rtw_flg) ~ .,
#                     data      = t,
#                     cut       = 6,        # single cutpoint at 6 months
#                     start = "tstart",
#                     end = "tstop",
#                     episode   = "interval")     # carry 'id' forward to identify subjects
#
## Now fit a Cox model including interaction terms for time interval and sex:
#rtw_cox <- coxph(Surv(tstart, tstop, rtw_flg) ~ alloc + work_status_pre + stroke_severity + age + factor(sex) * interval, data = df_split)
#summary(rtw_cox)

df_split <- survSplit(
  Surv(surv_time_rtw, rtw_flg) ~ .,
  data = t,
  start = "start",
  end = "stop",
  event  = "rtw_flg",
  cut = 6,       # split follow-up at 6 months
  episode = "time_interval"  # new variable indicating which interval (1=0-6, 2=6+)
)

df_split$alloc <- factor(df_split$alloc)
df_split$alloc <- relevel(df_split$alloc, ref = "Usual Care")
#df_split$time_interval <- factor(df_split$time_interval, levels = c("0-6", "6-12"))
#df_split$time_interval <- relevel(df_split$time_interval, ref = c("0-6"))

rtw_cox1 <- coxph(Surv(time = start, time2 = stop, rtw_flg) ~ alloc + work_status_pre + stroke_severity + age + factor(sex), data = df_split)
df_split <- survSplit(
  Surv(surv_time_rtw, rtw_flg) ~ .,
  data = t,
  start = "start",
  end = "stop",
  event  = "rtw_flg",
  cut = 6,       # split follow-up at 6 months
  episode = "time_interval"  # new variable indicating which interval (1=0-6, 2=6+)
)
df_split$age <- df_split$age - mean(df_split$age)
rtw_cox <- coxph(Surv(time = start, time2 = stop, rtw_flg) ~ alloc*factor(time_interval) + work_status_pre + stroke_severity + age + factor(sex), data = df_split)
rtw_cox <- coxph(Surv(time = start, time2 = stop, rtw_flg) ~ alloc + work_status_pre + stroke_severity + age, data = subset(df_split, sex == "Male"))
rtw_cox <- coxph(Surv(time = surv_time_rtw, rtw_flg) ~ alloc*factor(sex) + work_status_pre + stroke_severity + age, data = t)
anova(rtw_cox1, rtw_cox, test = "LRT")
summary(rtw_cox1)
summary(rtw_cox)

rtw_cox <- coxph(Surv(surv_time_rtw, rtw_flg) ~ alloc + work_status_pre + stroke_severity + age + factor(sex), data = t)
summary(rtw_cox)

## Approach #2 after class ##

# Men vs Women
rtw_cox_test <- coxph(Surv(surv_time_rtw, rtw_flg) ~ alloc + work_status_pre + stroke_severity + factor(sex), data = t)
summary(rtw_cox_test)

rtw_cox_men <- coxph(Surv(surv_time_rtw, rtw_flg) ~ alloc + work_status_pre + stroke_severity, data = subset(t, sex == "Male"))
summary(rtw_cox_men)

rtw_cox_women <- coxph(Surv(surv_time_rtw, rtw_flg) ~ alloc + work_status_pre + stroke_severity, data = subset(t, sex == "Female"))
summary(rtw_cox_women)

# First 6 months - Last 6 months
#t_0_6 <- t
#t_0_6$time_0_6 <- pmin(t_0_6$surv_time_rtw, 6)
#t_0_6$event_0_6 <- ifelse(t_0_6$rtw_flg == 1 & t_0_6$surv_time_rtw <= 6, 1, 0)

t_0_6 <- t
t_0_6 <- subset(t_0_6, surv_time_rtw > 0 & surv_time_rtw <= 6)
t_0_6$age <- t_0_6$age - mean(t_0_6$age)
rtw_cox_0_6 <- coxph(Surv(surv_time_rtw, rtw_flg) ~ alloc + work_status_pre + stroke_severity + age + sex, data = t_0_6)
summary(rtw_cox_0_6)

#t_6_12 <- subset(t_0_6, event_0_6 == 0)
#t_6_12$time_6_12 <- pmin(t_6_12$surv_time_rtw, 12) - 6
#t_6_12$event_6_12 <- ifelse(t_6_12$rtw_flg == 1 & t_6_12$surv_time_rtw <= 12, 1, 0)

t_0_6 <- t
t_6_12 <- subset(t_0_6, surv_time_rtw > 6)
t_6_12$age <- t_6_12$age - mean(t_6_12$age)
rtw_cox_6_12 <- coxph(Surv(surv_time_rtw, rtw_flg) ~ alloc + work_status_pre + stroke_severity + age + sex, data = t_6_12)
summary(rtw_cox_6_12)

cox_summary <- summary(rtw_cox_6_12)

cox_results <- data.frame(
  Variable = rownames(cox_summary$coefficients),
  #coef = cox_summary$coefficients[, "coef"],
  exp_coef = exp(cox_summary$coefficients[, "coef"]),  # Hazard ratio (exp(coef))
  se_coef = cox_summary$coefficients[, "se(coef)"],
  p_value = cox_summary$coefficients[, "Pr(>|z|)"]
)

forest_data <- data.frame(
  Variable = rownames(cox_summary$coefficients),
  HR = exp(cox_summary$coefficients[, "coef"])
  #Lower_95 = (cox_summary$conf.int[, "lower .95"]),
  #Upper_95 = (cox_summary$conf.int[, "upper .95"]),
  #p_value = cox_summary$coefficients[, "Pr(>|z|)"]
)
forest_data$CI_lower <- cox_results$exp_coef - 1.96 * cox_results$se_coef
forest_data$CI_upper <- cox_results$exp_coef + 1.96 * cox_results$se_coef
forest_data


write.csv(rtw_log_tidy, "forest_aim2_v3.csv", row.names = FALSE)


ggplot(t, aes(x = age, y = health_score)) +
  geom_point(color = "black") +  # Points in black
  geom_smooth(method = "lm", se = TRUE, color = "#569ec9", fill = "lightgray") +  # Linear regression line and shaded CI
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14),  # Axis text size
    axis.title = element_text(size = 16),  # Axis title size
    axis.ticks = element_line(color = "black"),  # Axis ticks
    axis.line = element_line(size = 1, color = "black"),  # Axis line thickness
    panel.grid.major = element_line(color = "gray", linetype = "dotted", size = 0.3),  # Major grid lines
    panel.grid.minor = element_blank(),  # No minor grid lines
    plot.title = element_text(hjust = 0.5, size = 18)  # Title positioning and size
  ) +
  labs(
    title = "Linear Regression of Weight on Age",
    x = "Age (Years)",
    y = "Health Score"
  )

your_data$predicted_health_score <- predict(model, newdata = t)
ggplot(t, aes(x = age, y = health_score)) +
  geom_point(color = "black") +  # Points in black
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightgray") +  # Linear regression line and shaded CI
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14),  # Axis text size
    axis.title = element_text(size = 16),  # Axis title size
    axis.ticks = element_line(color = "black"),  # Axis ticks
    axis.line = element_line(size = 1, color = "black"),  # Axis line thickness
    panel.grid.major = element_line(color = "gray", linetype = "dotted", size = 0.3),  # Major grid lines
    panel.grid.minor = element_blank(),  # No minor grid lines
    plot.title = element_text(hjust = 0.5, size = 18)  # Title positioning and size
  ) +
  scale_y_continuous(
    limits = c(min(t$health_score) - 10, max(t$health_score) + 10),
    breaks = c(mean(t$health_score) - 5, mean(t$health_score), mean(t$health_score) + 5)
  ) +
  scale_x_continuous(
    limits = c(min(t$age) - 10, max(t$age) + 10),
    breaks = c(mean(t$age) - 5, mean_age, mean_age + 5)
  ) +
  labs(
    title = "Linear Regression of Health Score on Age",
    x = "Age (Years)",
    y = "Health Score"
  )

library(devtools)
print(forest_model(rtw_cox_6_12, limits=log( c(.5, 50) ) ))
# Create the forest plot
# Create the forest plot with continuous color scale for p-values
ggplot(cox_results, aes(x = exp_coef, y = reorder(Variable, exp_coef))) +
  geom_point(aes(color = p_value), size = 3) +  # Plot points for hazard ratios
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.3) +  # Confidence intervals
  scale_x_continuous(trans = 'log', breaks = c(0.1, 1, 10, 100), labels = c("0.1", "1", "10", "100")) +  # Log scale for HR
  labs(x = "Hazard Ratio (exp(coef))", y = "Variable", title = "Forest Plot of Cox Model Coefficients") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "none"
  ) +
  scale_color_gradient(low = "blue", high = "red")  # Color gradient for p-values
# Customize p-value color if needed


# The ESSVR intervention substantially increases the rate of returning to work within 12 months compared with usual care.
# This effect is especially apparent after the first 6 months and is somewhat stronger among men than women.
# In other words, while ESSVR doesn’t show much additional benefit in the early post‑stroke window,
#  by 6–12 months a pronounced advantage emerges for those who received ESSVR.
# For men, the HR increases ~30% (29.68%) compared to women in the 6-12 month period.
# There are only 44 events vs 1014 in the 0-6 vs 6-12 month period.

# Extra:
t$TimePeriod <- ifelse(t$surv_time_rtw <= 6, "0_6", ifelse(t$surv_time_rtw > 6, "6_12", NA))

# Fit the Cox model including TimePeriod as a factor variable
rtw_cox_combined <- coxph(Surv(surv_time_rtw, rtw_flg) ~ alloc + work_status_pre + stroke_severity + age + sex + TimePeriod, data = t)
summary(rtw_cox_combined)


# Notes:

# Survival: People who haven't returned to work
# Events: People returning to work
# Hazard: Risk of returning to work
# Cumulative Hazard: Cumulative risk of rtw (Higher CH->Higher risk of rtw)

fittt <- survfit(Surv(surv_time_rtw, rtw_flg) ~ alloc + sex, data = t)
summary(fittt)
# From this we see how, as time goes by, the likelihood of rtw increases.
ggsurvplot(fittt, data = t, fun = "cumhaz")

p <- ggsurvplot(fittt, 
           fun = "cumhaz",  # Cumulative hazard
           data = t,        # Data frame
           pval = TRUE,     # Show p-value for comparing groups
           risk.table = TRUE, # Add risk table
           legend.title = " ",
           lwd = 1.2,
           xlab = "",
           ylab = "",
           risk.table.title = " ",
           legend = "none",
           legend.labs = c("Usual (Female)", "Usual (Male)", "ESSVR (Female)", "ESSVR (Male)"),
           #legend.labs = c("Male", "Female"), # Customize legend labels if necessary
           palette = c("#E69F00", "#56B4E9", "#009E73", "#D55E00")  # Customize colors if needed
)
p

ggsave("survival_plot_cum_haz.png", plot = p$plot, width = 10, height = 8, dpi = 300)

png("survplot_alloc_men_vs_women.png", width = 5000, height = 5000, res = 600)
print(p, newpage = FALSE)
dev.off()


png("survplot_men_vs_women.png", width = 5000, height = 5000, res = 600)
print(p, newpage = FALSE)
dev.off()

