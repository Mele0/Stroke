rm(list = ls())
library(car)
library(dplyr)
library(tidyr)
library(ggplot2)
library(survival)
library(survminer)
library(kableExtra)

## Load Data set
t <- read.csv("/Users/mele/Documents/Term 1/Introduction to Statistical Thinking and Data Analysis/Homework/Project/stroke_rtw.csv")#, row.names = "id")

t$rtw_dte <- as.Date(t$rtw_dte)
t$randomisation_dte <- as.Date(t$randomisation_dte)
t$exit_assessment_dte <- as.Date(t$exit_assessment_dte)

t$surv_time <- as.numeric(difftime(
  t$exit_assessment_dte, 
  t$randomisation_dte, 
  units = "days"
)) / 30.44

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

t$surv_time_rtw <- as.numeric(difftime(
  as.Date(t$rtw_dte), 
  t$randomisation_dte, 
  units = "days"
)) / 30.44

t$surv_time_rtw <- pmin(t$surv_time_rtw, 12)
t$work_status_pre <- factor(t$work_status_pre)
t$work_status_pre <- relevel(t$work_status_pre, ref = "Casual")
t <- t %>% mutate(rtw_flg = ifelse(rtw_flg == 0 & !is.na(rtw_dte), 1, rtw_flg))

# Initialize the `surv_time_rtw` variable based on the return-to-work date where available
t$surv_time_rtw <- ifelse(!is.na(t$rtw_dte), as.numeric(difftime(t$rtw_dte, t$randomisation_dte, units = "days")) / 30.44, NA)

# For individuals with rtw_flg == 0 and missing RTW date, calculate time from randomisation to exit_assessment_dte
# Calculate time difference for missing RTW date
t <- t %>% mutate(surv_time_rtw = ifelse(rtw_flg == 0 & is.na(rtw_dte), as.numeric(difftime(exit_assessment_dte, 
                                                                                            randomisation_dte, units = "days")) / 30.44, surv_time_rtw))

# For individuals with rtw_flg == 1 and missing RTW date, recode rtw_flg to 0 and calculate the time difference
# Calculate time difference for missing RTW date
# Recode rtw_flg to 0 for missing RTW date
t <- t %>% mutate(rtw_flg = ifelse(rtw_flg == 1 & is.na(rtw_dte), 0, rtw_flg), surv_time_rtw = ifelse(rtw_flg == 0 & is.na(rtw_dte), 
                                                                                                      as.numeric(difftime(exit_assessment_dte, randomisation_dte, units = "days")) / 30.44, surv_time_rtw))
#### Aim 3 ####
# Objective: Evaluate whether the ESSVR programme affected health and quality of life, measured in the health_score
# Idea: Linear Model
#t <- t[!(t$rtw_flg == 0 & is.na(t$rtw_dte)), ]
#t <- t[!(t$rtw_flg == 1 & is.na(t$rtw_dte)), ]
# + work_status_pre + hpw_pre + region
t$alloc <- factor(t$alloc)
t$alloc <- relevel(t$alloc, ref = "Usual Care")

fitt_lm <- lm(health_score ~ age + alloc + stroke_severity + factor(sex), data = t)
summary(fitt_lm)

fitt_lm_male <- lm(health_score ~ age + alloc + stroke_severity, data = subset(t, sex == "Male"))
summary(fitt_lm_male)

fitt_lm_female <- lm(health_score ~ age + alloc + stroke_severity, data = subset(t, sex == "Female"))
summary(fitt_lm_female)

fitt_lm_interaction <- lm(health_score ~ age * factor(sex)*stroke_severity + alloc * factor(sex) +, data = t)
summary(fitt_lm_interaction)

# Adjusted for sex: 55.01
# male intercept: 54.97
# female intercept: 49.13

# Assuming your dataset 't' contains the variables 'age', 'health_score', and 'sex'

t$fitted_values <- predict(fitt_lm)

library(ggplot2)
newdata <- with(t, 
                expand.grid(
                  age = seq(min(age), max(age)),
                  sex = c("Male", "Female"),
                  stroke_severity = "Mild",   # or another reference level
                  alloc = "Usual Care"             # likewise, a reference level
                )
)

newdata$pred <- predict(fitt_lm, newdata = newdata)

ggplot(newdata, aes(x = age, y = pred, color = sex)) +
  geom_line() +
  labs(x = "Age", y = "Predicted Health Score", color = "Sex") +
  theme_minimal()

# Plot age vs health score with separate regression lines for males and females
ggplot(t, aes(x = age, y = health_score, color = sex)) +
  geom_point(alpha = 0.6) +  # Scatter plot
  geom_vline(xintercept = 50, color = "gray", linetype = "dotted") +  # Vertical line at mean age
  geom_hline(yintercept = 50, color = "gray", linetype = "dotted") +
  geom_smooth(method = "lm", aes(fill = sex), color = c("gray"), alpha = 0.2) +  # Add regression line with CI
  geom_smooth(method = "lm", aes(group = sex), se = FALSE, linetype = "solid") +  # Regression lines
  labs(
    title = "Effect of Age on Health Score by Sex",
    x = "Age (Years)",
    y = "Health Score"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "top",  # Position the legend at the top
    legend.text = element_text(size = 12),  # Increase legend text size
    axis.ticks = element_line(color = "black", size = 1.2),  # Axis ticks size
    axis.text = element_text(size = 14, color = "black"),  # Increase axis tick labels size
    axis.title = element_text(size = 16, face = "bold", color = "black"),  # Increase axis title size
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),  # Center and increase title size
    axis.line = element_line(size = 1, color = "black"),  # Axis lines
    panel.grid = element_blank()  # Remove grid lines
  )+ xlim(30,70) + ylim(0,100) + scale_y_continuous(breaks = c(0,50,100)) + scale_x_continuous(breaks = c(30,50,70)) + scale_fill_manual(values = c("Male" = "#3c5488", "Female" = "salmon")) + scale_color_manual(values = c("Male" = "#3c5488", "Female" = "salmon"))
  

ggplot(t, aes(x = age, y = health_score, color = sex)) +
  geom_point(alpha = 0.5) +  # Scatter plot
  geom_smooth(method = "lm", aes(fill = sex), color = "#7cdbf2", alpha = 0.2) +  # Blue regression line with CI
  geom_smooth(method = "lm", aes(group = sex), se = FALSE, linetype = "solid") +  # Regression lines
  labs(
    title = "Effect of Age on Health Score by Sex",
    x = "Age",
    y = "Health Score"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank(), 
        axis.ticks = element_line(color = "black"),  # Axis ticks
        axis.line = element_line(size = 1, color = "black"), 
        panel.grid = element_blank()) + 
  xlim(31,70) + ylim(0,100) + 
  scale_x_continuous(breaks = c(31,50,70)) +
  scale_color_manual(values = c("Male" = "#00a087", "Female" = "#7cdbf2"))

ggplot(t, aes(x = age, y = health_score, color = sex)) +
  geom_point(alpha = 0.8) +  # Scatter plot
  geom_smooth(method = "lm", aes(fill = sex), color = "#3c5488", alpha = 0.2) +  # Blue regression line with CI
  geom_smooth(method = "lm", aes(group = sex), se = FALSE, linetype = "solid") +  # Regression lines
  labs(
    title = "Effect of Age on Health Score by Sex",
    x = "Age",
    y = "Health Score"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank(), 
        axis.ticks = element_line(color = "black"),  # Axis ticks
        axis.line = element_line(size = 1, color = "black"), 
        panel.grid = element_blank()) + 
  xlim(31,70) + ylim(0,100) + 
  scale_x_continuous(breaks = c(31,50,70)) +
  scale_color_manual(values = c("Male" = "#3c5488", "Female" = "#00a087"))  # Custom colors for Male and Female




ggplot(t, aes(x = age, y = health_score)) + 
  geom_jitter(aes(color = sex), width = 0.1, size = 2, alpha = 0.9) +  # Add jitter for better visibility of points
  geom_smooth(method = "lm", aes(fill = sex), color = "#7cdbf2", alpha = 0.2) +  # Add regression line with CI
  scale_color_manual(values = c("green", "black")) +  # Apply custom color for severe stroke, black for non-severe
  scale_fill_manual(values = c("gray", "black")) +  # Apply custom fill for boxplot
  labs(x = "Age", y = "Health Score") +  # Labels for the axes
  theme_minimal() +  # Minimal theme
  theme(
    text = element_text(size = 14),  # Set text size
    axis.text = element_text(size = 12),  # Axis text size
    axis.title = element_text(size = 14),  # Axis title size
    plot.margin = margin(10, 10, 10, 10),  # Add some space around the plot
    axis.ticks = element_line(color = "black"),  # Axis ticks
    axis.line = element_line(size = 1, color = "black"),
    panel.grid = element_blank(),  # Remove gridlines
    plot.title = element_text(hjust = 0, size = 16, face = "bold")  # Title formatting
  )


t_male$age_centered <- t_male$age - mean(t_male$age, na.rm = TRUE)
t_female$age_centered <- t_female$age - mean(t_female$age, na.rm = TRUE)

male_mean_health_score <- mean(t_male$health_score, na.rm = TRUE)
female_mean_health_score <- mean(t_female$health_score, na.rm = TRUE)

t_male <- subset(t, sex == "Male")
t_female <- subset(t, sex == "Female")

plot_male <- ggplot(t_male, aes(x = age, y = health_score)) + 
  geom_jitter(aes(color = "blue"), width = 0.1, size = 2, alpha = 0.9) +  # Add jitter for better visibility of points with blue color
  geom_smooth(method = "lm", aes(fill = "#569ec9"), color = "#569ec9", alpha = 0.2) +  # Add regression line with blue color and CI
  scale_color_manual(values = c("black")) +  # Set color for male
  scale_fill_manual(values = c("gray")) +  # Set fill for regression line
  labs(x = "Age", y = "Health Score", title = "Male") +  # Labels and title for the plot
  geom_vline(xintercept = mean(t_male$age, na.rm = TRUE), color = "black", linetype = "dotted") +  # Vertical line at mean age
  #geom_hline(yintercept = male_mean_health_score, color = "black", linetype = "dotted") +
  theme_minimal() +  # Minimal theme
  theme(
    text = element_text(size = 14),  # Set text size
    axis.text = element_text(size = 12),  # Axis text size
    axis.title = element_text(size = 14),  # Axis title size
    plot.margin = margin(10, 10, 10, 10),  # Add some space around the plot
    axis.ticks = element_line(color = "black"),  # Axis ticks
    axis.line = element_line(size = 1, color = "black"),  # Axis line thickness
    panel.grid = element_blank(),  # Remove gridlines
    plot.title = element_text(hjust = 0, size = 16, face = "bold")  # Title formatting
  ) + xlim(40,70) + ylim(0,75) + # Labels and title for the plot
  scale_x_continuous(breaks = c(0,50, 100))+#c(mean(t_male$age, na.rm = TRUE))) +  # Set x-ticks at 60 and 70
  scale_y_continuous(breaks = c(44, 50, 75))#breaks = c(male_mean_health_score))
plot_male

plot_female <- ggplot(t_female, aes(x = age, y = health_score)) + 
  geom_jitter(aes(color = "blue"), width = 0.1, size = 2, alpha = 0.9) +  # Add jitter for better visibility of points with blue color
  geom_smooth(method = "lm", aes(fill = "#569ec9"), color = "#569ec9", alpha = 0.2) +  # Add regression line with blue color and CI
  scale_color_manual(values = c("black")) +  # Set color for male
  scale_fill_manual(values = c("gray")) +  # Set fill for regression line
  labs(x = "Age", y = "Health Score", title = "Female") +  # Labels and title for the plot
  geom_vline(xintercept = mean(t_female$age, na.rm = TRUE)-22, color = "black", linetype = "solid", size = 1) +  # Solid vertical line at mean age
  #geom_hline(yintercept = mean_health_score, color = "black", linetype = "solid", size = 1) +  # Solid horizontal line at mean health score
  #geom_vline(xintercept = 0, color = "black", linetype = "solid", size = 1) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 1) +
  geom_vline(xintercept = mean(t_female$age, na.rm = TRUE), color = "black", linetype = "dotted") +  # Vertical line at mean age
  geom_hline(yintercept = female_mean_health_score, color = "black", linetype = "dotted") +
  theme_minimal() +  # Minimal theme
  theme(
    text = element_text(size = 14),  # Set text size
    axis.text = element_text(size = 12),  # Axis text size
    axis.title = element_text(size = 14),  # Axis title size
    plot.margin = margin(10, 10, 10, 10),  # Add some space around the plot
    panel.grid = element_blank(),  # Remove gridlines
    plot.title = element_text(hjust = 0, size = 16, face = "bold")  # Title formatting
  ) + xlim(40,70) + 
  scale_x_continuous(breaks = c(round(mean(t_female$age, na.rm = TRUE)-22,2), mean(t_female$age, na.rm = TRUE), 69)) +  # Set x-ticks at 60 and 70
  scale_y_continuous(breaks = c(female_mean_health_score))
plot_female

# Plot with one confidence interval for the model


ftttt <- lm(health_score ~ alloc + age + stroke_severity, data = t)
summary(ftttt)

fitt_men <- lm(health_score ~ alloc + age + stroke_severity, data = subset(t, sex == "Male"))
fitt_women <- lm(health_score ~ alloc + age + stroke_severity, data = subset(t, sex == "Female"))

summary(fitt_men)
summary(fitt_women)
plot(fitt_men)
tidy(fitt_men)
tidy(fitt_women)


##### Do the pdf without the plots and do it as tables in overleaf and then add the plots it with pdf editors

# Fit a linear model
model <- lm(health_score ~ alloc + age + stroke_severity + factor(sex), data = t)

par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
plot(model, 
     which = 1,  # Residuals vs Fitted
     col = "gray",
     pch = 19
     ,main = "Residuals vs Fitted")

plot(model, 
     which = 2,  # Normal Q-Q
     col = "gray",
     pch = 19,
     main = "Normal Q-Q")

plot(model, 
     which = 3,  # Scale-Location
     col = "gray",
     pch = 19,
     main = "Scale-Location")

plot(model, 
     which = 5,  # Residuals vs Leverage
     col = "gray",
     pch = 19,
     main = "Residuals vs Leverage")

par(mfrow = c(1, 1))  # Reset to default
library(ggfortify)
autoplot(model, which = 1:4, ncol = 2) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )




#####3
# Create a plot for binary stroke_severity (e.g., severe vs. non-severe) with custom color

ggplot(men, aes(x = stroke_severity, y = predicted_health_score)) + 
  geom_jitter(aes(color = stroke_severity), width = 0.1, size = 2, alpha = 0.9) +  # Add jitter for better visibility of points
  geom_boxplot(aes(fill = stroke_severity), alpha = 0.2, width = 0.4, outlier.shape = NA) +  # Add boxplot for distribution of health_score
  scale_color_manual(values = c("#7cdbf2", "black", "salmon")) +  # Apply custom color for severe stroke, black for non-severe
  scale_fill_manual(values = c("#7cdbf2", "black", "salmon")) +  # Apply custom fill for boxplot
  labs(x = "Stroke Severity", y = "Health Score") +  # Labels for the axes
  theme_minimal() +  # Minimal theme
  theme(
    text = element_text(size = 14),  # Set text size
    axis.text = element_text(size = 12),  # Axis text size
    axis.title = element_text(size = 14),  # Axis title size
    plot.margin = margin(10, 10, 10, 10),  # Add some space around the plot
    panel.grid = element_blank(),  # Remove gridlines
    plot.title = element_text(hjust = 0, size = 16, face = "bold")  # Title formatting
  )

# [Males]
# Intercept: 60 year-old males with mild stroke who received Usual Care, the predicted health score is 42.17.
# For every year over the age of 60 (60.02), the health_score is reduced by 0.2 points -> 42.17628 - 0.20388 = 41.9724
#     -> As they get older, their health & quality of life after coming back is worse
# For individuals who had a Moderate stroke, the health_score is reduced by 5.7 points, compared to Mild Stroke. -> 42.17628 - 5.74466 = 36.43162
# For individuals who had a Severe stroke, the health_score is reduced by 9.44 points, compared to Mild Stroke. -> 42.17628 - 9.44 = 32.73628

# [Females]
# Intercept: 60 year-old females with mild stroke who received Usual Care, the predicted score is 44.25.
# For females who had a Severe stroke, the health score is reduced by 6.74 points, compared to Mild Stroke. -> 44.25781 - 6.74513 = 37.51268

t$alloc <- relevel(t$alloc, ref = "Usual")
t$sex <- factor(t$sex)
t$sex <- relevel(t$sex, ref = "Female")
t$age <- t$age - mean(t$age)

final_fitt <- lm(health_score ~ alloc*factor(sex) + age*factor(sex) + stroke_severity, data = t)
summary(final_fitt)
plot(final_fitt)

#
#& t$surv_time_rtw > 6 & t$surv_time_rtw <= max(t$surv_time_rtw)))
#### Table ####
data_summary <- t %>%
  group_by(alloc) %>%
  summarise(
    Median_Age = median(age, na.rm = TRUE),
    IQR_Age = IQR(age, na.rm = TRUE),
    Median_HPW = median(hpw_pre, na.rm = TRUE),
    IQR_HPW = IQR(hpw_pre, na.rm = TRUE)
  )
sex_summary <- t %>%
  group_by(alloc, sex) %>%
  summarise(
    Count = n(),
    Percentage = n() / sum(n()) * 100
  )
region_summary <- t %>%
  group_by(alloc, region) %>%
  summarise(
    Count = n(),
    Percentage = n() / sum(n()) * 100
  )
work_status_pre_summary <- t %>%
  group_by(alloc, work_status_pre) %>%
  summarise(
    Count = n(),
    Percentage = n() / sum(n()) * 100
  )
stroke_severity_pre_summary <- t %>%
  group_by(alloc, stroke_severity) %>%
  summarise(
    Count = n(),
    Percentage = n() / sum(n()) * 100
  )
library(kableExtra)
final_table <- bind_rows(data_summary, sex_summary, region_summary, work_status_pre_summary, stroke_severity_pre_summary)
kable_output <- kable(final_table, format = "html", booktabs = TRUE) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) %>%
  column_spec(1, bold = TRUE, color = "blue") # Adjust styling as needed
save_kable(kable_output, "summary_table.html")

t$sex <- factor(t$sex)
t$alloc <- factor(t$alloc)
t$region <- factor(t$region)
t$stroke_severity <- factor(t$stroke_severity)
t$work_status_pre <- factor(t$work_status_pre)

(summary_stats <- t %>%
  group_by(sex, alloc) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = (Count / sum(Count)) * 100))

# Calculate overall counts and percentages
overall_stats <- t %>%
  group_by(sex) %>%
  summarise(Total_Count = n(), .groups = 'drop') %>%
  mutate(Total_Percentage = (Total_Count / sum(Total_Count)) * 100)

# Print the results
print(summary_stats)
print(overall_stats)

categorical_summary <- t %>%
  group_by(alloc, sex, region, work_status_pre, stroke_severity) %>%
  summarise(
    Count = n(),
    Percentage = n() / sum(n()) * 100,
    .groups = 'drop'
  ) %>%
  pivot_wider(
    names_from = alloc,
    values_from = c(Count, Percentage),
    names_sep = "_"
  )

continuous_summary <- t %>%
  group_by(alloc) %>%
  summarise(
    Median_Age = median(age, na.rm = TRUE),
    IQR_Age = IQR(age, na.rm = TRUE),
    Median_HPW = median(hpw_pre, na.rm = TRUE),
    IQR_HPW = IQR(hpw_pre, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_longer(
    -alloc,
    names_to = "variable",
    values_to = "value"
  ) %>%
  separate(variable, into = c("summary", "variable"), sep = "_") %>%
  pivot_wider(
    names_from = alloc,
    values_from = value
  )

final_summary <- bind_rows(categorical_summary, continuous_summary)

#

#### PLOTS ####

library(ggplot2)
ggplot(subset_ESSR, aes(x = age, fill = as.factor(rtw_flg))) +
  geom_histogram(binwidth = 5, position = "fill") +
  labs(title = "Return to Work by Age", x = "Age", y = "Proportion", fill = "Returned to Work")


ggplot(subset_ESSR, aes(x = hpw_pre, fill = stroke_severity)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Hours Worked by Stroke Severity",
       x = "Hours Worked Pre-Stroke", y = "Density", fill = "Stroke Severity")


summary_men <- summary(rtw_cox_men)
summary_women <- summary(rtw_cox_women)

# Prepare data frames for both men and women
forest_data_men <- data.frame(
  Variable = rownames(summary_men$coefficients),
  HR = exp(summary_men$coefficients[, "coef"]),
  Lower_95 = (summary_men$conf.int[, "lower .95"]),
  Upper_95 = (summary_men$conf.int[, "upper .95"]),
  Group = "Men"
)

forest_data_women <- data.frame(
  Variable = rownames(summary_women$coefficients),
  HR = exp(summary_women$coefficients[, "coef"]),
  Lower_95 = (summary_women$conf.int[, "lower .95"]),
  Upper_95 = (summary_women$conf.int[, "upper .95"]),
  Group = "Women"
)

# Combine the men and women data
forest_data <- rbind(forest_data_men, forest_data_women)

# Adjust the vertical positioning to separate the groups
forest_data$Variable <- paste(forest_data$Variable, forest_data$Group, sep = "_")

# Create the forest plot
ggplot(forest_data, aes(x = HR, y = reorder(Variable, HR), color = Group)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = Lower_95, xmax = Upper_95), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray") +  # Reference line at HR = 1
  theme_minimal() +
  labs(
    x = "Hazard Ratio (HR)",
    y = "Variable",
    title = "Forest Plot of Hazard Ratios by Sex"
  ) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "top"
  ) +
  scale_color_manual(values = c("Men" = "forestgreen", "Women" = "orange")) + xlim(-0.5,1.5) 



model <- coxph(Surv(surv_time_rtw, rtw_flg) ~ alloc + work_status_pre + stroke_severity + age, data = t)
baseline_hazard <- basehaz(model, centered = FALSE)  # From here we see it doesn't change >11.95
#plot(baseline_hazard$time, baseline_hazard$hazard, type = "l", 
#     xlab = "Time", ylab = "Baseline Hazard", main = "Baseline Hazard over Time")
t$surv_time_rtw <- pmin(t$surv_time_rtw, 12)
library(broom)
library(forestplot)
library(knitr)
model_summary <- tidy(model, conf.int = TRUE, conf.level = 0.95)
model_summary <- model_summary %>%
  mutate(OddsRatio = exp(estimate),
         LowerCI = exp(conf.low),
         UpperCI = exp(conf.high))

model_summary$`Forest Plot` <- paste(rep("  ", 20), collapse = " ")
model_summary$term <- ifelse(is.na(model_summary$term), 
                             model_summary$term,
                             paste0("   ", model_summary$term))
model_summary$`OR (95% CI)` <- ifelse(is.na(model_summary$std.error), "",
                                      sprintf("%.2f (%.2f to %.2f)", model_summary$OddsRatio, model_summary$LowerCI, model_summary$UpperCI))
model_summary$`p.value` <- ifelse(model_summary$p.value < 0.001,
                                  "<0.001", 
                                  format(round(model_summary$p.value, 3), nsmall = 3, scientific = FALSE))
tm <- forest_theme(base_size = 18,
                   ci_pch = 15,
                   ci_col = "#762a83",
                   ci_fill = "black",
                   sizes = 2,
                   #ci_alpha = 0.8,
                   ci_pch_size = 20,
                   ci_lty = 1,
                   ci_lwd = 1.5,
                   ci_Theight = 0.2, # Set a T end at the end of CI 
                   refline_lwd = gpar(lwd = 1, lty = "dashed", col = "grey20"),
                   vertline_lwd = 1,
                   vertline_lty = "dashed",
                   vertline_col = "grey20",
                   summary_fill = "#4575b4",
                   summary_col = "#4575b4")

p <- forest(model_summary[,c("term", "Forest Plot", "OR (95% CI)","p.value")],
            est = model_summary$OddsRatio,
            lower = model_summary$LowerCI, 
            upper = model_summary$UpperCI,
            sizes = model_summary$std.error,
            ci_column = 2,
            ref_line = 1,
            arrow_lab = c("Placebo Better", "Treatment Better"),
            xlim = c(0.4, 2),
            ticks_at = c(0.5, 1, 1.5, 2),theme = tm)
plot(p)