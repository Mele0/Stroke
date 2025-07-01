# Stroke Rehabilitation and Return-to-Work Analysis

This project evaluates the effectiveness of an early stroke specialist vocational rehabilitation (ESSVR) programme in supporting return to work (RTW) and improving quality of life among stroke survivors in England. Using a randomized controlled trial framework, we analyze clinical, demographic, and socioeconomic data to uncover predictors of programme success and health recovery.

## 🎯 Project Objectives

- Identify demographic and clinical factors influencing successful completion of the ESSVR programme.
- Evaluate the impact of ESSVR on return-to-work rates within the first and second half-year after stroke.
- Assess whether ESSVR improves long-term health outcomes, using a composite score of mood, functionality, and quality of life.

## 📄 Study Design

The trial enrolled 1,058 individuals aged 18–69 from four English regions who experienced a stroke between May and November 2020 and were employed pre-stroke. Participants were randomized 1:1 to either:

- **ESSVR + Usual Care**  
- **Usual Care Alone**

Participants were followed for 12 months post-randomization, and multiple data points were collected: demographic information, stroke severity, work history, RTW dates, and composite health scores.

## 🧠 Methodology

Data cleaning addressed inconsistencies in return-to-work dates and status flags. Three primary analyses were conducted:

**1. Logistic Regression**  
Identified predictors of ESSVR completion. Variables included stroke severity, age, sex, region, pre-stroke work status, and hours worked.

**2. Cox Proportional Hazards Model**  
Evaluated RTW over time, splitting follow-up into 0–6 and 6–12 month intervals. Interaction terms between ESSVR and time were used to isolate delayed programme effects.

**3. Linear Regression**  
Tested whether ESSVR improved health-related quality of life at 12 months, adjusting for demographic and clinical covariates.

All analyses used complete-case records and were implemented in R (v4.2.1) with the `survival` package.

## 📊 Key Findings

### ESSVR Completion
- Participants with **permanent** (aOR = 1.88, p = 0.031) or **self-employed** (aOR = 2.52, p = 0.019) status pre-stroke were significantly more likely to complete the ESSVR programme.
- Severe stroke (aOR = 0.43, p = 0.002) and increased age reduced odds of completion.

### Return to Work
- No significant RTW benefit observed in the first 6 months (HR = 1.35, p = 0.319).
- From 6–12 months, ESSVR significantly improved RTW (HR = 2.00, p = 0.028), suggesting a delayed positive effect.
- Stroke severity, contract type, and sex also influenced RTW. Fixed-term contracts were associated with reduced RTW rates (HR = 0.62, p < 0.001).

### Health Outcomes
- No significant impact of ESSVR on composite health scores (β = 1.38, p = 0.100).
- Stroke severity and age were significant predictors of lower health outcomes.
- Severe strokes reduced scores by over 8 points on average compared to mild strokes.

## 🌍 Implications

The ESSVR programme effectively increases workforce reintegration between 6 and 12 months post-stroke, especially for individuals with stable employment histories. However, its impact on broader health recovery is limited, indicating that RTW support must be complemented by additional therapeutic strategies to enhance post-stroke quality of life.

## 🔒 Limitations

- Potential recall bias in self-reported RTW dates.
- Minor inconsistencies in RTW flagging required exclusion of affected records.
- A 12-month follow-up may be insufficient to observe full health benefits.

## 📂 Data Overview

The dataset includes the following key variables:

- `sex`, `age`, `region`
- `work_status_pre`, `hpw_pre` (hours/week)
- `stroke_severity`, `alloc` (group allocation)
- `rtw_flg`, `rtw_dte`, `exit_assessment_dte`
- `essvr_complete_flg`, `health_score`

## 📈 Visualizations

<p align="center">
  <img src="https://upload.wikimedia.org/wikipedia/commons/1/10/Mondrian_Partitioning.png" width="400"/>
  <br>
  <em>Figure: ESSVR return-to-work effectiveness over time stratified by sex.</em>
</p>

---
