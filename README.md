# 🧠 Stroke Rehabilitation and Return-to-Work Analysis

This project investigates the effectiveness of an Early Stroke Specialist Vocational Rehabilitation (ESSVR) programme in improving return-to-work (RTW) rates and health outcomes among stroke survivors. Using randomized controlled trial (RCT) data, we analyze the clinical, demographic, and occupational factors that influence programme success, long-term reintegration into employment, and post-stroke quality of life.

## 📍 Background

Stroke is one of the leading causes of disability in the UK and globally. In England alone, ~60,000 people experience a stroke annually, and about 40% of these are of working age (18–69 years). Fewer than half return to work, which may lead to loss of income, identity, and psychosocial well-being.

ESSVR is a structured intervention delivered by occupational therapists. It involves:
- Individual assessment of stroke-related challenges at work
- Employer and family education
- Work preparation and skills practice
- Planning and monitoring phased return-to-work pathways

The trial compared ESSVR (plus usual care) against usual care alone in 1,058 participants over a 12-month period.

---

## 📁 Dataset Overview

Key variables include:
- **Demographics**: `sex`, `age`, `region`
- **Occupation pre-stroke**: `work_status_pre`, `hpw_pre` (hours per week)
- **Clinical**: `stroke_severity`
- **Trial arm**: `alloc` (ESSVR or usual care)
- **Outcomes**:
  - `rtw_flg` and `rtw_dte` (return-to-work status and date)
  - `health_score` (composite score at 12 months)
  - `essvr_complete_flg` (ESSVR completion for intervention group)

---

## 🧪 Objectives & Analysis Strategy

The analysis is divided into three main aims, implemented in `scripts/main_analysis.R`.

### ✅ **Aim 1 – Predictors of ESSVR Completion**

We analyzed which pre-stroke factors predict whether participants allocated to ESSVR completed the full programme. 

**Approach**:
- Logistic regression on ESSVR participants only
- Key predictors: age, stroke severity, sex, work status, hours worked

**Key Findings**:
- Permanent (aOR = 1.88) and self-employed (aOR = 2.52) participants had higher odds of completion
- Severe stroke reduced completion likelihood (aOR = 0.43)
- Age negatively correlated with completion (aOR = 0.97 per year)

### 🔁 **Aim 2 – Return to Work Outcomes**

We examined how ESSVR impacts the timing and likelihood of returning to work.

**Approach**:
- Cox proportional hazards model
- Time-split analysis: 0–6 months vs. 6–12 months post-stroke
- Stratified models by sex

**Key Findings**:
- No ESSVR benefit in first 6 months (HR = 1.35, p = 0.319)
- Significant RTW benefit between 6–12 months (HR = 2.00, p = 0.028)
- Fixed-term workers returned less (HR = 0.62), and men had higher RTW rates

### 💙 **Aim 3 – Health and Quality of Life**

We evaluated whether ESSVR improves self-reported health (composite score 0–100) at 12 months.

**Approach**:
- Linear regression
- Adjusted for age, stroke severity, region, and work status
- Stratified by sex

**Key Findings**:
- ESSVR had no statistically significant effect on health score (β = 1.38, p = 0.10)
- Moderate and severe strokes associated with significant score reductions (up to −8.3 points)
- Older age linked to lower scores, especially in men

---

## 📊 Code Summary

All analysis is performed in R (`main_analysis.R`) using:

- `glm()` for logistic and linear regressions
- `coxph()` from the `survival` package for time-to-event analysis
- `survSplit()` for time interval partitioning
- `ggsurvplot()` and `forestplot()` for visualization
- Complete-case analysis used to handle inconsistencies

---

## 📌 Conclusion

This study demonstrates that ESSVR significantly increases return-to-work success in the 6–12 month post-stroke period but has limited impact on overall health scores. Understanding the nuances of stroke severity, age, and employment context can inform more targeted rehabilitation policies.

---

## 📚 Citation

If using this repository or findings, please cite:

> Meléndez, A. *Effectiveness of an Early Vocational Rehabilitation Programme to Support Return to Work for Stroke Survivors*. Imperial College London, 2025.

---

## 📬 Contact

For questions or collaboration inquiries, contact:  
**Alex Meléndez** – `amr24@ic.ac.uk`

