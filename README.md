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

### ⚙️ **Aim 3 – Health and Quality of Life**

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
## 📊 Key Results Summary

<div align="center">
  <h3>Table 1: Participant Characteristics by Trial Arm</h3>
  <table border="1" cellpadding="6" cellspacing="0">
    <tr><th>Variable</th><th>Overall (N=1002)</th><th>Usual Care (N=481)</th><th>ESSVR (N=521)</th></tr>
    <tr><td><b>Age (median, IQR)</b></td><td>62 (55–65)</td><td>62 (56–66)</td><td>62 (56–67)</td></tr>
    <tr><td><b>Hours Worked/Week</b></td><td>40 (25–45)</td><td>40 (30–45)</td><td>40 (25–45)</td></tr>
    <tr><td><b>Male</b></td><td>652 (65%)</td><td>311 (65%)</td><td>341 (65%)</td></tr>
    <tr><td><b>Female</b></td><td>350 (35%)</td><td>170 (35%)</td><td>180 (35%)</td></tr>
    <tr><td><b>Stroke Severity</b></td><td></td><td></td><td></td></tr>
    <tr><td>  Mild</td><td>431 (43%)</td><td>215 (45%)</td><td>216 (41%)</td></tr>
    <tr><td>  Moderate</td><td>420 (42%)</td><td>197 (41%)</td><td>223 (43%)</td></tr>
    <tr><td>  Severe</td><td>151 (15%)</td><td>69 (14%)</td><td>82 (16%)</td></tr>
  </table>
</div>

<div align="center">
  <h3>Table 2: Logistic Regression – Predictors of ESSVR Completion</h3>
  <table border="1" cellpadding="6" cellspacing="0">
    <tr><th>Predictor</th><th>Adjusted OR</th><th>95% CI</th><th>p-value</th></tr>
    <tr><td>Permanent</td><td>1.88</td><td>1.06 – 3.33</td><td>0.031</td></tr>
    <tr><td>Self-Employed</td><td>2.52</td><td>1.18 – 5.60</td><td>0.019</td></tr>
    <tr><td>Severe Stroke</td><td>0.43</td><td>0.25 – 0.73</td><td>0.002</td></tr>
    <tr><td>Age (per year)</td><td>0.97</td><td>0.94 – 0.99</td><td>0.037</td></tr>
  </table>
</div>

<div align="center">
  <h3>Table 3: Cox Model – Return to Work</h3>
  <table border="1" cellpadding="6" cellspacing="0">
    <tr><th>Variable</th><th>HR</th><th>95% CI</th><th>p-value</th></tr>
    <tr><td>ESSVR (0–6 months)</td><td>1.35</td><td>0.74 – 2.27</td><td>0.319</td></tr>
    <tr><td>ESSVR (6–12 months)</td><td>2.00</td><td>1.10 – 3.56</td><td>0.028</td></tr>
    <tr><td>Fixed-Term Contract</td><td>0.62</td><td>0.48 – 0.79</td><td>&lt;0.001</td></tr>
    <tr><td>Severe Stroke</td><td>0.53</td><td>0.43 – 0.66</td><td>&lt;0.001</td></tr>
    <tr><td>Age (per year)</td><td>0.98</td><td>0.97 – 0.99</td><td>&lt;0.001</td></tr>
    <tr><td>Male (vs. Female)</td><td>1.26</td><td>1.09 – 1.47</td><td>0.002</td></tr>
  </table>
</div>

<div align="center">
  <h3>Table 4: Linear Model – Health Score at 12 Months</h3>
  <table border="1" cellpadding="6" cellspacing="0">
    <tr><th>Variable</th><th>Effect Estimate (β)</th><th>95% CI</th><th>p-value</th></tr>
    <tr><td>ESSVR</td><td>1.38</td><td>−0.3 to 3.1</td><td>0.100</td></tr>
    <tr><td>Moderate Stroke</td><td>−4.6</td><td>−6.4 to −2.8</td><td>&lt;0.001</td></tr>
    <tr><td>Severe Stroke</td><td>−8.3</td><td>−10.9 to −5.7</td><td>&lt;0.001</td></tr>
    <tr><td>Age</td><td>−0.16</td><td>−0.27 to −0.05</td><td>0.002</td></tr>
  </table>
</div>


---

## 📌 Conclusion

This study demonstrates that early vocational rehabilitation (ESSVR), when combined with usual care, significantly enhances return-to-work outcomes for stroke survivors—particularly in the 6 to 12 months following the stroke. Completion of the ESSVR program was more likely among individuals with stable pre-stroke employment and less likely among those with severe strokes or older age, underscoring the influence of baseline characteristics on rehabilitation success. Although ESSVR did not lead to measurable improvements in health-related quality of life within the first year, its consistent effect across regions and demographic subgroups supports its scalability. The lack of impact on broader health outcomes suggests that ESSVR alone is not sufficient to address the full spectrum of post-stroke recovery needs, and highlights the importance of integrating complementary interventions to support physical and psychosocial well-being.

---

## 📚 Citation

If using this repository or findings, please cite:

> Meléndez, A. *Effectiveness of an Early Vocational Rehabilitation Programme to Support Return to Work for Stroke Survivors*. Imperial College London, 2025.

