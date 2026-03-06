
# PoS App Methodology and Development Notes

Project: Internal Probability of Success (PoS) Tool for Predicting OS Outcomes Using PFS Evidence  
Author: Jinjie Chen  
Tool: R Shiny Application  
Method: Multivariate Meta-analysis

---

## 1. Project Motivation

In oncology drug development, overall survival (OS) is widely regarded as the gold‑standard endpoint. However, OS typically requires substantially longer follow‑up than progression‑free survival (PFS).

In many trials:

- PFS readout becomes available earlier
- OS data remain immature
- development decisions must be made before OS maturity

Typical situations include:

- interim PFS analysis
- final PFS readout with immature OS
- significant PFS result but ongoing OS follow‑up

Therefore, a quantitative framework is needed to translate PFS evidence into a probabilistic prediction of OS outcomes.

---

## 2. Project Objective

Develop a statistical framework and interactive tool to estimate:

**Probability of Success (PoS) for Overall Survival**

The tool integrates:

- historical trial evidence
- current study results

to derive the predictive distribution of the OS treatment effect.

---

## 3. Statistical Framework

The method uses a **bivariate random‑effects meta‑analysis model** to learn the joint distribution of treatment effects on OS and PFS.

---

## 4. Historical Trial Model

For historical study i:

Observed treatment effects

y_i = (logHR_OS_i, logHR_PFS_i)

Sampling model

y_i | θ_i ~ N(θ_i , S_i)

Within‑study covariance

S_i =
[ SE_OS_i²            ρ_i SE_OS_i SE_PFS_i  
  ρ_i SE_OS_i SE_PFS_i      SE_PFS_i² ]

---

## 5. Between‑Study Model

True treatment effects follow

θ_i ~ N(η , Σ_B)

Mean treatment effects

η = (η_OS, η_PFS)

Between‑study covariance

Σ_B =
[ τ_OS²   τ_OP  
  τ_OP    τ_PFS² ]

Interpretation

- τ_OS² : heterogeneity in OS effect  
- τ_PFS² : heterogeneity in PFS effect  
- τ_OP : correlation between OS and PFS across trials  

Parameters are estimated using **multivariate meta‑analysis** (mvmeta in R).

---

## 6. Predicting OS from PFS

For current trial c:

Observed PFS estimate

logHR_PFS_c

Conditional expectation

E(θ_OS_c | θ_PFS_c) =
η_OS + (Σ_OP / Σ_PP) (θ_PFS_c − η_PFS)

Variance

Var(θ_OS_c | θ_PFS_c) =
Σ_OO − Σ_OP² / Σ_PP

Predictive distribution

θ_OS_c ~ N( μ_pred , σ_pred² )

---

## 7. Incorporating OS Interim Evidence

If interim OS data are available:

Observed data vector

y = (logHR_OS_interim, logHR_PFS_c)

Prior

θ_c ~ N(η , Σ_B)

Sampling covariance

S =
[ SE_OS_int²      ρ SE_OS_int SE_PFS  
  ρ SE_OS_int SE_PFS    SE_PFS² ]

Posterior distribution

θ_c | y ~ N(m , V)

Posterior covariance

V = ( Σ_B⁻¹ + S⁻¹ )⁻¹

Posterior mean

m = V ( Σ_B⁻¹ η + S⁻¹ y )

OS predictive distribution

θ_OS_c | y ~ N(m1 , V11)

---

## 8. Probability of Success

Define success threshold

HR_target

Log scale

ℓ_target = log(HR_target)

Probability of success

PoS = Φ( (ℓ_target − μ_pred) / σ_pred )

Interpretation

Probability that final OS hazard ratio will meet the predefined success threshold.

---

## 9. Shiny Application

An interactive **R Shiny application** was developed to operationalize the framework.

Core features include:

### Historical Data Upload

Users upload datasets containing:

- logHR_OS  
- logHR_PFS  
- SE_OS  
- SE_PFS  
- within‑study correlation  

### Current Trial Inputs

Users specify:

- current PFS logHR  
- SE_PFS  
- SE_OS  
- correlation  
- OS success threshold  

Optional:

- OS interim estimate

### Estimation Methods

Two heterogeneity estimation methods are supported:

Primary analysis: REML  
Sensitivity analysis: Method of Moments

### Scenario Analysis

Users can explore PoS across a range of hypothetical PFS outcomes.

### Visualization

The application provides:

- PoS scenario curves
- historical OS–PFS scatter plots
- diagnostic warnings when method sensitivity is large

---

## 10. Development Progress

Completed components

- statistical engine
- Shiny UI
- CSV upload
- scenario analysis
- summary export
- GitHub version control

Repository: internal-pos-app

---

## 11. Planned Extensions

Short‑term

- integration of OS interim evidence
- improved visualization
- additional diagnostics

Medium‑term

- predictive distribution plots
- model assumption diagnostics
- UI improvements

Long‑term

- integration into internal decision workflows
- extension to additional endpoints
- potential Bayesian extension

---

## 12. Internal Forum Abstract

Proposed title

From PFS Readout to OS Probability of Success:
A Multivariate Meta‑analytic Framework and Interactive Decision Tool

Key message

The framework translates PFS evidence into probabilistic predictions for OS outcomes.

---

## 13. Key Takeaway

The project provides a transparent statistical framework and interactive tool to support decision making when OS data are immature but PFS results are available.
