
# Internal Probability of Success (PoS) Tool

A statistical framework and interactive **R Shiny application** for predicting **overall survival (OS) outcomes** using **progression-free survival (PFS) evidence** and historical clinical trial data.

Author: Jinjie Chen  
Methodology: Multivariate Meta-analysis  
Language: R / Shiny

---

# Background

In oncology drug development, **overall survival (OS)** is widely considered the gold-standard endpoint. However, OS typically requires substantially longer follow-up compared with **progression-free survival (PFS)**.

In many clinical trials:

- PFS results become available earlier
- OS data remain immature
- development decisions must be made before OS maturity

Typical scenarios include:

- interim PFS analysis
- final PFS readout with immature OS
- statistically significant PFS results while OS follow-up is ongoing

This project aims to provide a **quantitative framework to translate PFS evidence into a probabilistic prediction of OS outcomes.**

---

# Methodology Overview

The statistical approach integrates:

1. **Historical trials**
2. **Current study evidence**

using a **bivariate random-effects meta-analysis model**.

Historical studies with paired OS and PFS treatment effects are used to estimate the joint distribution of the two endpoints.

From this model we derive the **predictive distribution of the OS treatment effect** for the current study conditional on the observed PFS result.

The final output is the:

**Probability of Success (PoS)**

defined as the probability that the final OS hazard ratio will meet a predefined success threshold.

---

# Mathematical Framework

Let the treatment effects be

θ = (θ_OS , θ_PFS)

Historical studies follow

θ_i ~ N(η , Σ_B)

where

η = (η_OS , η_PFS)

and

Σ_B =
[ τ_OS²   τ_OP
  τ_OP    τ_PFS² ]

represents between-study heterogeneity.

Given the observed PFS result from the current study, the predictive distribution of the OS effect can be derived analytically under the multivariate normal framework.

Probability of success is then computed as

PoS = P( logHR_OS < log(HR_target) )

---

# Shiny Application

The framework is implemented as an **interactive R Shiny application**.

Key features include:

### Historical Data Upload

Users upload historical datasets containing

- logHR_OS
- logHR_PFS
- SE_OS
- SE_PFS
- within-study correlation

### Current Study Inputs

Users specify

- current PFS logHR
- standard errors
- correlation
- OS success threshold

Optional:

- OS interim estimate

### Analysis Options

Two heterogeneity estimation methods are available

- REML
- Method of Moments

### Visualization

The application provides

- OS–PFS scatter plots
- PoS scenario curves
- diagnostic warnings

---

# Repository Structure

internal-pos-app
│
├── app.R
├── analysis_functions.R
│
├── data/
│
├── docs/
│   └── PoS_App_Project_Notes.md
│
└── README.md

---

# Potential Applications

This framework may support decision making in situations where:

- PFS evidence is available but OS data remain immature
- program continuation decisions must be made
- uncertainty around OS outcomes needs to be quantified

---

# Future Development

Planned extensions include

- integration of OS interim evidence
- additional visualization tools
- model diagnostics
- possible full Bayesian implementation

---

# License

Internal research project prototype.
