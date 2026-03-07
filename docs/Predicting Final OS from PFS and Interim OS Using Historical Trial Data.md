# Predicting Final OS from PFS and Interim OS Using Historical Trial Data

## 1. Objective

We aim to compute the **probability of success (PoS)** for the final Overall Survival (OS) endpoint in a current clinical trial using historical surrogate relationships between **Progression-Free Survival (PFS)** and **Overall Survival (OS)**.

Two scenarios are considered:

1. **Option 1:** Current trial has **PFS readout only**
2. **Option 2:** Current trial has **PFS readout and interim OS**

The PoS is defined as:

[
PoS = P(\theta_{OS}^{final} < \theta_{success})
]

where

* (\theta_{OS}^{final}) = final OS log(HR)
* (\theta_{success}) = predefined success threshold.

---

# 2. Historical Data

We assume we have **K historical trials** with:

| Quantity     | Description                                 |
| ------------ | ------------------------------------------- |
| (y_{PFS,i})  | observed log(HR) for PFS                    |
| (y_{OS,i})   | observed log(HR) for OS                     |
| (se_{PFS,i}) | standard error of PFS                       |
| (se_{OS,i})  | standard error of OS                        |
| (r_i)        | within-study correlation between PFS and OS |

For each study:

[
y_i =
\begin{pmatrix}
y_{OS,i} \
y_{PFS,i}
\end{pmatrix}
]

Within-study covariance:

[
S_i =
\begin{pmatrix}
se_{OS,i}^2 & r_i se_{OS,i} se_{PFS,i} \
r_i se_{OS,i} se_{PFS,i} & se_{PFS,i}^2
\end{pmatrix}
]

---

# 3. Random Effects Meta-Analysis Model

We assume the following **bivariate hierarchical model**:

[
y_i \sim N(\theta_i, S_i)
]

[
\theta_i \sim N(\eta, \Sigma)
]

where:

| Parameter                        | Interpretation           |
| -------------------------------- | ------------------------ |
| (\eta = (\eta_{OS}, \eta_{PFS})) | mean treatment effects   |
| (\Sigma)                         | between-trial covariance |

---

# 4. Estimation of Historical Parameters

We estimate:

[
\hat{\eta}, \hat{\Sigma}
]

using **bivariate random-effects meta-analysis**.

Two possible estimators:

### REML (Recommended)

Restricted maximum likelihood.

Advantages:

* statistically efficient
* widely used in surrogate endpoint evaluation

### Method of Moments (MM)

Alternative moment-based estimator.

Advantages:

* computationally simple

However, REML is typically preferred because MM can produce noticeable differences when the number of trials is small.

---

# 5. Prediction of Final OS

We derive the **conditional distribution of OS given PFS**.

Let

[
h = \frac{\Sigma_{12}}{\Sigma_{22}}
]

Conditional expectation:

[
E(OS | PFS) =
\eta_{OS} + h(PFS - \eta_{PFS})
]

Conditional variance:

[
v_{cond} =
\Sigma_{11} -
\frac{\Sigma_{12}^2}{\Sigma_{22}}
]

---

# 6. Option 1: PFS Only

Observed:

* (y_{PFS})
* (se_{PFS})

Predictive mean:

[
\mu =
\eta_{OS} +
h(y_{PFS}-\eta_{PFS})
]

Predictive variance:

[
Var =
h^2 se_{PFS}^2 + v_{cond}
]

Additionally, we propagate uncertainty of (\hat{\eta}):

[
Var_{total}
===========

h^2 se_{PFS}^2
+
v_{cond}
+
a^T V_{\eta} a
]

where

[
a = (1, -h)
]

---

# 7. Option 2: PFS + Interim OS

Observed vector:

[
y =
\begin{pmatrix}
y_{OS}^{interim} \
y_{PFS}
\end{pmatrix}
]

Within-study covariance:

[
S =
\begin{pmatrix}
se_{OS,int}^2 & r_{int} se_{OS,int} se_{PFS} \
r_{int} se_{OS,int} se_{PFS} & se_{PFS}^2
\end{pmatrix}
]

Total covariance:

[
V = \Sigma + S
]

Prediction of final OS uses **multivariate normal conditioning**:

[
E(OS_{final}|y)
===============

\eta_{OS} +
\Sigma_{12}V^{-1}(y-\eta)
]

Predictive variance:

[
Var =
\Sigma_{11} -
\Sigma_{12}V^{-1}\Sigma_{21}
+
a^T V_{\eta} a
]

---

# 8. Probability of Success (PoS)

Given predictive distribution:

[
OS_{final} \sim N(\mu, Var)
]

Probability of success:

[
PoS =
P(OS_{final} < \theta_{success})
================================

\Phi
\left(
\frac{\theta_{success}-\mu}{\sqrt{Var}}
\right)
]

---

# 9. Simulation Validation

Simulation studies were conducted to validate the prediction procedure.

Example results:

| Metric   | Value   |
| -------- | ------- |
| Bias     | -0.0017 |
| RMSE     | 0.125   |
| Coverage | 0.966   |

This suggests the predictive model performs well under simulated settings.

---

# 10. Current Limitations

The current implementation **does not propagate uncertainty in the between-trial covariance matrix**:

[
\Sigma
]

Specifically:

| Source of Uncertainty   | Included |
| ----------------------- | -------- |
| OS conditional variance | ✓        |
| uncertainty of η        | ✓        |
| uncertainty of Σ        | ✗        |

Ignoring uncertainty in (\Sigma) can lead to **overconfident predictions** and **inflated PoS**.

---

# 11. Planned Improvements (Next Version)

Future development will address several methodological improvements.

---

## 11.1 Propagate Uncertainty in Σ

Implement **bootstrap over historical trials**:

1. Resample historical trials
2. Refit bivariate meta-analysis
3. Obtain ((\eta^*, \Sigma^*))
4. Recompute predictive OS
5. Average PoS across bootstrap samples

This approach naturally propagates uncertainty in:

* η
* Σ
* surrogate relationship strength

---

## 11.2 Full Bayesian Model

Alternative approach:

[
(\eta, \Sigma) \sim prior
]

Estimate posterior:

[
p(OS_{final} | data)
]

Advantages:

* coherent uncertainty propagation
* natural predictive distributions

---

## 11.3 Model Validation

Further validation should include:

* sensitivity to within-study correlation (r)
* sensitivity to historical trial heterogeneity
* robustness to small number of trials

---

## 11.4 Dynamic Updating

Allow sequential updating when new interim OS becomes available.

---

# 12. Summary

The current framework:

1. Uses **bivariate random-effects meta-analysis** on historical trials
2. Predicts final OS for the current trial using:

   * PFS only, or
   * PFS + interim OS
3. Computes **probability of success** for final OS

Future versions will improve the framework by:

* propagating uncertainty in between-trial covariance
* implementing bootstrap or Bayesian approaches
* improving robustness when historical trial numbers are small.

---
