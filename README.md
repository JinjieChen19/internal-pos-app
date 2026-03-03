# Internal PoS App

A lightweight internal R Shiny app for estimating the **predictive probability of success (PoS)** for OS using:

* historical studies with OS/PFS treatment effect estimates
* current-study PFS information
* bivariate meta-analysis with **REML** as the primary method
* **method of moments (MM)** as a sensitivity analysis

---

## What this app does

This app estimates the probability that the current study will meet a predefined **OS success threshold**.

The workflow is:

1. Use historical studies with OS and PFS treatment effect estimates.
2. Estimate the between-study covariance structure using bivariate meta-analysis.
3. Use the current-study PFS result to derive the predictive distribution of the current-study OS effect.
4. Compute PoS under:

   * **REML** (primary analysis)
   * **Method of Moments** (sensitivity analysis)
5. Compare the two methods and quantify potential method sensitivity.

The app also provides:

* automatic text summary
* warning messages
* PFS scenario analysis
* downloadable summary and scenario outputs

---

## Main statistical interpretation

### Primary analysis

**REML** is treated as the primary analysis for between-study covariance estimation.

### Sensitivity analysis

**Method of Moments (MM)** is shown as a sensitivity analysis.

### PoS interpretation

PoS is the predictive probability that the current-study OS effect will be more favorable than the predefined success threshold.

In general:

* **more negative log(HR)** indicates a more favorable treatment effect
* a **higher PoS** indicates a higher predicted probability of success

### Method sensitivity

The app reports:

[
|PoS(REML) - PoS(MM)|
]

If this absolute difference exceeds the user-defined warning threshold, the result should be interpreted with caution because it may be sensitive to the heterogeneity estimation method.

---

## Required historical CSV format

The uploaded historical CSV must contain the following columns:

* `Study`
* `logHR_OS`
* `logHR_PFS`
* `SE_OS`
* `SE_PFS`
* `R_WITHIN`

### Column definitions

* **Study**: study identifier
* **logHR_OS**: study-level OS treatment effect estimate on the log(HR) scale
* **logHR_PFS**: study-level PFS treatment effect estimate on the log(HR) scale
* **SE_OS**: standard error of the OS effect estimate
* **SE_PFS**: standard error of the PFS effect estimate
* **R_WITHIN**: within-study correlation between the OS and PFS effect estimates

### Basic input rules

* `SE_OS` and `SE_PFS` must be **greater than 0**
* `R_WITHIN` must be between **-1 and 1**
* historical data should contain **at least 3 studies**
* uploaded files should be in **CSV** format

---

## Current-study inputs

The app requires the following current-study inputs:

* **Current study PFS log(HR)**
* **Current study OS SE**
* **Current study PFS SE**
* **Current study within-study correlation**

These values are used together with the historical studies to derive the predictive distribution for current-study OS.

---

## Main outputs

### 1. Auto Summary

A short narrative summary of the main result, including:

* REML PoS
* predicted current-study OS log(HR)
* predictive SD
* MM PoS
* whether the REML vs MM difference is material

### 2. Main Results

Displays, by method:

* PoS
* predicted current-study OS log(HR)
* predictive SD
* status

### 3. Method Comparison

Displays:

* absolute PoS difference
* REML PoS
* MM PoS
* interpretation of method sensitivity

### 4. Warnings

Possible warnings include:

* near-boundary or near-zero between-study covariance estimates
* substantial REML vs MM differences
* other analysis issues or model-fitting problems

### 5. Predictive Distribution Plot

Shows the predictive distribution for the current-study OS effect.

### 6. PFS Scenario Analysis

Shows how PoS changes as the current-study PFS log(HR) varies while other inputs remain fixed.

### 7. Between-study Covariance Matrices

Displays estimated covariance matrices under:

* REML
* Method of Moments

---

## Downloadable outputs

The app currently supports downloading:

* **Summary Text**
* **Summary CSV**
* **Scenario CSV**
* **Example CSV Template**

### Summary Text

A plain-text narrative summary suitable for:

* email
* memo
* meeting notes
* slides

### Summary CSV

A structured summary of the current run, including:

* data source
* current-study inputs
* success threshold
* REML results
* MM results
* absolute PoS difference
* material sensitivity flag

### Scenario CSV

The scenario analysis data used for the PFS scenario plots, including:

* `current_pfs_loghr`
* `pos_reml`
* `pos_mm`
* `abs_diff`

### Example CSV Template

A fake-data template showing the expected historical CSV structure.

---

## How to use the app

### Option 1: Use the built-in example data

1. Leave CSV upload empty.
2. Keep **Use built-in example data** checked.
3. Enter the current-study inputs.
4. Click **Run Analysis**.

### Option 2: Upload your own historical CSV

1. Prepare a CSV with the required columns.
2. Upload the file using **Upload historical studies CSV**.
3. Enter the current-study inputs.
4. Click **Run Analysis**.

---

## Recommended interpretation workflow

For routine internal use, the recommended order is:

1. Review the **Auto Summary**
2. Review **REML PoS** as the primary result
3. Check **MM PoS** as a sensitivity analysis
4. Examine **Absolute PoS Difference**
5. Review any **warnings**
6. Inspect the **scenario plots**
7. Download the summary and scenario outputs if needed

---

## Practical interpretation notes

### If REML and MM are close

If the absolute PoS difference is small, the result is less sensitive to the heterogeneity estimation method.

### If REML and MM differ materially

If the absolute PoS difference exceeds the warning threshold, interpret the result more cautiously.

### If between-study covariance is near zero

If the estimated between-study covariance is near zero or on the boundary, predictive uncertainty may be underestimated.

### If current-study PFS is weak or unfavorable

PoS may drop quickly, and sensitivity to model assumptions may become more visible.

---

## Limitations

This app is intended as an **internal decision-support tool** and not as a fully validated production system.

Users should keep in mind:

* results depend on the quality and relevance of the historical studies
* results depend on the assumed within-study correlations
* REML and MM may behave differently when heterogeneity is weak or near the boundary
* scenario analysis is descriptive and should support, not replace, scientific judgment

---

## Local run instructions

Install required packages:

```r
install.packages(c("shiny", "ggplot2", "mvmeta", "MASS"))
```

Run the app from the project directory:

```r
shiny::runApp()
```

---

## Suggested future enhancements

Potential next steps for future versions include:

* downloadable plot files
* richer error messages
* additional sensitivity analyses
* scenario-region highlighting
* more formal reporting output
* deployment to an internal shared environment

---

## Contact / ownership

This app is currently intended for internal exploratory and planning use.
Please contact the app owner for questions about inputs, assumptions, or interpretation.
