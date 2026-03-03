# Internal PoS App Quick Start

This app estimates **predictive probability of success (PoS)** for OS using historical OS/PFS studies and current-study PFS information.

---

## What you need

Prepare either:

* your own historical studies **CSV**

or

* use the built-in example dataset

You also need the current-study inputs:

* **Current study PFS log(HR)**
* **Current study OS SE**
* **Current study PFS SE**
* **Current study within-study correlation**

---

## Historical CSV format

Your CSV must contain these columns:

* `Study`
* `logHR_OS`
* `logHR_PFS`
* `SE_OS`
* `SE_PFS`
* `R_WITHIN`

Basic rules:

* `SE_OS` > 0
* `SE_PFS` > 0
* `R_WITHIN` must be between -1 and 1
* at least 3 historical studies are recommended

If needed, click **Download Example CSV Template** in the app and use it as a starting point.

---

## How to run

### Option 1

Use the built-in example data

1. Leave CSV upload empty
2. Keep **Use built-in example data** checked
3. Enter current-study inputs
4. Click **Run Analysis**

### Option 2

Use your own historical CSV

1. Upload your CSV
2. Enter current-study inputs
3. Click **Run Analysis**

---

## How to read the results

### Main result

Use **REML PoS** as the **primary analysis**.

### Sensitivity analysis

Use **Method of Moments (MM) PoS** as a **sensitivity analysis**.

### Key comparison

Check:

[
|PoS(REML) - PoS(MM)|
]

* if the difference is **small**, the result is less sensitive to the heterogeneity estimation method
* if the difference exceeds the warning threshold, interpret the result with more caution

### Sign convention

More negative **log(HR)** generally indicates a more favorable treatment effect.

---

## Recommended review order

1. Read the **Auto Summary**
2. Check **REML PoS**
3. Check **MM PoS**
4. Review **Absolute PoS Difference**
5. Review **Warnings**
6. Inspect the **PFS Scenario Analysis**
7. Download outputs if needed

---

## What to download

The app supports:

* **Summary Text**
  Useful for email, memo, or slides

* **Summary CSV**
  Useful for structured documentation

* **Scenario CSV**
  Useful for reviewing the full scenario curve outside the app

* **Example CSV Template**
  Useful for preparing historical input data

---

## Common interpretation guidance

### If REML and MM are close

The PoS result is less sensitive to the heterogeneity estimation method.

### If REML and MM differ materially

Interpret the PoS result more cautiously.

### If scenario curves separate in some regions

The result may become more method-sensitive for certain current-study PFS assumptions.

### If warnings appear

Review the warnings before using the result in internal decision discussions.

---

## Local run

Install packages:

```r id="tyzf41"
install.packages(c("shiny", "ggplot2", "mvmeta", "MASS"))
```

Run:

```r id="cf6yhk"
shiny::runApp()
```

---

## Internal use note

This tool is intended for **internal exploratory and planning use**.
It should support, not replace, scientific and statistical judgment.
