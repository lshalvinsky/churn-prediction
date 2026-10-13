# Churn Prediction & Behavioral Segmentation

**Contributor:** [lshalvinsky](https://github.com/lshalvinsky)

A reproducible R project that creates synthetic customer and marketing data,
discovers behavioral customer segments, predicts 90-day churn, and turns model
scores into retention targeting priorities. It is a teaching example; all
customer records are simulated.

**Analysis workbook:** [`churn_segmentation.Rmd`](churn_segmentation.Rmd).
Open it in RStudio and choose **Knit**, or render to HTML from the project root.

## Workflow

1. Generate 6,000 synthetic customer records from five hidden behavioral profiles.
2. Inspect customer, purchase, digital engagement, marketing, and service data.
3. Build recency/frequency/monetary (RFM) scores.
4. Create five behavioral segments using log-transformed, standardized activity features.
5. Profile segments by spend, campaign response, and observed future churn.
6. Compare logistic regression and random-forest models on a stratified holdout.
7. Rank customers by predicted churn risk and assess AUC, decile lift, and recall at a fixed targeting budget.
8. Export customer scores, segment profiles, and model metrics.

The simulated `true_persona` field exists only to illustrate the data generator.
It is not used to form segments or train churn models. The churn target is not
included among segmentation features or prediction inputs. Use the model for
demonstration only; validate calibration, leakage, fairness, and business value
before applying a similar workflow to real customer data.

## Requirements

- R 4.1 or later
- `ranger` for random-forest classification
- `ggplot2`, `knitr`, `rmarkdown`, and `scales` to render the workbook

Install dependencies once:

```r
install.packages(c("ranger", "ggplot2", "knitr", "rmarkdown", "scales"))
```

From PowerShell at the project root, generate data and run the analysis:

```powershell
Rscript scripts/generate_data.R
Rscript scripts/run_analysis.R
```

The analysis creates:

```text
data/raw/customers.csv
data/processed/customer_churn_scores.csv
reports/segment_profiles.csv
reports/churn_deciles.csv
reports/model_metrics.csv
reports/classification_metrics.csv
```

These generated files are excluded from Git. Both scripts and the workbook
generate deterministic sample data on first run if the raw CSV is absent.

## Render the workbook

In RStudio, open `churn_segmentation.Rmd` and choose **Knit**. Or run:

```r
rmarkdown::render("churn_segmentation.Rmd")
```

## Run tests

```powershell
Rscript tests/test_pipeline.R
```

The smoke tests exercise synthetic-data generation, RFM scoring, segmentation,
profile summaries, AUC, top-budget targeting, and decile lift. The end-to-end
model fit and prediction checks also run when `ranger` is installed.

## Project layout

```text
R/                          synthetic data, segmentation, and churn model functions
scripts/generate_data.R     generate a raw customer sample
scripts/run_analysis.R      fit segments/models and export score summaries
tests/test_pipeline.R       reproducible smoke checks
churn_segmentation.Rmd      rendered analysis workbook
```
