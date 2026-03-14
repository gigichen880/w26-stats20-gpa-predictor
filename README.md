# GPA Lifestyle Analyzer

An interactive R Shiny web application for exploring how student lifestyle habits (study, sleep, social activity, exercise, extracurriculars, and stress) relate to academic GPA. Users can customize, fit and compare five machine learning models, tune hyperparameters, and predict their own GPA from a custom daily schedule.


## Features

### Overview Tab
- Dataset summary metrics (observations, predictors, average GPA, GPA range)
- GPA distribution histogram with mean line
- Scatter plots of GPA vs each lifestyle variable with LOESS and linear smooths
- Predictor correlation heatmap
- Boxplot of GPA by stress level (Low / Moderate / High)

### Models Tab
Each model has its own sub-tab with a variable selector, hyperparameter controls, a **Fit** button, metric bubbles, and diagnostic plots.

| Sub-tab | Model | Metrics |
|---|---|---|
| OLS | Linear Regression | R², Adj. R², CV RMSE, AIC, F p-value, # terms |
| Decision Tree | CART Regression Tree | CV RMSE, depth, # leaves |
| Random Forest | `randomForest` ensemble | OOB RMSE, OOB R², tree count, mtry |
| Regularized OLS | Elastic Net / Ridge / Lasso via `glmnet` | Method, optimal λ, CV RMSE, # non-zero coefs |
| KNN | K-Nearest Neighbours via `caret` | Best k, CV RMSE, CV MAE |
| Compare All | 5-fold CV across all fitted models | RMSE, MAE, R² bar charts + predicted-vs-actual scatter |

All variable selectors support including **Stress Level** (encoded as integer 0/1/2). OLS additionally supports **quadratic terms** (e.g. `I(study^2)`) and **interaction terms** between numeric predictors.

### Predict Tab
- Selects from already-fitted models (unfitted ones are greyed out)
- 24-hour budget bar enforces realistic daily hour allocations across all five activity sliders
- Stress level radio buttons (Low / Moderate / High)
- OLS returns a **95% prediction interval**; other models return point estimates
- Per-prediction interpretation text based on GPA quantile thresholds

## Project Structure

```
gpa_app/
├── app.R                    # Entry point — sources globals + UI modules + server
├── globals.R                # Libraries, data load, shared constants, UI/server helpers
├── helpers.R                # load_data(), theme_gpa(), plot_corr_heatmap(), metric_box()
├── data/
│   └── student_lifestyle_dataset.csv
├── www/
│   └── styles.css           # All custom CSS
└── modules/
    ├── ui_overview.R        # tab_overview UI definition
    ├── ui_models.R          # tab_models UI definition (all model sub-tabs)
    ├── ui_predict.R         # tab_predict UI definition
    ├── server_overview.R    # Overview tab server logic
    ├── server_models.R      # Models tab server logic (all 5 models + comparison)
    └── server_predict.R     # Predict tab server logic
```

## Prerequisites

### R Packages

Install all dependencies before running:

```r
install.packages(c(
  "shiny",
  "ggplot2",
  "dplyr",
  "tidyr",
  "rpart",
  "rpart.plot",
  "caret",
  "randomForest",
  "glmnet"
))
```

Tested on R ≥ 4.2.


## Running the App

1. Clone or download this repository.
2. Open R or RStudio and set the working directory to the `gpa_app/` folder.
3. Run:

```r
shiny::runApp()
```

Or from outside the folder:

```r
shiny::runApp("path/to/gpa_app")
```

The app will open in your default browser.

## Models

### Linear Regression (OLS)
Standard `lm()` with optional quadratic (`I(x^2)`) and interaction (`x1:x2`) terms. Diagnostic plots: coefficient plot with CIs, residuals vs fitted, Normal Q-Q, Scale-Location.

### Decision Tree
`rpart()` CART regression tree. User controls max depth (1–10). Visualised with `rpart.plot`.

### Random Forest
`randomForest()` with user-controlled number of trees (50–500). Reports OOB RMSE and R² (no separate CV needed). Variable importance shown as % increase in MSE.

### Regularized Regression (Elastic Net)
`glmnet` / `cv.glmnet` with user-controlled α slider (0 = Ridge, 1 = Lasso, in-between = Elastic Net). Requires ≥ 2 predictors. Shows coefficient regularization path and CV error vs log(λ).

### KNN
`caret` KNN with centre/scale preprocessing. Searches k = 1 to `knn_k_max` (user-controlled) via 5-fold CV and selects best k by RMSE.


## Data

**Source:** [`data/student_lifestyle_dataset.csv`](https://www.kaggle.com/datasets/steve1215rogg/student-lifestyle-dataset) 

2,000 total rows.
| Column (raw) | Renamed | Type |
|---|---|---|
| `Study_Hours_Per_Day` | `study` | numeric |
| `Sleep_Hours_Per_Day` | `sleep` | numeric |
| `Social_Hours_Per_Day` | `social` | numeric |
| `Physical_Activity_Hours_Per_Day` | `exercise` | numeric |
| `Extracurricular_Hours_Per_Day` | `extra` | numeric |
| `Stress_Level` | `stress` | integer: Low=0, Moderate=1, High=2 |
| `GPA` | `GPA` | numeric (response) |

Stress is stored as an integer rather than a factor to prevent `lm()` from silently creating dummy variables and breaking coefficient interpretation in the Predict tab.
