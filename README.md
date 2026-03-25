# DataPilot: EDA & Preprocessing Web Application

A no-code interactive data science web application built with R Shiny. Upload any tabular dataset, clean and preprocess it, engineer new features, and explore distributions — all through a point-and-click interface.

🔗 **Live App:**  
📄 **Project Report:** See `report/DataPilot_Report.docx`

---

## Table of Contents

- [Overview](#overview)
- [Features](#features)
- [Requirements](#requirements)
- [Installation](#installation)
- [How to Run](#how-to-run)
- [How to Use the App](#how-to-use-the-app)
- [Project Structure](#project-structure)
- [Team Contributions](#team-contributions)

---

## Overview

DataPilot is a fully interactive, no-code data preprocessing and exploratory data analysis tool. It is designed for users who need to explore and clean datasets quickly without writing code. The application supports uploading data in four formats, applying a full preprocessing pipeline, engineering new features, and visualising the results through seven interactive chart types.

The app is built on the `shinydashboard` framework with a persistent left-side navigation menu. All controls are collapsible in the sidebar and update the main panel in real time via Shiny's reactive programming model.

---

## Features

### 1. Data Loading
- Upload your own dataset in **CSV**, **Excel (.xlsx)**, **JSON**, or **RDS** format
- Uploaded files always take priority over the built-in dataset selector
- Two built-in demo datasets: **iris** and **mtcars**
- Dynamic sidebar info strip shows current row count, column count, and missing value total

### 2. Data Cleaning & Preprocessing
All cleaning steps are checkbox-gated — expand only what you need:

| Step | Options |
|------|---------|
| **Missing Values** | Row removal, mean imputation, median imputation, mode imputation, custom fill value |
| **Duplicates** | Keep first, keep last, remove all — with column-level duplicate key definition |
| **Outliers (IQR)** | Identify only, remove rows, or Winsorize (cap at IQR bounds) |
| **Categorical Encoding** | One-hot encoding (with optional first-dummy drop) or label encoding |
| **Scaling** | Z-score standardisation or Min-Max normalisation (0–1) |

### 3. Feature Engineering
Create new columns with five transformation methods:
- **Log transform** — `log1p(x)`, handles non-positive values with NA
- **Square root** — `sqrt(x)`, handles negative values with NA
- **Square** — `x²`
- **Interaction term** — `x₁ × x₂` for two selected numeric columns
- **Equal-width binning** — `cut(x, n)` with user-defined bin count (2–20)

A **real-time preview card** in the sidebar displays the new column's mean, SD, and range immediately after creation. The **Transformation Comparison** tab shows before/after statistics and overlapping histograms side by side.

### 4. Exploratory Data Analysis
Seven interactive chart types, all rendered with **plotly** (hover, zoom, pan, export):

| Chart | Description |
|-------|-------------|
| Histogram | Adjustable bin count (5–100), optional colour grouping |
| Density Plot | Kernel density estimate with optional grouping |
| Scatter Plot | Any two numeric variables, optional colour grouping |
| Grouped Boxplot | Numeric distribution by categorical group |
| Violin Plot | Violin + inner boxplot overlay |
| Bar Chart | Ranked categorical frequency counts |
| Correlation Heatmap | Pairwise Pearson coefficients with value labels |

### 5. Filter & Export
- **Row filter**: filter by any column value; live badge shows rows retained and percentage
- **Download**: export the fully processed dataset as CSV at any stage

---

## Requirements

### R Version
R ≥ 4.1.0 recommended

### Required Packages

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "shinyBS",
  "readr",
  "readxl",
  "dplyr",
  "ggplot2",
  "plotly",
  "DT",
  "tidyr",
  "scales",
  "jsonlite"
))
```

| Package | Version Tested | Purpose |
|---------|---------------|---------|
| shiny | ≥ 1.7.0 | Core reactive web framework |
| shinydashboard | ≥ 0.7.2 | Dashboard layout and components |
| shinyBS | ≥ 0.61 | Bootstrap tooltips on sidebar controls |
| readr | ≥ 2.1.0 | Fast CSV reading |
| readxl | ≥ 1.4.0 | Excel file reading |
| dplyr | ≥ 1.1.0 | Data manipulation |
| ggplot2 | ≥ 3.4.0 | Chart construction |
| plotly | ≥ 4.10.0 | Interactive chart rendering |
| DT | ≥ 0.27 | Interactive data tables |
| tidyr | ≥ 1.3.0 | Data reshaping |
| scales | ≥ 1.2.0 | Percentage formatting |
| jsonlite | ≥ 1.8.0 | JSON parsing |

---

## Installation

### Step 1 — Clone the repository

```bash
git clone https://github.com/your-username/datapilot.git
cd datapilot
```

### Step 2 — Open R or RStudio and install dependencies

```r
install.packages(c(
  "shiny", "shinydashboard", "shinyBS", "readr", "readxl",
  "dplyr", "ggplot2", "plotly", "DT", "tidyr", "scales", "jsonlite"
))
```

### Step 3 — Run the application

```r
shiny::runApp("AppliedDS_Project2_final.R")
```

Or open `AppliedDS_Project2_final.R` in RStudio and click the **Run App** button in the top-right corner of the editor pane.

> **Note:** Make sure to select **Run in Window** or **Run External** from the dropdown next to the Run App button so the app opens in a browser or separate window.

---

## How to Use the App

### Step-by-Step Workflow

**① Load your data**  
Upload a CSV, Excel, JSON, or RDS file using the Browse button in the sidebar, or select `iris` or `mtcars` from the built-in dataset dropdown. The sidebar info strip updates immediately with dataset dimensions.

**② Inspect the data**  
Navigate to **Data Preview** to see the full table with row/column/missing counts. Go to **Summary Stats** for the R `summary()` output. Check **Missing Values**, **Duplicates**, and **Outliers** tabs for detailed reports.

**③ Clean the data**  
In the sidebar, enable the steps you need by ticking the checkboxes:
- Expand **Missing Values** → choose columns and method → the Data Preview updates immediately
- Expand **Duplicates** → define the key columns and removal strategy
- Expand **Outliers** → select numeric columns and choose identify / remove / Winsorize

**④ Encode categorical variables**  
Expand **Encoding** → select a categorical column → choose one-hot or label encoding → optionally name the output column.

**⑤ Scale numeric variables**  
Expand **Scaling** → select a column → choose Z-score or Min-Max → check the preview card for the resulting mean and range.

**⑥ Engineer new features**  
Expand **Feature Engineering** → choose a transformation method → configure the parameters → name the new column → the preview card shows instant statistics.  
Navigate to **Transformation Comparison** to see a side-by-side before/after view.

**⑦ Explore with charts**  
Navigate to **EDA – Charts** in the left menu. Use the **Chart Variables** panel in the sidebar to select X/Y variables and an optional Color/Group variable. Tabs available:
- Histogram / Density
- Scatter
- Boxplot / Violin
- Bar Chart

Navigate to **Correlation** for the full heatmap.

**⑧ Filter to a subgroup**  
Expand **Filter** in the sidebar → select a column and value → all charts and tables update to the filtered subset. The badge shows rows retained.

**⑨ Download the result**  
Click **Download Processed Data** at the bottom of the sidebar to export the fully processed dataset as a CSV file.

---

## Project Structure

```
datapilot/
│
├── AppliedDS_Project2_final.R   # Main Shiny application (UI + Server)
├── README.md                    # This file
│
└── report/
    └── DataPilot_Report.docx    # Project report (1-2 pages)
```

The entire application is contained in a single file (`AppliedDS_Project2_final.R`) with clearly commented sections:

```
# UI section
#   ├── dashboardHeader
#   ├── dashboardSidebar  (sections 1–10: data source → download)
#   └── dashboardBody     (9 tabItems: guide, preview, summary, missing,
#                          dupes, outliers, comparison, eda, corr)
#
# Server section
#   ├── Helpers           (to_dataframe, get_mode, iqr_outlier_flag, etc.)
#   ├── Reactive chain    (raw_data → na → dup → outlier → encoded
#                          → pre_filter → feature_data)
#   ├── Dynamic UI        (dataset_info_bar, info_boxes, fe_preview,
#                          scale_preview, filter_row_count)
#   └── Outputs           (table, summary, charts ×7, comparison, download)
```

---

## Deployment

The application is deployed on [shinyapps.io](https://www.shinyapps.io/).

To deploy your own instance:

```r
install.packages("rsconnect")
library(rsconnect)

# Set up your shinyapps.io account credentials first:
# rsconnect::setAccountInfo(name="<your-account>", token="<token>", secret="<secret>")

rsconnect::deployApp(
  appFiles = "AppliedDS_Project2_final.R",
  appName  = "datapilot"
)
```

---

*STAT 5243 Applied Data Science | Columbia University | Spring 2026*
