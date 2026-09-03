# Mr Klean Laundry Business Analytics

## Business Intelligence, Exploratory Data Analysis & Predictive Analytics Using R, Power BI and SQL

### Project Overview

This project analyzes the operational, financial and customer performance of a laundry business using data analytics and machine learning.

The analysis combines:

- SQL for relational data design and data integration
- Power BI for business intelligence and dashboard development
- R for exploratory data analysis, statistical analysis and predictive modelling

The objective is to transform business data into actionable insights that can support better operational and financial decision-making.

---
## Business Questions

The analysis focuses on questions such as:

- Which laundry services generate the highest revenue and profit?
- Which customer categories contribute most to business performance?
- Which states generate the strongest financial performance?
- Which delivery types perform best?
- What are the major operational costs?
- How does revenue and demand change over time?
- Which factors are most associated with Net Profit?
- Can machine-learning models predict Net Profit?

---

## Dataset

The project uses a synthetic laundry-business dataset containing **1,000 records and 32 variables**.

The data was designed using a relational database structure, generated using Mockaroo, integrated through MySQL, and exported for analysis in R.

The dataset contains information relating to:

- Customers
- Orders
- Services
- Employees
- Machines
- Payments
- Operational costs
- Revenue
- Expenses
- Net Profit

---

## Tools & Technologies

| Tool | Purpose |
|---|---|
| SQL / MySQL | Database design, integration and data preparation |
| Power BI | Business intelligence and dashboard development |
| R | Data analysis and predictive modelling |
| Mockaroo | Synthetic data generation |
| Excel | Data preparation and supporting analysis |

### R Packages

The analysis uses R packages including:

- `caret`
- `ggplot2`
- `corrplot`
- `randomForest`
- `gbm`
- `kernlab`
- `Metrics`
- `ipred`
- `tidyr`

---

## Analysis Performed

### Exploratory Data Analysis

The project examines:

- Customer demographics
- Customer types
- State performance
- Service performance
- Order quantities
- Weight of clothes
- Delivery types
- Revenue
- Expenses
- Net Profit
- Monthly business trends
- Operational costs
- Correlations between numerical variables

---

## Machine Learning

Net Profit was selected as the target variable.

Revenue and Total Expense were excluded from the predictive modelling dataset because they directly determine Net Profit and could cause the models to learn the underlying calculation rather than meaningful business patterns.

Six regression models were evaluated:

1. Bagging
2. Random Forest
3. Gradient Boosting Machine (GBM)
4. Support Vector Machine (SVM)
5. Partial Least Squares (PLS)
6. k-Nearest Neighbors (kNN)

The modelling process used an **80% training / 20% testing split** and **10-fold cross-validation**.

---

## Model Evaluation

The models were evaluated using:

- RMSE — Root Mean Squared Error
- R² — R-squared
- MAE — Mean Absolute Error

In the documented analysis, PLS produced the strongest recorded performance.

However, the exceptionally high PLS performance requires additional target-leakage and out-of-sample validation before treating the model as production-ready.

---

## Key Business Findings

The analysis identified several important business patterns:

- Laundry was the strongest-performing service in the analysis.
- Customer Pickup was the strongest delivery type.
- Labour cost was the largest operational cost.
- Tax and electricity were also significant cost components.
- Cost-related variables were among the strongest predictors of Net Profit.
- Monthly analysis revealed changes in demand and revenue over time.

---

## Business Recommendations

Based on the analysis, the business should:

1. Continue strengthening its highest-performing laundry services.
2. Monitor labour costs closely because they represent a major operational expense.
3. Review electricity and other major operating costs regularly.
4. Use customer and service-level data to identify profitable customer segments.
5. Monitor monthly demand patterns to improve operational planning.
6. Use predictive analytics as a decision-support tool while validating models with additional real-world data.

---

## Project Structure

```text

Mr-Klean-Laundry-Business-Analytics/
│
├── README.md
│
├── Mr_Klean_Laundry_Analysis_GitHub.R
│
└── LaundryBusinessMLDataset_final.csv

```

## Project Files

- [R Analysis Script](Mr_Klean_Laundry_Analysis_GitHub.R)
- [Business Analytics Presentation](Mr_Klean_Laundry_Business_Analytics.pdf)
- [Step-by-Step Modelling Report](Mr_Klean_Laundry_Step_by_Step_Modelling_Corrected.pdf)
- [Dataset](LaundryBusinessMLDataset_final.csv)

