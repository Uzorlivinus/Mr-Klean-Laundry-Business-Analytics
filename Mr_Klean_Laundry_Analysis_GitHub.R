# ================================================================
# MR KLEAN LAUNDRY BUSINESS
# Business Intelligence, Exploratory Data Analysis & Predictive Analytics
# R Programming Project
#
# GitHub-ready R script converted from the original R Markdown work.
# Original analysis, variables, models and methodology are preserved.
# ================================================================

# title: "Mr Klean Laundry Business"
# author: "Uzorlivinus"
# date: "2026-08-05"
# output: html_document

knitr::opts_chunk$set(echo = TRUE)

# Ensemble Machine Learning Project

# Loading Libraries

library(caret)
library(ipred)
library(Metrics)
library(randomForest)
library(gbm)
library(kernlab)
library(tidyr)
library(ggplot2)
library(corrplot)

# Import the dataset

LaundryData <- read.csv(
  "LaundryBusinessMLDataset_Final.csv",
  stringsAsFactors = FALSE
)

# To Verify The Dataset

dim(LaundryData)

names(LaundryData)

head(LaundryData)

# Examining Dataset Structure

str(LaundryData)

# Summary Statistic

summary(LaundryData)

# Checking Missing Values

colSums(is.na(LaundryData))

# Data Preprocessing

# Convert categorical Variables to the Correct Data Types

LaundryData$Gender <- as.factor(LaundryData$Gender)

LaundryData$State <- as.factor(LaundryData$State)

LaundryData$CustomerType <- as.factor(LaundryData$CustomerType)

LaundryData$ServiceName <- as.factor(LaundryData$ServiceName)

LaundryData$LoadType <- as.factor(LaundryData$LoadType)

LaundryData$DeliveryType <- as.factor(LaundryData$DeliveryType)

LaundryData$PaymentMethod <- as.factor(LaundryData$PaymentMethod)

LaundryData$PaymentStatus <- as.factor(LaundryData$PaymentStatus)

# Convert Dates Into The Right Format For R

LaundryData$OrderDate <-
  as.Date(LaundryData$OrderDate,
          format = "%m/%d/%Y")

LaundryData$PickupDate <-
  as.Date(LaundryData$PickupDate,
          format = "%m/%d/%Y")

LaundryData$DeliveryDate <-
  as.Date(LaundryData$DeliveryDate,
          format = "%m/%d/%Y")

LaundryData$PaymentDate <-
  as.Date(LaundryData$PaymentDate,
          format = "%m/%d/%Y")

# Verify If Changes Have Been Effected

str(LaundryData)

# Exploratory Data Analysis (EDA) (Customer Demographics)
# Frequency Distribution

# Gender
table(LaundryData$Gender)

# Customer Type
table(LaundryData$CustomerType)

# Laundry Service
table(LaundryData$ServiceName)

# Delivery Type
table(LaundryData$DeliveryType)

# Payment Status
table(LaundryData$PaymentStatus)

# Load Type
table(LaundryData$LoadType)

# Customer Distribution by State

sort(table(LaundryData$State),
     decreasing = TRUE)

# Age Distribution

summary(LaundryData$Age)

sd(LaundryData$Age)

# Visualisation

# Question 1. Which gender form majority of Mr. klean Laundry Business?

barplot(table(LaundryData$Gender),
        main="Gender Distribution",
        xlab="Gender",
        ylab="Number of Customers")

# Fig. Shows the highest number of customers in terms of gender
# Interpretation:
# The number of customers is more on the male than the female.

customer_summary <- aggregate(
  cbind(Revenue, NetProfit) ~ CustomerType,
  data = LaundryData,
  sum,
  na.rm = TRUE
)

customer_summary

customer_long <- reshape(
  customer_summary,
  varying = c("Revenue", "NetProfit"),
  v.names = "Amount",
  timevar = "Measure",
  times = c("Revenue", "NetProfit"),
  direction = "long"
)

customer_long

# Question 2. Which customer category generates the highest revenue and NetProfit?

ggplot(customer_long,
       aes(x = CustomerType,
           y = Amount,
           fill = Measure)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Revenue and Net Profit by Customer Category",
    x = "Customer Category",
    y = "Amount",
    fill = "Financial Measure"
  ) +
  theme_minimal()

# Fig 2. Revenue and NetProfit by Customer Type

# The graph compares:
# •	Walk-in
# •	Regular
# •	Corporate
# •	Student
# Interpretation
# The graph identifies the customer categories making the largest contribution to revenue and profit.

# Business implication
# The most profitable customer categories should receive greater attention in customer retention and marketing strategies.

state_summary <- aggregate(
  cbind(Revenue, NetProfit) ~ State,
  data = LaundryData,
  sum,
  na.rm = TRUE
)

state_summary

state_long <- reshape(
  state_summary,
  varying = c("Revenue", "NetProfit"),
  v.names = "Amount",
  timevar = "Measure",
  times = c("Revenue", "NetProfit"),
  direction = "long"
)

state_long

# Question 2. Do customers from different states contribute differently to revenue and profitability?

state_long$State <- reorder(
  state_long$State,
  state_long$Amount
)

ggplot(state_long,
       aes(x = Amount,
           y = State,
           fill = Measure)) +
  geom_col(position = "dodge") +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Revenue and Net Profit by State",
    x = "Amount",
    y = "State",
    fill = "Financial Measure"
  ) +
  theme_minimal()

# Fig.3. Revenue and net profit by state

# Interpretation
# The graph shows differences in revenue and profitability across states.

# Business implication
# Management can identify stronger markets and potential areas for business expansion.

service_summary <- aggregate(
  cbind(Revenue, NetProfit) ~ ServiceName,
  data = LaundryData,
  sum,
  na.rm = TRUE
)

service_summary

service_long <- reshape(
  service_summary,
  varying = c("Revenue", "NetProfit"),
  v.names = "Amount",
  timevar = "Measure",
  times = c("Revenue", "NetProfit"),
  direction = "long"
)

service_long

# Question 3. Which laundry services generate the highest revenue and profit?

ggplot(service_long,
       aes(x = Amount,
           y = ServiceName,
           fill = Measure)) +
  geom_col(position = "dodge") +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Revenue and Net Profit by Laundry Service",
    x = "Amount",
    y = "Laundry Service",
    fill = "Financial Measure"
  ) +
  theme_minimal()

# Fig.4.Revenue and Net Profit by Laundry

# Interpretation
# The analysis shows that Laundry is the strongest-performing service of Mr Klean Laundry Business, generating the highest revenue and NetProfit. Ironing and Dry Cleaning are the next strongest contributors, while Mending generates the lowest revenue and NetProfit. Management should therefore continue to support and promote the Laundry service while reviewing the performance and demand for lower-performing services.

# Business implication
# High-performing services can be prioritized while lower-performing services can be reviewed for pricing and cost efficiency.

# Question 4. Is there a relationship between the quantity of clothes, the weight of clothes processed, and the revenue generated?

service_quantity <- aggregate(
  QuantityOfClothes ~ ServiceName,
  data = LaundryData,
  mean,
  na.rm = TRUE
)

service_weight <- aggregate(
  WeightKg ~ ServiceName,
  data = LaundryData,
  mean,
  na.rm = TRUE
)

service_revenue <- aggregate(
  Revenue ~ ServiceName,
  data = LaundryData,
  mean,
  na.rm = TRUE
)

service_quantity
service_weight
service_revenue

# The above code was used to find the average of how many clothes that are processed for each service,the average of weight of the processed clothes for each service and how much revenue does each service generate per transaction?

# The three in one were used to describe and compare the three variables across the five laundry services before performing actual relationship analysis. Service → Average Quantity + Average Weight + Average Revenue

cor(
  LaundryData[, c(
    "QuantityOfClothes",
    "WeightKg",
    "Revenue"
  )],
  use = "complete.obs"
)

cor_matrix <- cor(
  LaundryData[, c(
    "QuantityOfClothes",
    "WeightKg",
    "Revenue"
  )],
  use = "complete.obs"
)

cor_data <- as.data.frame(as.table(cor_matrix))

ggplot(cor_data,
       aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = round(Freq, 2)), size = 5) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  labs(
    title = "Correlation Between Quantity, Weight and Revenue",
    x = "",
    y = "",
    fill = "Correlation"
  ) +
  theme_minimal()

# Fig. 5. Relationship Between Quantity, Weight and Revenue
# Interpretation
# Yes, there is relationship between weight and quantity but there is no relationship between weight and revenue or quantity and revenue.
# Quantity and weight are strongly related to each other, but neither has a meaningful linear relationship with revenue. Therefore, revenue is likely influenced by other business factors beyond the volume or weight of clothes processed.
# Management should not rely on the number or weight of clothes alone when making revenue or pricing decisions.

# Therefore, management should pay greater attention to other factors that may determine revenue, such as:
# Service type
# Pricing structure
# Customer category
# Delivery type
# Additional service charges
# Strong presentation statement

# Question 5. Which delivery option (Customer Pickup, Home Delivery, or Express Delivery) contributes most to business revenue?

delivery_summary <- aggregate(
  cbind(Revenue, NetProfit) ~ DeliveryType,
  data = LaundryData,
  sum,
  na.rm = TRUE
)

delivery_summary

delivery_long <- reshape(
  delivery_summary,
  varying = c("Revenue", "NetProfit"),
  v.names = "Amount",
  timevar = "Measure",
  times = c("Revenue", "NetProfit"),
  direction = "long"
)

ggplot(
  delivery_long,
  aes(
    x = Amount,
    y = DeliveryType,
    fill = Measure
  )
) +
  geom_col(position = "dodge") +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Revenue and Net Profit by Delivery Option",
    x = "Amount ($)",
    y = "Delivery Option",
    fill = "Financial Measure"
  ) +
  theme_minimal()

# Fig.6. Revenue and Net Profit by Delivery Option
# Interpretation
# Customer Pickup contributes the highest revenue and NetProfit to Mr Klean Laundry Business. This indicates that Customer Pickup is currently the strongest delivery option in terms of overall financial contribution. Management should maintain and strengthen this service while reviewing the lower-performing Express and Home Delivery options to determine whether pricing, demand, operating costs, or service utilization are affecting their profitability.

# Question 6. Which operational cost components contribute most to the overall expenses incurred by the business?

# This question is important because management needs to know where the business is spending the most money so that cost-control efforts can focus on the areas with the greatest impact.

# Calculate the operational Cost

cost_summary <- data.frame(
  CostComponent = c(
    "Labour Cost",
    "Electricity Cost",
    "Water Cost",
    "Detergent Cost",
    "Chemical Cost",
    "Packaging Cost",
    "Fuel Cost",
    "Maintenance Cost",
    "Discount Given",
    "Tax",
    "Other Cost"
  ),
  TotalCost = c(
    sum(LaundryData$LabourCost, na.rm = TRUE),
    sum(LaundryData$ElectricityCost, na.rm = TRUE),
    sum(LaundryData$WaterCost, na.rm = TRUE),
    sum(LaundryData$DetergentCost, na.rm = TRUE),
    sum(LaundryData$ChemicalCost, na.rm = TRUE),
    sum(LaundryData$PackagingCost, na.rm = TRUE),
    sum(LaundryData$FuelCost, na.rm = TRUE),
    sum(LaundryData$MaintenanceCost, na.rm = TRUE),
    sum(LaundryData$DiscountGiven, na.rm = TRUE),
    sum(LaundryData$Tax, na.rm = TRUE),
    sum(LaundryData$OtherCost, na.rm = TRUE)
  )
)

cost_summary

# The analysis made use of the operational costs such as Labour Cost, Electricity Cost, Water Cost, Detergent Cost, Chemical Cost, Packaging Cost, Fuel Cost, Maintenance Cost, Tax, Other Cost and Discount Given

# Sort from Highest to Lowest

cost_summary <- cost_summary[
  order(-cost_summary$TotalCost),
]

cost_summary

ggplot(
  cost_summary,
  aes(
    x = TotalCost,
    y = reorder(CostComponent, TotalCost)
  )
) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Operational Cost Components",
    x = "Total Cost ($)",
    y = "Cost Component"
  ) +
  theme_minimal()

# Fig.7. Operational Cost

# Interpretation
# The analysis shows that Labour Cost is the dominant operational expense, followed by Tax and Electricity. This indicates that effective labour management and control of energy-related expenses should be major priorities for management in improving operational efficiency and profitability.”

# Question 7. Are there monthly or seasonal patterns in customer demand, revenue generation, and profitability?
# To do this, We will examine three business measures:
# Number of Orders → customer demand
# Total Revenue ($) → revenue generation
# Total NetProfit ($) → profitability

# Create the month variable

LaundryData$Month <- format(
  as.Date(LaundryData$OrderDate),
  "%Y-%m"
)

head(LaundryData[, c("OrderDate", "Month")])

# Summarise monthly performance

monthly_summary <- aggregate(
  cbind(Revenue, NetProfit) ~ Month,
  data = LaundryData,
  sum,
  na.rm = TRUE
)

monthly_orders <- aggregate(
  OrderID ~ Month,
  data = LaundryData,
  length
)

names(monthly_orders)[2] <- "Orders"

monthly_summary <- merge(
  monthly_summary,
  monthly_orders,
  by = "Month"
)

monthly_summary

# We run the code above because the management question ask about monthly or seasonal patterns.So, we need to determine whether certain periods have:
# Higher customer demand
# Higher revenue
# Higher profitability

# Trend In Monthly Customers Order

ggplot(monthly_summary, aes(x = Month, y = Orders, group = 1)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  labs(
    title = "Monthly Customer Demand",
    x = "Month",
    y = "Number of Orders"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

# Trend In Monthly Revenue

ggplot(monthly_summary, aes(x = Month, y = Revenue, group = 1)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  labs(
    title = "Monthly Revenue Trend",
    x = "Month",
    y = "Revenue ($)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

# Trend in Monthly Netprofit

ggplot(monthly_summary, aes(x = Month, y = NetProfit, group = 1)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  labs(
    title = "Monthly Net Profit Trend",
    x = "Month",
    y = "Net Profit ($)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

# Interpretation
# The monthly analysis reveals substantial fluctuations in customer demand, revenue, and profitability. Although high-demand months generally produce higher revenue and profit, no consistent seasonal pattern is evident across the years. Management should therefore focus on flexible capacity planning and targeted strategies for managing periods of high and low demand.”

# Business Implication
# This finding is useful for management because it suggests that Mr Klean Laundry Business should prepare for fluctuations in demand rather than relying on a fixed seasonal cycle.

# Exploratory Data Analysis (EDA) (Financial Performance Analysis)

# Total Revenue
sum(LaundryData$Revenue)

# Total Expenses
sum(LaundryData$TotalExpense)

# Total Net Profit
sum(LaundryData$NetProfit)

# Average Revenue
mean(LaundryData$Revenue)

# Average Expense
mean(LaundryData$TotalExpense)

# Average Net Profit
mean(LaundryData$NetProfit)

# Descriptive Statistics
# 1. Summary Statistics

summary(LaundryData$Revenue)

summary(LaundryData$TotalExpense)

summary(LaundryData$NetProfit)

# Interpretation (Revenue)
# This gives you the Minimum, 1st Quartile, Median, Mean, 3rd Quartile, and Maximum for each variable
# The revenue generated per transaction ranged from approximately $510 to $14,994, showing considerable variation in transaction values.
# The mean revenue of $7,839.30 is very close to the median of $7,797.90. This suggests that the revenue distribution is relatively balanced, with no major distortion caused by extremely high or low transactions.
# The standard deviation of $4,185.62 indicates substantial variation around the average revenue. This means that customers do not spend the same amount per transaction; some transactions generate considerably more revenue than others.

# nterpretation:
# Total expenses per transaction ranged from $200.30 to $8,310.70.

# The average expense was $3,643.50, while the median was $3,446.20. The relatively close values indicate that the expense distribution is reasonably stable, although some transactions incur considerably higher costs.

# The standard deviation of $2,018.11 shows that operating expenses vary substantially between transactions. This variation is important because controlling high-cost transactions can help improve profitability.

# 2. Measures of Dispersion

sd(LaundryData$Revenue)

sd(LaundryData$TotalExpense)

sd(LaundryData$NetProfit)

# This gives the standard deviation, which tells us how widely the values vary around their respective means.

# Distributive Analysis
# Revenue Distribution

hist(LaundryData$Revenue,
     main = "Revenue Distribution",
     xlab = "Revenue ($)",
     ylab = "Frequency")

# Expense Distribution

hist(LaundryData$TotalExpense,
     main = "Total Expense Distribution",
     xlab = "Total Expense ($)",
     ylab = "Frequency")

# Net Profit Distribution

hist(LaundryData$NetProfit,
     main = "Net Profit Distribution",
     xlab = "Net Profit ($)",
     ylab = "Frequency")

# Relationship Between Variables (Correlation Analysis)
# Since correlation only works with numeric variables, create a new dataset containing only numeric columns.

# Select only numeric variables
numeric_data <- LaundryData[sapply(LaundryData, is.numeric)]

# View variable names
names(numeric_data)

# Correlation Matrix

cor_matrix <- cor(numeric_data)

round(cor_matrix, 2)

# Outlier Detection and Treatment

# Revenue

boxplot(LaundryData$Revenue,
        main = "Boxplot of Revenue",
        ylab = "Revenue ($)")

# Total Expenses

boxplot(LaundryData$TotalExpense,
        main = "Boxplot of Total Expense",
        ylab = "Total Expense ($)")

# Net Profit

boxplot(LaundryData$NetProfit,
        main = "Boxplot of Net Profit",
        ylab = "Net Profit ($)")

# Quantity Of Clothes

boxplot(LaundryData$QuantityOfClothes,
        main = "Boxplot of Quantity of Clothes",
        ylab = "Number of Clothes")

# Wieght

boxplot(LaundryData$WeightKg,
        main = "Boxplot of Weight",
        ylab = "Weight (Kg)")

# Age

boxplot(LaundryData$Age,
        main = "Boxplot of Customer Age",
        ylab = "Age")

# Data Preparation for Modelling
# The target variable is NetProfit and apart from Revenue and Total Expense that directly determines the target variable via formula. I will use other variables to predict the Target Variable to deprive the model from learning the formula rather than discovering meaningful patterns. I will use the listed variable:
# Gender
# Age
# State
# CustomerType
# ServiceName
# QuantityOfClothes
# WeightKg
# LoadType
# DeliveryType
# PaymentMethod
# PaymentStatus
# LabourCost
# ElectricityCost
# WaterCost
# DetergentCost
# ChemicalCost
# PackagingCost
# FuelCost
# MaintenanceCost
# DiscountGiven
# Tax
# OtherCost

# Create the Modelling Dataset

model_data <- LaundryData[, c(
  "Gender",
  "Age",
  "State",
  "CustomerType",
  "ServiceName",
  "QuantityOfClothes",
  "WeightKg",
  "LoadType",
  "DeliveryType",
  "PaymentMethod",
  "PaymentStatus",
  "LabourCost",
  "ElectricityCost",
  "WaterCost",
  "DetergentCost",
  "ChemicalCost",
  "PackagingCost",
  "FuelCost",
  "MaintenanceCost",
  "DiscountGiven",
  "Tax",
  "OtherCost",
  "NetProfit"
)]

# Confirm the Dataset

str(model_data)

dim(model_data)

# Numerical variables from the 23-variable modelling dataset

# Select all numerical variables from the 23-variable modelling dataset
numeric_data <- model_data[
  sapply(model_data, is.numeric)
]

# Calculate the correlation matrix
cor_matrix <- cor(
  numeric_data,
  use = "complete.obs",
  method = "pearson"
)

# View the correlation matrix
round(cor_matrix, 3)

# Correlation Matrix of Numerical Variables

corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  number.cex = 0.7,
  tl.col = "black",
  tl.srt = 45,
  title = "Correlation Matrix of Numerical Modelling Variables",
  mar = c(0, 0, 2, 0)
)

# Select numerical variables from the modelling data
numeric_data <- model_data[
  sapply(model_data, is.numeric)
]

# Calculate correlation of each numerical variable with NetProfit
cor_with_profit <- cor(
  numeric_data,
  use = "complete.obs"
)[, "NetProfit"]

# Remove NetProfit itself
cor_with_profit <- cor_with_profit[
  names(cor_with_profit) != "NetProfit"
]

# Convert to data frame
cor_df <- data.frame(
  Variable = names(cor_with_profit),
  Correlation = as.numeric(cor_with_profit)
)

# Order variables by correlation
cor_df <- cor_df[
  order(cor_df$Correlation),
]

# Create horizontal bar chart

ggplot(
  cor_df,
  aes(
    x = Correlation,
    y = reorder(Variable, Correlation)
  )
) +
  geom_col() +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  scale_x_continuous(
    limits = c(-1, 1)
  ) +
  labs(
    title = "Correlation of Numerical Variables with Net Profit",
    x = "Correlation with Net Profit",
    y = "Numerical Modelling Variable"
  ) +
  theme_minimal()

# This is a more clearer correlation schart and it hows the statistical relationship with NetProfit.

# Feature Selection for Our Target NetProfit

set.seed(123)

# Build Random Forest model
rf_feature_model <- train(
  NetProfit ~ .,
  data = model_data,
  method = "rf",
  trControl = trainControl(
    method = "cv",
    number = 10
  ),
  importance = TRUE
)

# Display variable importance
importance_results <- varImp(rf_feature_model)

print(importance_results)

# Random Forest model was built to identify the variables that contribute most to predicting NetProfit

saveRDS(rf_feature_model, "rf_feature_model.rds")

rf_feature_model <- readRDS("rf_feature_model.rds")

plot(
  importance_results,
  top = 15,
  main = "Top 15 Features Important for Predicting Net Profit"
)

# Feature importance shows which predictors contribute most to the machine-learning model.

plot(
  importance_results,
  top = 22,
  main = "Feature Importance for Net Profit Prediction"
)

# Machine Learning Modeling (Split the Data into Training 80% set and Testing 20% Set)

# Set seed for reproducibility
set.seed(123)

# Split data (80% Training, 20% Testing)
trainIndex <- createDataPartition(model_data$NetProfit,
                                  p = 0.80,
                                  list = FALSE)

train_data <- model_data[trainIndex, ]
test_data  <- model_data[-trainIndex, ]

# To Check The Split

dim(train_data)
dim(test_data)

str(train_data)

str(test_data)

# Create the Training Control
# We'll first create a training control object. This ensures that every model is trained under the same conditions, making the comparison fair and scientifically valid.

# Set up 10-fold Cross Validation
control <- trainControl(
  method = "cv",
  number = 10
)

control

# The training data is divided into 10 equal folds.
# 9 folds are used to train.
# 1 fold is used to validate.
# This process repeats 10 times.
# The final performance is the average across all folds.
# This approach produces more reliable and less biased performance estimates than a single train/test split.

# Building The Model (Bagging Regression Model)

# Train the Bagging Model

set.seed(123)

bag_model <- train(
  NetProfit ~ .,
  data = train_data,
  method = "treebag",
  trControl = control
)

# View The Model

bag_model

# Make Predictions

bag_pred <- predict(
    bag_model,
    newdata = test_data
)

# Evaluate The Model

RMSE(bag_pred, test_data$NetProfit)

MAE(bag_pred, test_data$NetProfit)

R2(bag_pred, test_data$NetProfit)

# Visualize Actual Vs Predicted Variables

plot(
  test_data$NetProfit,
  bag_pred,
  xlab = "Actual Net Profit",
  ylab = "Predicted Net Profit",
  main = "Bagging Regression: Actual vs Predicted"
)

abline(0, 1, col = "red", lwd = 2)

# Random Forest Regression (Train the Random Forest Model)

set.seed(123)

rf_model <- train(
  NetProfit ~ .,
  data = train_data,
  method = "rf",
  trControl = control,
  importance = TRUE
)

# Save the trained Random Forest model

saveRDS(rf_model, "RandomForest_Model.rds")

rf_model <- readRDS("RandomForest_model.rds")

# To View The Model

rf_model

# Prediction Using The TestSet

rf_pred <- predict(
  rf_model,
  newdata = test_data
)

# Evaluate The Model

RMSE(rf_pred, test_data$NetProfit)

MAE(rf_pred, test_data$NetProfit)

R2(rf_pred, test_data$NetProfit)

# Plot Of Actual Vs Predicted

plot(
  test_data$NetProfit,
  rf_pred,
  xlab = "Actual Net Profit",
  ylab = "Predicted Net Profit",
  main = "Random Forest Regression: Actual vs Predicted"
)

abline(0, 1, col = "red", lwd = 2)

# Gradient Boosting Machine (GBM) Regression
#Set Seed

set.seed(123)

# Train GBM Model

gbm_model <- train(
  NetProfit ~ .,
  data = train_data,
  method = "gbm",
  trControl = trainControl(method = "cv", number = 10),
  verbose = FALSE
)

saveRDS(gbm_model, "gbm_Model.rds")

gbm_model <- readRDS("gbm_model.rds")

# Display The Model

print(gbm_model)

# Predict The Model on  Test Data

gbm_pred <- predict(gbm_model, newdata = test_data)

# Evalute Model Perfomance

postResample(gbm_pred, test_data$NetProfit)

# Scatter plot (Actual vs Predicted)

plot(
  test_data$NetProfit,
  gbm_pred,
  main = "GBM Regression: Actual vs Predicted",
  xlab = "Actual Net Profit",
  ylab = "Predicted Net Profit",
  pch = 1
)

abline(0, 1, col = "red", lwd = 2)

# Support Vector Machine (SVM) Regression
# Train the SVM model

set.seed(123)

svm_model <- train(
  NetProfit ~ .,
  data = train_data,
  method = "svmRadial",
  trControl = control,
  tuneLength = 5
)

svm_model

# Predict The Model on  Test Data

svm_predictions <- predict(svm_model, newdata = test_data)

head(svm_predictions)

# Evalute Model Perfomance

postResample(svm_predictions, test_data$NetProfit)

# Scatter Plot Actual Vs Predicted

plot(
  test_data$NetProfit,
  svm_predictions,
  main = "SVM Regression: Actual vs Predicted",
  xlab = "Actual Net Profit",
  ylab = "Predicted Net Profit",
  pch = 1
)

abline(0, 1, col = "red", lwd = 2)

# Partial Least Squares (PLS) Regression Model

# Train the PLS Model

set.seed(123)

pls_model <- train(
  NetProfit ~ .,
  data = train_data,
  method = "pls",
  trControl = control,
  tuneLength = 20
)

pls_model

saveRDS(pls_model, "pls_model.rds")

pls_model <- readRDS("pls_model.rds")

# # Predict NetProfit for the test dataset

pls_predictions <- predict(pls_model, newdata = test_data)

# Display the first six predictions
head(pls_predictions)

# Evaluate PLS Regression Model

postResample(pls_predictions, test_data$NetProfit)

# Actual vs Predicted Plot for PLS

plot(test_data$NetProfit,
     pls_predictions,
     main = "PLS Regression: Actual vs Predicted Net Profit",
     xlab = "Actual Net Profit",
     ylab = "Predicted Net Profit",
     pch = 19)

abline(a = 0,
       b = 1,
       col = "red",
       lwd = 2)

# k-Nearest Neighbors (kNN) Regression
# Train the kNN Regression Model

knn_model <- train(
  NetProfit ~ .,
  data = train_data,
  method = "knn",
  trControl = control,
  tuneLength = 10
)
knn_model

saveRDS(knn_model, "knn_model.rds")

knn_model <- readRDS("knn_model.rds")

# Predict Net Profit on the Test Data

knn_predictions <- predict(knn_model, newdata = test_data)

# View the first six predictions

head(knn_predictions)

# Evaluate the kNN Model

postResample(knn_predictions, test_data$NetProfit)

# Actual vs Predicted Plot for kNN

plot(test_data$NetProfit,
     knn_predictions,
     main = "kNN Regression: Actual vs Predicted Net Profit",
     xlab = "Actual Net Profit",
     ylab = "Predicted Net Profit",
     pch = 19)

abline(0, 1, col = "red", lwd = 2)

# Performance Summary Table To Create Model Comparisim
# By gathering the evaluation results from each model.

model_comparison <- data.frame(

  Model = c("Bagging",
            "Random Forest",
            "GBM",
            "SVM",
            "PLS",
            "kNN"),

  RMSE = c(
    535.7497,
    300.1462,
    125.0357,
    450.6872,
    0.0336,
    495.5149
  ),

  Rsquared = c(
    0.9464361,
    0.9823295,
    0.9969741,
    0.9622112,
    1.0000000,
    0.9515468
  ),

  MAE = c(
    421.0446,
    215.2852,
    90.8455,
    270.0380,
    0.0280,
    334.3182
  )

)

model_comparison

# checking if there is any leakage before accepting that pls is the best model here

names(train_data)

formula(pls_model)

varImp(pls_model)

predictors <- names(train_data)

predictors

# Rank the Models by RMSE

model_ranking <- model_comparison[order(model_comparison$RMSE), ]

model_ranking

# Comparison of RMSE Across Machine Learning Models

# Arrange models from lowest RMSE to highest RMSE
rmse_data <- model_comparison[
  order(model_comparison$RMSE),
]

barplot(
  rmse_data$RMSE,
  names.arg = rmse_data$Model,
  horiz = TRUE,
  main = "Model Comparison: RMSE (Lower is Better)",
  xlab = "RMSE",
  ylab = "Model",
  las = 1,
  col = "blue"
)

# Comparison of R-squared Across Machine Learning Models

# Arrange models from highest R-squared to lowest
r2_data <- model_comparison[
  order(model_comparison$Rsquared, decreasing = TRUE),
]

barplot(
  r2_data$Rsquared,
  names.arg = r2_data$Model,
  horiz = TRUE,
  main = "Model Comparison: R-squared (Higher is Better)",
  xlab = "R-squared",
  ylab = "Model",
  las = 1,
  col = "blue"
)

# Comparison of MAE Across Machine Learning Models

# Arrange models from lowest MAE to highest
mae_data <- model_comparison[
  order(model_comparison$MAE),
]

barplot(
  mae_data$MAE,
  names.arg = mae_data$Model,
  horiz = TRUE,
  main = "Model Comparison: MAE (Lower is Better)",
  xlab = "Mean Absolute Error (MAE)",
  ylab = "Model",
  las = 1,
  col = "blue"
)

model_ranking <- data.frame(
  Rank = 1:6,
  Model = c("PLS", "GBM", "Random Forest", "SVM", "kNN", "Bagging"),
  RMSE = c(0.0336, 125.0357, 300.1462, 450.6872, 495.5149, 535.7497),
  Rsquared = c(1.0000, 0.9969741, 0.9823295, 0.9622112, 0.9515468, 0.9464361),
  MAE = c(0.0280, 90.8455, 215.2852, 270.0380, 334.3182, 421.0446),
  Performance = c(
    "Excellent",
    "Very Good",
    "Very Good",
    "Good",
    "Good",
    "Fair"
  )
)

model_ranking

# Residual Analysis
# PLS Prediction On Test Dataset
# We're asking our trained PLS model to predict NetProfit for the 200 observations in test_data.
# We already evaluated PLS and got the excellent results:
# RMSE = 0.0336
# R² = 1.0000
# MAE = 0.0280
# Now we're going to examine why the model performs so well by looking at its residuals.

pls_predictions <- predict(pls_model, newdata = test_data)

pls_predictions <- as.vector(pls_predictions)

head(pls_predictions)

# Calculate the residuals

# A residual is the difference between the actual Net Profit and the model's predicted Net Profit:
# Residual = Actual − Predicted
# A residual close to 0 means the prediction is very close to the actual value.

pls_residuals <- test_data$NetProfit - pls_predictions

head(pls_residuals)

summary(pls_residuals)

# The residual analysis of the PLS model showed that the prediction errors were centered very close to zero, with a mean residual of 0.002923 and a median residual of 0.004813. The residuals ranged from -0.062933 to 0.088423, indicating very small prediction errors. These results suggest that the PLS model produced highly accurate predictions with minimal systematic bias.

# Residual vs Fitted Plot

plot(
  pls_predictions,
  pls_residuals,
  xlab = "Fitted Values",
  ylab = "Residuals",
  main = "Residuals vs Fitted Values"
)

abline(h = 0, lty = 2)

# The model's prediction errors appear to be small and randomly distributed suggesting that PLS predicted NetProfit with high predictive accuracy

# Histogram of Residuals
# check the distribution of the residuals.

hist(
  pls_residuals,
  main = "Histogram of PLS Model Residuals",
  xlab = "Residuals"
)

# The histogram of the PLS residuals showed that the residuals were concentrated around zero, with most prediction errors falling approximately between -0.05 and 0.05. This indicates that the model produced very small prediction errors and exhibited minimal systematic bias.

# Q-Q Plot
# The Q-Q plot helps us assess whether the residuals approximately follow a normal distribution.

qqnorm(
  pls_residuals,
  main = "Q-Q Plot of PLS Model Residuals"
)

qqline(pls_residuals, lty = 2)

# The Q-Q plot of the PLS model residuals showed that the majority of the observations closely followed the diagonal reference line, with only slight deviations at the tails. This indicates that the residuals were approximately normally distributed. Therefore, the residual diagnostics provide no substantial evidence of violation of the normality assumption

# The residual diagnostics provide strong supporting evidence for the adequacy of the PLS model. The residuals are small, centered close to zero, randomly distributed, and approximately normally distributed. This is important because we're not simply saying "PLS is the best because it has the lowest RMSE." We have now performed additional diagnostic checks to support the model's adequacy.

# PLS Variable Importance
# We use this to check the predictor variables that contribute most to the prediction of NetProfit in the PLS model.

pls_importance <- varImp(pls_model)

pls_importance

# LabourCost is the most influential predictor in the PLS model, with its importance scaled to 100.
# Tax is the second most important predictor, with an importance score of 58.48. The next important variables are MaintenanceCost and ElectricityCost, followed by ChemicalCost, OtherCost, FuelCost, DetergentCost, WaterCost, and DiscountGiven. This gives us an important business interpretation: cost-related variables are the dominant predictors of NetProfit.

# Visualize the PLS Variable Importance

plot(
  pls_importance,
  main = "Variable Importance for PLS Model"
)

# The gragh shows that LabourCost = 100% → the most influential predictor followed by Tax ≈ 58.5% → second most influential. Then, MaintenanceCost ≈ 23.4%, ElectricityCost ≈ 22.9%. The remaining variables have progressively lower importance.

# Actual vs Predicted NetProfit
# his plot compares:
# Actual NetProfit from test_data and Predicted NetProfit from pls_predictions

plot(
  test_data$NetProfit,
  pls_predictions,
  xlab = "Actual NetProfit",
  ylab = "Predicted NetProfit",
  main = "Actual vs Predicted NetProfit - PLS Model"
)

abline(0, 1, lty = 2)

# The actual-versus-predicted plot for the PLS model showed that the observations were closely distributed along the 45-degree reference line (y=x). This indicates a strong agreement between the observed and predicted NetProfit values. The close alignment of the points with the reference line provides further evidence of the model's high predictive accuracy and low prediction error on the independent test dataset.

# Confirmation that PLS is the selected model
# Based on the comparative evaluation of the six regression models, Partial Least Squares (PLS) was selected as the final predictive model. The PLS model achieved the lowest RMSE (0.0336) and MAE (0.0280), together with an R² of 1.0000. Residual diagnostics further indicated that the prediction errors were centered close to zero, randomly distributed, and approximately normally distributed. The actual-versus-predicted plot also demonstrated a strong agreement between observed and predicted NetProfit values. Consequently, PLS was considered the best-performing model for predicting NetProfit in the study.

# PLS Model Summary

pls_final_summary <- data.frame(
  Model = "PLS",
  Components = pls_model$bestTune$ncomp,
  RMSE = 0.0336,
  Rsquared = 1.0000,
  MAE = 0.0280
)

pls_final_summary

# Interpretation

# The PLS model selected 14 components through 10-fold cross-validation and achieved extremely strong performance:

# RMSE = 0.0336 — very small prediction error.
# R² = 1.0000 — the model explains essentially all observed variation in the cross-validation results.
# MAE = 0.0280 — the average absolute prediction error is extremely small.
