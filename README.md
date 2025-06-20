Telecom Churn Analysis
Project Overview
This project analyzes customer churn behavior in the telecom sector using a dataset with over 26,000 customer records and 81 variables. The objective is to identify the factors influencing churn, build predictive models, and recommend actionable insights to proactively reduce churn rates and enhance customer retention.

Author
Ajuna P John
June 2021

Problem Statement
Telecom providers are facing rising customer attrition. Current reactive strategies are insufficient. This project seeks to:

Derive insights from customer behavior

Predict churn likelihood using statistical and machine learning models

Recommend interventions to reduce churn

Dataset
Name: Telecom_Sampled.csv

Observations: 26,518

Features: 81 (47 continuous, 34 categorical)

Target Variable: churn (0 = Non-Churn, 1 = Churn)

Methodology
Data Preprocessing
Handled missing values using median imputation and KNN for categorical variables

Outlier treatment via capping/flooring

Transformation of variables including factor conversions

Dropped high-missing or irrelevant variables

Exploratory Data Analysis (EDA)
Univariate and bivariate analyses performed

Chi-square and correlation tests used

Factor analysis and PCA for dimension reduction

Modeling Techniques Used
Logistic Regression

Linear Discriminant Analysis (LDA)

Classification and Regression Trees (CART)

Random Forest (with tuning)

Stepwise AIC Regression

Model Evaluation Metrics
Accuracy

Confusion Matrix

ROC Curve and AUC Score

Key Findings
Churn is influenced heavily by:

Minutes of usage

Customer and equipment characteristics

Cost and billing variables

Network quality (dropped/blocked calls)

Random Forest performed best:

Train Accuracy: 97.02%

Test Accuracy: 76.34%

AUC: 0.6686

Recommendations
Flexible Plans: Offer customized packages based on usage to retain customers.

Improve Network Quality: Address dropped/blocked call issues.

Proactive Billing Strategies: Revise plans before customer dissatisfaction grows.

Targeted Marketing: Focus on specific customer demographics for retention campaigns.


Tools & Technologies
R

Data Visualization (ggplot2)

Statistical Analysis

Machine Learning Models
