📊 Telecom Churn Analysis
A capstone project focused on identifying key factors behind customer churn in the telecom industry and building predictive models to proactively reduce churn.

👩‍💻 Author
Ajuna P John

📌 Project Objective
Customer attrition is a major challenge in the telecom sector. This project aims to:

Analyze customer behavior using a rich telecom dataset.

Predict churn using machine learning models.

Recommend strategic actions for churn prevention.

📂 Dataset Overview
Source: Telecom_Sampled.csv

Records: 26,518 customers

Features: 81 variables

Target: churn (1 = Churned, 0 = Not Churned)

⚙️ Methodology
🧹 Data Preprocessing
Imputation of missing values (Median & KNN)

Outlier detection and treatment via capping/flooring

Transformation of skewed variables

Removal of irrelevant and high-missing-value features

🔍 Exploratory Data Analysis
Univariate and bivariate analysis

Factor analysis and PCA for dimensionality reduction

Identification of key churn influencers

🤖 Modeling Techniques
Model	Accuracy (Test)	AUC
Logistic Regression	75.86%	0.625
LDA	75.8%	0.627
CART	73.68%	0.603
Random Forest (Tuned)	76.34%	0.669

💡 Key Insights
Minutes of Usage, Billing Factors, and Equipment Age are strong churn predictors.

Service issues like dropped calls significantly affect customer retention.

Random Forest outperformed other models in predictive performance.

✅ Recommendations
Offer flexible, customized plans to boost engagement.

Improve network quality to reduce dropped/blocked calls.

Reassess billing strategies to retain at-risk customers.

Use targeted campaigns based on customer demographics.

🧰 Tech Stack
Language: R

Libraries: ggplot2, caret, rpart, MASS, randomForest

Tools: RStudio, Excel
