# Customer Revenue Prediction – Capstone Project

This project predicts customer revenue using a combination of Elastic Net, 
Neural Network, XGBoost, and ensemble modeling techniques. It was developed as 
part of the MS in Data Analytics program at Southern New Hampshire University.

## 📦 Project Overview

- **Goal**: Predict monthly customer revenue using behavioral, usage, and 
service data
- **Techniques**: Feature engineering, stratified modeling, ensemble learning, 
bootstrapped evaluation
- **Tools**: R, caret, xgboost, neuralnet, tidyverse, ggplot2

## 🧠 CRISP-DM Workflow

1. **Business Understanding**: Support CRM and marketing teams in identifying 
high-value customers
2. **Data Understanding**: Raw data includes usage metrics, service calls, 
overage, churn indicators
3. **Data Preparation**: Feature engineering, log transforms, interaction terms,
PCA, quartile stratification
4. **Modeling**: Elastic Net, Neural Net, XGBoost, ensemble 
average/weighted/stacked, stratified XGBoost
5. **Evaluation**: RMSE, MAE, R², bootstrapped confidence intervals, 
lift/gain curves, residual diagnostics
6. **Deployment**: Final model saved as `.rds`; scoring function implemented for
new data

## 📈 Results Summary

| Model               | RMSE  | RMSE CI (Lower–Upper) | MAE   | R²    | R² CI (Lower–Upper) |
|--------------------|-------|------------------------|-------|-------|----------------------|
| Elastic Net        | 37.8  | 34.4 – 40.8            | 31.8  | 0.811 | 0.760 – 0.853        |
| Neural Network     | 83.4  | 66.9 – 98.7            | 58.2  | 0.396 | 0.272 – 0.527        |
| XGBoost            | 37.8  | 34.2 – 41.8            | 30.5  | 0.810 | 0.758 – 0.854        |
| Ensemble Average   | 36.7  | 33.4 – 40.3            | 30.6  | 0.821 | 0.774 – 0.861        |
| Ensemble Weighted  | 36.7  | 33.2 – 40.1            | 30.4  | 0.821 | 0.775 – 0.864        |
| Ensemble Stacked   | 38.4  | 34.8 – 42.1            | 30.7  | 0.805 | 0.749 – 0.852        |
| Stratified XGBoost | 19.7  | 17.7 – 21.9            | 16.0  | 0.949 | 0.935 – 0.962        |

> Stratified XGBoost outperformed all other models, achieving the lowest RMSE 
and highest R² with tight confidence intervals, indicating strong predictive 
stability.

## 📊 Generated Figures

This project automatically saves key visualizations to `outputs/figures/` 
during script execution. These include:

- Residual plots for all models
- Fit plots (Actual vs Predicted) for Elastic Net, XGBoost, Neural Net, 
Ensemble, and Stratified models
- Gain and Lift charts for XGBoost and ensemble comparisons
- PCA variance and loadings plots
- Prediction density on verification data

These figures support stakeholder reporting, model diagnostics, and CRM 
dashboard integration.

## 🚀 How to Run

1. Clone the repository and set your working directory
2. Ensure required R packages are installed (see `requirements.txt`)
3. Run `scripts/customer_revenue_capstone.R` from top to bottom
4. Outputs will be saved in `/outputs/reports/` and `/outputs/figures/`

## 📊 Stakeholder Impact

- Enables CRM teams to prioritize retention for high-revenue customers
- Lift charts and gain curves support targeted marketing strategies
- Scoring function allows real-time prediction on new customer data

## 📁 Repository Structure
```
Customer-Revenue-Prediction-Capstone-Project/ 
├── data/ 
│ └── raw/ # Original data files 
├── scripts/ 
│ └── customer_revenue_capstone.R 
├── outputs/ 
│ ├── reports/ # Model metrics and predictions 
│ └── figures/ # Residuals, lift/gain plots 
├── README.md 
├── requirements.txt 
└── .gitignore
```

## 📄 License

This project is licensed under the **Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)** license.

You are free to:
- Share and adapt the materials for non-commercial purposes
- Credit the author and link to the license
- Distribute any derivative work under the same license

For full license details, visit [Creative Commons License](https://creativecommons.org/licenses/by-nc-sa/4.0/).


## 👤 Author

**Bill R. Wathier**  
Graduate Student, Data Analytics  
Southern New Hampshire University  
📧 [billrwathier@yahoo.com](mailto:billrwathier@yahoo.com)  
🔗 [LinkedIn Profile](https://www.linkedin.com/in/billwathier)


