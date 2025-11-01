# DAT 690 GE Customer Retention Based on Revenue Prediction
# Author: Bill R. Wathier
# Southern New Hampshire University  
# DAT 690: Capstone in Data Analytics
#===========================================================================#

# MY PERSONAL SETUP: prompt option and working directory
options(prompt = "R> ") 
setwd("D:/GitHubProjects/Customer-Revenue-Prediction-Capstone-Project")

# =============================================================== #
# ========== Customer Revenue Prediction Project ================ #
# =============================================================== #
# CRISP-DM Step 1: Business Understanding 
# Goal: Predict monthly customer revenue using usage, service, and engagement 
# metrics

# Possible Stakeholders: Marketing/Analytics Manager, Revenue Ops Team, 
# CRM Integration Lead
# Success Metrics: R² ≥ 0.65 and RMSE ≤ $15 on hold-out data

# ======================================================================= #
# CRISP-DM Step 2: Data Understanding 
# Note: CRISP-DM Step 3 (Data Preparation) will be interwoven here and 
# during EDA

# Installing Required Packages - Keep adding as the project progresses
required_packages <- c("tidyverse", "skimr", "janitor", "DataExplorer", 
                       "GGally", "corrplot", "caret", "factoextra", 
                       "neuralnet", "xgboost")
new_packages <- required_packages[!(required_packages %in% 
                                      installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries for installed packages
library(tidyverse)      # Core data manipulation and visualization
library(skimr)          # Compact data summary
library(janitor)        # Clean column names
library(DataExplorer)   # Automated EDA
library(GGally)         # Pairwise plots
library(corrplot)       # Correlation matrix visualization
library(caret)          # Feature pruning and modeling prep
library(factoextra)     # PCA visualization
library(glmnet)         # Elastic net regression
library(Metrics)        # RMSE and MAE
library(rsample)        # Resampling utilities
library(neuralnet)      # Neural network library
library(xgboost)        # Gradient boosting model

# ======================================================================= #
# Configuration Block – Centralized Parameters for Reproducibility & Control
# ======================================================================= #

config <- list(
  seed = 690,
  success_r2 = 0.65,
  success_rmse = 15,
  quartile_models = c("enet", "xgb", "xgb", "xgb"),
  nn_hidden_layers = c(5, 3),
  nn_quartile_layers = c(10, 5),
  xgb_params = list(
    nrounds = 100,
    max_depth = 6,
    eta = 0.1,
    subsample = 1.0,
    colsample_bytree = 1.0,
    early_stopping_rounds = 10
  ),
  ensemble_weights = list(weighted = c(xgb = 0.6, enet = 0.4)),
  min_train = NULL,
  max_train = NULL
)


# Load and clean raw data file
revenue_data_raw <- read_csv("data/raw/CustomerRevenue_Data.csv") %>% 
  clean_names()

# Defining a modular EDA function
run_eda <- function(df, target_var = "revenue", segment_var = NULL) {
  
  original_df <- df  # Preserve original for plotting
  
  # Initial profiling
  cat("\n--- Initial Profiling ---\n")
  skim(df)
  glimpse(df)
  summary(df)
  
  # Missingness audit
  cat("\n--- Missingness Audit ---\n")
  missing_pct <- df %>% summarise(across(everything(), ~mean(is.na(.)) * 100))
  print(missing_pct)
  
  # Flag missingness as binary features
  df <- df %>% mutate(across(where(is.numeric), ~as.integer(is.na(.)), .names = "missing_{.col}"))
  
  # Categorical variable profiling
  cat("\n--- Categorical Variable Profiling ---\n")
  cat_vars <- df %>% select(where(is.character), where(is.factor)) %>% names()
  
  for (var in cat_vars) {
    cat("\nFrequency table for:", var, "\n")
    print(df %>% count(.data[[var]], sort = TRUE))
    
    ggplot(df, aes(x = .data[[var]])) +
      geom_bar(fill = "steelblue") +
      labs(title = paste("Distribution of", var), x = var, y = "Count") +
      theme_minimal() -> p
    print(p)
  }
  
  # Feature engineering validation
  cat("\n--- Feature Engineering Validation ---\n")
  if ("custcare_per_month" %in% names(df)) {
    ggplot(df, aes(x = custcare_per_month, y = .data[[target_var]])) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", color = "blue") +
      labs(title = "Custcare per Month vs Revenue") -> p1
    print(p1)
  }
  
  if ("data_gb_per_month" %in% names(df)) {
    ggplot(df, aes(x = data_gb_per_month, y = .data[[target_var]])) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", color = "darkgreen") +
      labs(title = "Data GB per Month vs Revenue") -> p2
    print(p2)
  }
  
  # Correlation matrix
  cat("\n--- Correlation Matrix ---\n")
  numeric_vars <- df %>% select(where(is.numeric)) %>%
    select(where(~ sd(.x, na.rm = TRUE) > 0))
  cor_matrix <- cor(numeric_vars, use = "complete.obs")
  corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7)
  
  # Distribution and outlier detection
  cat("\n--- Distribution & Outliers ---\n")
  plot_histogram(original_df)
  if (target_var %in% names(original_df)) {
    plot_boxplot(original_df, by = target_var)
  } else {
    cat(paste0("Target variable '", target_var, "' not found in original_df. Skipping boxplot.\n"))
  }
  
  # Segment-based summary (if applicable)
  if (!is.null(segment_var)) {
    cat("\n--- Segment-Based Summary ---\n")
    df %>%
      group_by(.data[[segment_var]]) %>%
      summarise(
        MeanRevenue = mean(.data[[target_var]], na.rm = TRUE),
        SDRevenue = sd(.data[[target_var]], na.rm = TRUE),
        Count = n()
      ) %>%
      print()
  }
  
  return(df)
}

# Defining a modular preprocessing function
prep_data <- function(df, reference_cols = NULL) {
  
  df <- df %>% clean_names()
  df <- df %>% select(-any_of(c("customer", "phone_number")))
  df <- df %>%
    mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))
  
  df <- df %>%
    mutate(
      custcare_per_month = custcare / months,
      service_calls_per_month = customer_service_calls / months,
      data_gb_per_month = data_usage_gb / months,
      mou_per_phone = mou / phones,
      roam_per_phone = roam / phones,
      overage_log = log1p(overage),
      roam_log = log1p(roam),
      changem_log = log1p(abs(changem)),
      data_usage_log = log1p(data_usage_gb),
      mou_data_interaction = mou * data_usage_gb,
      changem_squared = changem^2,
      roam_overage_interaction = roam * overage,
      phones_squared = phones^2
    )
  
  nzv <- nearZeroVar(df, saveMetrics = TRUE)
  df <- df[, !nzv$nzv]
  
  cor_matrix <- cor(df %>% select(where(is.numeric)), use = "complete.obs")
  high_corr <- findCorrelation(cor_matrix, cutoff = 0.9)
  df <- df[, -high_corr]
  
  if (!is.null(reference_cols)) {
    df <- df[, intersect(reference_cols, names(df))]
  }
  
  return(df)
}

# Run EDA on raw data (non-destructive)
run_eda(revenue_data_raw, target_var = "revenue", segment_var = "competitive_package")

# Preprocess for modeling
revenue_data <- prep_data(revenue_data_raw)

# ======================================================================= #
# CRISP-DM Step 3: Data Preparation – Feature Engineering and Transformation
# ======================================================================= #
# Data is now preprocessed using prep_data()
# Proceed to train/test split and modeling

# Already dropped high-cardinality ID columns in prep_data() function

# Visualize missing data
plot_missing(revenue_data)

# Outlier detection: find extreme values using IQR
numeric_vars <- revenue_data %>% select(where(is.numeric))
iqr_outliers <- function(x) {
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  sum(x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr))
}
outlier_counts <- sapply(numeric_vars, iqr_outliers)
sort(outlier_counts, decreasing = TRUE)

# Correlation matrix for numeric predictors
cor_matrix <- cor(numeric_vars)
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7)

# Univariate numeric EDA: histograms
plot_histogram(revenue_data)

# Boxplots for skewed variables
plot_boxplot(revenue_data, by = "revenue", 
             geom_boxplot_args = list(outlier.colour = "red"))

# Pairwise scatterplots for top correlated features
ggpairs(revenue_data %>% select(revenue, mou, mourec, peakvce, 
                                data_usage_gb, competitive_package))

# ======================================================================= #
# CRISP-DM Step 3: Data Preparation – Transformations and Feature Pruning
# ======================================================================= #

# Log-transform skewed predictors (add 1 to avoid undefined log(0))
revenue_data <- revenue_data %>%
  mutate(
    overage_log = log1p(overage),
    roam_log = log1p(roam),
    changem_log = log1p(abs(changem)),  # handles negative values
    data_usage_log = log1p(data_usage_gb)
  )

# Remove near-zero variance predictors
nzv <- nearZeroVar(revenue_data, saveMetrics = TRUE)
revenue_data <- revenue_data[, !nzv$nzv]

# Remove highly correlated predictors (r > 0.9)
cor_matrix <- cor(revenue_data %>% select(where(is.numeric)))
high_corr <- findCorrelation(cor_matrix, cutoff = 0.9)
revenue_data <- revenue_data[, -high_corr]

# ======================================================================= #
# CRISP-DM Step 3: Data Preparation – PCA
# ======================================================================= #

# Create PCA object without altering main dataset
pca_input <- revenue_data %>% select(where(is.numeric))
pca_result <- prcomp(pca_input, center = TRUE, scale. = TRUE)

# Visualize variance explained
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50))

# Interpret PCA loadings: contribution of original variables to PC1 and PC2
loadings <- as_tibble(pca_result$rotation, rownames = "Feature")

ggplot(loadings, aes(x = PC1, y = PC2, label = Feature)) +
  geom_segment(aes(xend = 0, yend = 0), arrow = arrow(length = unit(0.2, "cm")),
               color = "gray") +
  geom_text(color = "blue", size = 3, hjust = 0.5, vjust = -0.5) +
  labs(title = "PCA Loadings: Contribution to PC1 and PC2",
       x = "PC1 Loading", y = "PC2 Loading") +
  theme_minimal()

# Print top contributors
loadings %>%
  mutate(Magnitude = sqrt(PC1^2 + PC2^2)) %>%
  arrange(desc(Magnitude)) %>%
  slice_head(n = 10)

# Extract PCA components for modeling
pca_components <- as_tibble(pca_result$x)
revenue_data_pca <- bind_cols(revenue = revenue_data$revenue, pca_components)

# PCA performed for exploratory purposes only; not used in final models

# ======================================================================= #
# CRISP-DM Step 3: Data Preparation – Train/Test Split
# ======================================================================= #

# Create revenue quartiles for stratified sampling
revenue_data <- revenue_data %>%
  mutate(revenue_quartile = ntile(revenue, 4))

# Add temporary row ID for join tracking
revenue_data <- revenue_data %>% 
  mutate(temp_id = row_number())

# Sample 80% from each quartile for training
set.seed(config$seed)  # For reproducibility
train_data <- revenue_data %>%
  group_by(revenue_quartile) %>%
  slice_sample(prop = 0.8) %>%
  ungroup()

# Remaining 20% for testing using anti_join on temp_id
test_data <- anti_join(revenue_data, train_data, by = "temp_id")

# Drop helper columns
train_data <- train_data %>% select(-revenue_quartile, -temp_id)
test_data  <- test_data %>% select(-revenue_quartile, -temp_id)

# Confirm split balance
summary(train_data$revenue)
summary(test_data$revenue)

# ======================================================================= #
# CRISP-DM Step 3: Data Preparation – Final Model Matrix Prep
# ======================================================================= #

# Separate predictors and target
predictors <- setdiff(names(train_data), "revenue")

model_matrix_train <- train_data %>% select(all_of(predictors))
target_train <- train_data$revenue
config$min_train <- min(target_train)
config$max_train <- max(target_train)

min_train <- min(target_train)
max_train <- max(target_train)

model_matrix_test <- test_data %>% select(all_of(predictors))
target_test <- test_data$revenue

# ====================================================================== #
# CRISP-DM Step 4: Modeling
# ====================================================================== #

# ====================================================================== #
# CRISP-DM Step 4: Modeling – Elastic Net Regression
# ====================================================================== #

# Convert predictors to matrix format for glmnet
x_train <- model_matrix_train %>% as.matrix()
x_test  <- model_matrix_test %>% as.matrix()

# Elastic Net – Grid Search over Alpha
set.seed(config$seed)
alpha_grid <- seq(0, 1, by = 0.25)
cv_results <- map_df(alpha_grid, function(a) {
  cv <- cv.glmnet(x_train, target_train, alpha = a, nfolds = 5)
  tibble(alpha = a, lambda_min = cv$lambda.min, rmse = min(cv$cvm))
})

# Select best alpha
best_alpha <- cv_results %>% filter(rmse == min(rmse)) %>% pull(alpha)
best_lambda <- cv_results %>% filter(rmse == min(rmse)) %>% pull(lambda_min)

# Fit final model with best alpha
elastic_net_model <- glmnet(x_train, target_train, alpha = best_alpha, 
                            lambda = best_lambda)
pred_enet <- predict(elastic_net_model, s = best_lambda, newx = x_test)

# ====================================================================== #
# Elastic Net – Feature Importance
# ====================================================================== #

# Extract coefficients and name the column explicitly
coef_matrix <- as.matrix(coef(elastic_net_model))
coef_df <- tibble(
  Feature = rownames(coef_matrix),
  Coefficient = coef_matrix[, 1]
) %>%
  filter(Coefficient != 0) %>%
  arrange(desc(abs(Coefficient)))

# View top features
print(coef_df)

# ====================================================================== #
# CRISP-DM Step 4: Modeling – Feedforward Neural Network 
# ====================================================================== #

# Normalize predictors for neural network
normalize <- function(x) {
  rng <- max(x) - min(x)
  if (rng == 0 || is.na(rng)) return(rep(0, length(x)))
  (x - min(x)) / rng
}
train_nn <- model_matrix_train %>% mutate(across(everything(), normalize))
test_nn  <- model_matrix_test %>% mutate(across(everything(), normalize))

# Combine predictors and target
train_nn <- bind_cols(train_nn, revenue = normalize(target_train))

# Build formula dynamically
nn_formula <- as.formula(paste("revenue ~", paste(predictors, 
                                                  collapse = " + ")))

# Train neural network
set.seed(config$seed)
nn_model <- neuralnet(nn_formula, data = train_nn, hidden = config$nn_hidden_layers, 
                      linear.output = TRUE)

# Predict on test set
pred_nn <- compute(nn_model, test_nn)$net.result
pred_nn <- pred_nn * (max_train - min_train) + min_train 
pred_nn <- pmax(pred_nn, 0)

# ====================================================================== #
# CRISP-DM Step 4: Modeling – XGBoost Regression
# ====================================================================== #

# Prepare data for XGBoost
xgb_train <- xgb.DMatrix(data = as.matrix(model_matrix_train), 
                         label = target_train)
xgb_test  <- xgb.DMatrix(data = as.matrix(model_matrix_test), 
                         label = target_test)

# Train XGBoost model
set.seed(config$seed)
xgb_model <- xgboost(
  data = xgb_train,
  objective = "reg:squarederror",
  nrounds = config$xgb_params$nrounds,
  max_depth = config$xgb_params$max_depth,
  eta = config$xgb_params$eta,
  subsample = config$xgb_params$subsample,
  colsample_bytree = config$xgb_params$colsample_bytree,
  early_stopping_rounds = config$xgb_params$early_stopping_rounds,
  verbose = 1
)

# Predict on test set
pred_xgb <- predict(xgb_model, xgb_test)

# ====================================================================== #
# CRISP-DM Step 4: Modeling – Stacked Ensemble
# ====================================================================== #

# Create training data for stacked ensemble meta-model
stack_train <- tibble(
  enet = predict(elastic_net_model, s = best_lambda, newx = x_train),
  xgb  = predict(xgb_model, xgb_train),
  revenue = target_train
)

# Train meta-model (stacked ensemble)
meta_model <- lm(revenue ~ enet + xgb, data = stack_train)

# Predict on test set using stacked ensemble
stack_test <- tibble(
  enet = pred_enet,
  xgb  = pred_xgb
)
pred_ensemble_stack <- predict(meta_model, newdata = stack_test)

# Generate ensemble predictions
pred_ensemble_avg      <- (pred_enet + pred_xgb) / 2
weights <- config$ensemble_weights$weighted
pred_ensemble_weighted <- weights["xgb"] * pred_xgb + weights["enet"] * pred_enet

# ====================================================================== #
# XGBoost – Feature Importance
# ====================================================================== #

# Extract feature importance
importance_matrix <- xgb.importance(model = xgb_model)

# View top features
print(importance_matrix)

# Plot importance
xgb.plot.importance(importance_matrix, top_n = 10, measure = "Gain")

# ====================================================================== #
# CRISP-DM Step 4: Modeling – Stratified Models by Revenue Quartile
# ====================================================================== #

# Segment Data by Revenue Quartile
train_quartiles <- train_data %>% mutate(quartile = ntile(revenue, 4))
test_quartiles  <- test_data  %>% mutate(quartile = ntile(revenue, 4))

# Define Modular Training Function
train_quartile_model <- function(data, model_type = "xgb") {
  predictors <- setdiff(names(data), c("revenue", "quartile"))
  x <- data %>% select(all_of(predictors)) %>% as.matrix()
  y <- data$revenue
  
  if (model_type == "xgb") {
    xgb_train <- xgb.DMatrix(data = x, label = y)
    xgb_model <- xgboost(
      data = xgb_train,
      objective = "reg:squarederror",
      nrounds = 100,
      max_depth = 4,
      eta = 0.1,
      verbose = 0
    )
    return(xgb_model)
  }
  
  if (model_type == "enet") {
    cv <- cv.glmnet(x, y, alpha = 1, nfolds = 10)
    enet_model <- glmnet(x, y, alpha = 1, lambda = cv$lambda.min)
    return(enet_model)
  }
  
  if (model_type == "nn") {
    normalize <- function(x) {
      rng <- max(x) - min(x)
      if (rng == 0 || is.na(rng)) return(rep(0, length(x)))
      (x - min(x)) / rng
    }
    
    x_nn <- data %>% select(all_of(predictors)) %>% mutate(across(everything(),
                                                                  normalize))
    y_nn <- normalize(y)
    nn_data <- bind_cols(x_nn, revenue = y_nn)
    nn_formula <- as.formula(paste("revenue ~", paste(predictors, 
                                                      collapse = " + ")))
    nn_model <- neuralnet(nn_formula, data = nn_data, hidden = config$nn_quartile_layers, 
                          linear.output = TRUE)
    return(nn_model)
  }
}

# Assign model types per quartile
model_types <- config$quartile_models  # Customize as needed

# Train Models for Each Quartile
quartile_models <- map2(1:4, model_types, function(q, mtype) {
  data_q <- train_quartiles %>% filter(quartile == q)
  train_quartile_model(data_q, model_type = mtype)
})

# Define Model-Aware Prediction Function
predict_quartile <- function(model, data, model_type) {
  x <- data %>% select(all_of(predictors)) %>% as.matrix()
  
  if (model_type == "xgb") {
    pred <- predict(model, xgb.DMatrix(data = x))
    return(pmax(pred, 0))  # elliminate negatives
  } else if (model_type == "enet") {
    pred <- predict(model, newx = x)
    return(pmax(pred, 0))  # elliminate negatives
  } else if (model_type == "nn") {
    normalize <- function(x) {
      rng <- max(x) - min(x)
      if (rng == 0 || is.na(rng)) return(rep(0, length(x)))
      (x - min(x)) / rng
    }
    x_nn <- data %>% select(all_of(predictors)) %>% mutate(across(everything(), 
                                                                  normalize))
    pred <- compute(model, x_nn)$net.result
    pred <- pred * (max_train - min_train) + min_train  # Use stored min/max
    return(pmax(pred, 0))  # elliminate negatives
  }
}

# Predict on Test Set by Quartile
test_predictions <- map2(1:4, model_types, function(q, mtype) {
  model <- quartile_models[[q]]
  data_q <- test_quartiles %>% filter(quartile == q)
  preds <- predict_quartile(model, data_q, mtype)
  tibble(actual = data_q$revenue, predicted = preds, quartile = q)
})

# Combine All Predictions
stratified_results <- bind_rows(test_predictions)

# ====================================================================== #
# CRISP-DM Step 5: Evaluation
# ====================================================================== #

# ====================================================================== #
# Utility Function – Model Performance Evaluation
# ====================================================================== #

evaluate_model <- function(actual, predicted, model_name = "Model", n_boot = 1000) {
  rmse_val <- rmse(actual, predicted)
  mae_val  <- mae(actual, predicted)
  r2_val   <- cor(actual, predicted)^2
  
  # Bootstrap confidence intervals
  boot_metrics <- replicate(n_boot, {
    idx <- sample(seq_along(actual), replace = TRUE)
    rmse(actual[idx], predicted[idx])
  })
  rmse_ci <- quantile(boot_metrics, probs = c(0.025, 0.975))
  
  boot_r2 <- replicate(n_boot, {
    idx <- sample(seq_along(actual), replace = TRUE)
    cor(actual[idx], predicted[idx])^2
  })
  r2_ci <- quantile(boot_r2, probs = c(0.025, 0.975))
  
  cat(paste0(model_name, " Performance:\n"))
  cat("RMSE:", round(rmse_val, 2), " [95% CI: ", round(rmse_ci[1], 2), "–", round(rmse_ci[2], 2), "]\n")
  cat("MAE:", round(mae_val, 2), "\n")
  cat("R²:", round(r2_val, 3), " [95% CI: ", round(r2_ci[1], 3), "–", round(r2_ci[2], 3), "]\n")
  
  return(tibble(
    Model = model_name,
    RMSE = round(rmse_val, 2),
    RMSE_Lower = round(rmse_ci[1], 2),
    RMSE_Upper = round(rmse_ci[2], 2),
    MAE = round(mae_val, 2),
    R2 = round(r2_val, 3),
    R2_Lower = round(r2_ci[1], 3),
    R2_Upper = round(r2_ci[2], 3)
  ))
}

# Re-run All Model Evaluation Calls
results_enet <- evaluate_model(target_test, pred_enet, "Elastic Net")
results_nn   <- evaluate_model(target_test, pred_nn, "Neural Network")
results_xgb  <- evaluate_model(target_test, pred_xgb, "XGBoost")
results_ensemble_avg <- evaluate_model(target_test, pred_ensemble_avg, "Ensemble Average")
results_ensemble_weighted <- evaluate_model(target_test, pred_ensemble_weighted, "Ensemble Weighted")
results_ensemble_stack <- evaluate_model(target_test, pred_ensemble_stack, "Ensemble Stacked")
overall_eval <- evaluate_model(stratified_results$actual, stratified_results$predicted, "Stratified XGBoost")

# ====================================================================== #
# CRISP-DM Step 5: Evaluation – Load Verification Data
# ====================================================================== #

# Load verification dataset
verify_data <- read_csv("data/raw/CustomerRevenue_Verify.csv") %>% clean_names()

# Drop high-cardinality ID columns
verify_data <- verify_data %>% select(-customer, -phone_number)

# Impute missing numeric values with median
verify_data <- verify_data %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), 
                                           .)))

# Apply same feature engineering as training data
verify_data <- verify_data %>%
  mutate(
    custcare_per_month = custcare / months,
    service_calls_per_month = customer_service_calls / months,
    data_gb_per_month = data_usage_gb / months,
    mou_per_phone = mou / phones,
    roam_per_phone = roam / phones,
    overage_log = log1p(overage),
    roam_log = log1p(roam),
    changem_log = log1p(abs(changem)),
    data_usage_log = log1p(data_usage_gb)
  )

# Add interaction terms and polynomial features
verify_data <- verify_data %>%
  mutate(
    mou_data_interaction = mou * data_usage_gb,
    changem_squared = changem^2,
    roam_overage_interaction = roam * overage,
    phones_squared = phones^2
  )

# Remove near-zero variance and highly correlated predictors
# Align columns with training set
verify_data <- verify_data[, names(model_matrix_train)]  

# Predict using Elastic Net
verify_matrix <- verify_data %>% as.matrix()
pred_verify_enet <- predict(elastic_net_model, s = best_lambda, 
                            newx = verify_matrix)

# Predict using Neural Network
verify_nn <- verify_data %>% mutate(across(everything(), normalize))
pred_verify_nn <- compute(nn_model, verify_nn)$net.result
pred_verify_nn <- pred_verify_nn * (max(target_train) - min(target_train)) 
+ min(target_train)

# Predict using XGBoost
verify_xgb <- xgb.DMatrix(data = as.matrix(verify_data))
pred_verify_xgb <- predict(xgb_model, verify_xgb)

# Combine predictions from all models
verify_results <- tibble(
  ElasticNet = as.vector(pred_verify_enet),
  NeuralNet  = as.vector(pred_verify_nn),
  XGBoost    = as.vector(pred_verify_xgb)
)

# Predict using Ensemble Stacked
verify_stack <- predict(meta_model, newdata = tibble(
  enet = pred_verify_enet,
  xgb = pred_verify_xgb
))

# Predict using Stratified XGBoost
verify_quartile <- ntile(pred_verify_xgb, 4)
verify_stratified <- map_dbl(seq_len(nrow(verify_data)), function(i) {
  q <- verify_quartile[i]
  model <- quartile_models[[q]]
  data_row <- verify_data[i, , drop = FALSE]
  predict_quartile(model, data_row, model_types[q])
})

# Add to verification results
verify_results <- verify_results %>%
  mutate(
    EnsembleStacked = verify_stack,
    StratifiedXGBoost = verify_stratified
  )

# Visualize prediction distributions
verify_results %>%
  pivot_longer(cols = everything(), names_to = "Model", 
               values_to = "PredictedRevenue") %>%
  ggplot(aes(x = PredictedRevenue, fill = Model)) +
  geom_density(alpha = 0.5) +
  labs(title = "Model Predictions on Verification Data", 
       x = "Predicted Revenue", y = "Density") +
  theme_minimal()

# ====================================================================== #
# CRISP-DM Step 5: Evaluation – Model Comparison Summary
# ====================================================================== #

# Create summary table of test set metrics
model_comparison <- bind_rows(
  results_enet,
  results_nn,
  results_xgb,
  results_ensemble_avg,
  results_ensemble_weighted,
  results_ensemble_stack,
  overall_eval
)
print(model_comparison)

# need to flatten R2 from a matrix to a vector.
model_comparison <- model_comparison %>%
  mutate(R2 = as.numeric(R2))

# Export model comparison to CSV and RDS
write_csv(model_comparison, "outputs/reports/model_comparison_summary.csv")
saveRDS(model_comparison, "outputs/reports/model_comparison_summary.rds")

# ====================================================================== #
# CRISP-DM Step 5: Evaluation – Residual and Fit Diagnostics
# ====================================================================== #

# Create residuals
residuals_enet <- target_test - pred_enet
residuals_xgb  <- target_test - pred_xgb
residuals_nn   <- target_test - pred_nn

# Residual plots
par(mfrow = c(1, 3))
plot(pred_enet, residuals_enet, main = "Elastic Net Residuals",
     xlab = "Predicted Revenue", ylab = "Residuals", col = "blue", pch = 16)
abline(h = 0, col = "red", lty = 2)

plot(pred_xgb, residuals_xgb, main = "XGBoost Residuals",
     xlab = "Predicted Revenue", ylab = "Residuals", col = "darkgreen", 
     pch = 16)
abline(h = 0, col = "red", lty = 2)

plot(pred_nn, residuals_nn, main = "Neural Net Residuals",
     xlab = "Predicted Revenue", ylab = "Residuals", col = "purple", pch = 16)
abline(h = 0, col = "red", lty = 2)

# Residuals for Stratified and Ensemble Stacked
residuals_strat <- stratified_results$actual - stratified_results$predicted
residuals_stack <- target_test - pred_ensemble_stack

# Residual plots for Stratified and Ensemble Stacked
par(mfrow = c(1, 2))
plot(stratified_results$predicted, residuals_strat, 
     main = "Stratified XGBoost Residuals",
     xlab = "Predicted Revenue", ylab = "Residuals", col = "orange", pch = 16)
abline(h = 0, col = "red", lty = 2)

plot(pred_ensemble_stack, residuals_stack, main = "Ensemble Stacked Residuals",
     xlab = "Predicted Revenue", ylab = "Residuals", col = "brown", pch = 16)
abline(h = 0, col = "red", lty = 2)

# Fit plots for Stratified and Ensemble Stacked
par(mfrow = c(1, 2))
plot(stratified_results$actual, stratified_results$predicted, 
     main = "Stratified XGBoost Fit",
     xlab = "Actual Revenue", ylab = "Predicted Revenue", col = "orange", 
     pch = 16)
abline(0, 1, col = "black", lty = 2)

plot(target_test, pred_ensemble_stack, main = "Ensemble Stacked Fit",
     xlab = "Actual Revenue", ylab = "Predicted Revenue", col = "brown", 
     pch = 16)
abline(0, 1, col = "black", lty = 2)

# Prediction vs. Actual scatterplots
par(mfrow = c(2, 3))  # 2 rows, 3 columns for 6 models

plot(target_test, pred_enet, main = "Elastic Net Fit",
     xlab = "Actual Revenue", ylab = "Predicted Revenue", col = "blue", 
     pch = 16)
abline(0, 1, col = "black", lty = 2)

plot(target_test, pred_xgb, main = "XGBoost Fit",
     xlab = "Actual Revenue", ylab = "Predicted Revenue", col = "darkgreen", 
     pch = 16)
abline(0, 1, col = "black", lty = 2)

plot(target_test, pred_nn, main = "Neural Net Fit",
     xlab = "Actual Revenue", ylab = "Predicted Revenue", col = "purple", 
     pch = 16)
abline(0, 1, col = "black", lty = 2)

plot(stratified_results$actual, stratified_results$predicted, 
     main = "Stratified XGBoost Fit",
     xlab = "Actual Revenue", ylab = "Predicted Revenue", col = "orange", 
     pch = 16)
abline(0, 1, col = "black", lty = 2)

plot(target_test, pred_ensemble_stack, main = "Ensemble Stacked Fit",
     xlab = "Actual Revenue", ylab = "Predicted Revenue", col = "brown", 
     pch = 16)
abline(0, 1, col = "black", lty = 2)

# Highlight best model based on success criteria
r2_enet <- results_enet$R2
rmse_enet <- results_enet$RMSE
r2_nn <- results_nn$R2
rmse_nn <- results_nn$RMSE
r2_xgb <- results_xgb$R2
rmse_xgb <- results_xgb$RMSE
rmse_stack <- results_ensemble_stack$RMSE
r2_stack   <- results_ensemble_stack$R2
rmse_strat <- overall_eval$RMSE
r2_strat   <- overall_eval$R2

if (r2_enet >= config$success_r2 & rmse_enet <= config$success_rmse) {
  final_model <- "Elastic Net"
} else if (r2_nn >= config$success_r2 & rmse_nn <= config$success_rmse) {
  final_model <- "Neural Network"
} else if (r2_xgb >= config$success_r2 & rmse_xgb <= config$success_rmse) {
  final_model <- "XGBoost"
} else if (r2_stack >= config$success_r2 & rmse_stack <= config$success_rmse) {
  final_model <- "Ensemble Stacked"
} else if (r2_strat >= config$success_r2 & rmse_strat <= config$success_rmse) {
  final_model <- "Stratified XGBoost"
} else {
  final_model <- "None meet success criteria"
}

cat("\nSelected Model for Deployment:", final_model, "\n")

# ====================================================================== #
# CRISP-DM Step 5: Evaluation – Lift Chart and Gain Curve
# ====================================================================== #

# Prepare data for lift/gain analysis
df <- tibble(actual = target_test, predicted = pred_xgb) %>%
  arrange(desc(predicted)) %>%
  mutate(decile = ntile(row_number(), 10))

# Summarize by decile
decile_summary <- df %>%
  group_by(decile) %>%
  summarise(total_actual = sum(actual)) %>%
  arrange(decile) %>%
  mutate(
    cumulative_actual = cumsum(total_actual),
    percent_customers = decile * 10,
    random_gain = percent_customers / 100 * sum(df$actual),
    lift = cumulative_actual / random_gain
  )

# Gain Curve
ggplot(decile_summary, aes(x = percent_customers)) +
  geom_line(aes(y = cumulative_actual, color = "Model Gain"), size = 1.2) +
  geom_line(aes(y = random_gain, color = "Random Gain"), linetype = "dashed", size = 1) +
  labs(title = "Gain Curve – XGBoost Model",
       x = "Percent of Customers Targeted",
       y = "Cumulative Revenue Captured") +
  scale_color_manual(values = c("Model Gain" = "blue", "Random Gain" = "gray")) +
  theme_minimal()

# Lift Chart
ggplot(decile_summary, aes(x = decile, y = lift)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(title = "Lift Chart – XGBoost Model",
       x = "Decile",
       y = "Lift (Model Gain / Random Gain)") +
  theme_minimal()

# ====================================================================== #
# CRISP-DM Step 5: Evaluation – Lift & Gain Curve Comparison
# ====================================================================== #

# Prepare gain/lift data for each model
gain_data <- function(actual, predicted, model_name) {
  df <- tibble(actual = actual, predicted = predicted) %>%
    arrange(desc(predicted)) %>%
    mutate(decile = ntile(row_number(), 10)) %>%
    group_by(decile) %>%
    summarise(total_actual = sum(actual), .groups = "drop") %>%
    mutate(
      cumulative_actual = cumsum(total_actual),
      percent_customers = decile * 10,
      model = model_name,
      gain = cumulative_actual / sum(total_actual),
      lift = total_actual / (sum(total_actual) / 10)
    )
  return(df)
}

# Compute for each model
# Compute gain/lift data for all models
gain_enet      <- gain_data(target_test, pred_enet, "Elastic Net")
gain_xgb       <- gain_data(target_test, pred_xgb, "XGBoost")
gain_stack     <- gain_data(target_test, pred_ensemble_stack, "Ensemble Stacked")
gain_avg       <- gain_data(target_test, pred_ensemble_avg, "Ensemble Average")
gain_weighted  <- gain_data(target_test, pred_ensemble_weighted, "Ensemble Weighted")
gain_strat     <- gain_data(stratified_results$actual, stratified_results$predicted, "Stratified XGBoost")

gain_all <- bind_rows(
  gain_enet,
  gain_xgb,
  gain_stack,
  gain_avg,
  gain_weighted,
  gain_strat
)

# Stakeholder Summary: Revenue Captured in Top Decile
top_decile_summary <- gain_all %>%
  filter(decile == 10) %>%
  mutate(
    RevenueCaptured = round(cumulative_actual / sum(cumulative_actual) * 100, 1)
  ) %>%
  select(model, RevenueCaptured)

cat("\nStakeholder Summary – Revenue Captured in Top Decile:\n")
print(top_decile_summary)

# Export for stakeholder reporting
write_csv(top_decile_summary, "outputs/reports/top_decile_summary.csv")

# flatten list or matrix columns before exporting
model_comparison <- model_comparison %>%
  mutate(across(everything(), ~ unlist(.x)))

# Export gain/lift summary to CSV and RDS
write_csv(gain_all, "outputs/reports/gain_lift_summary.csv")
saveRDS(gain_all, "outputs/reports/gain_lift_summary.rds")

# Gain Curve
ggplot(gain_all, aes(x = percent_customers, y = gain, color = model)) +
  geom_line(size = 1.2) +
  labs(title = "Gain Curve Comparison",
       x = "Percent of Customers Targeted",
       y = "Cumulative Revenue Gain") +
  theme_minimal()

# Lift Chart
ggplot(gain_all, aes(x = decile, y = lift, color = model)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray") +
  labs(title = "Lift Chart Comparison",
       x = "Decile",
       y = "Lift (Model Gain / Random Gain)") +
  theme_minimal()

# ====================================================================== #
# CRISP-DM Step 6: Deployment
# ====================================================================== #

# ====================================================================== #
# CRISP-DM Step 6: Deployment – Save Final Model
# ====================================================================== #

# Save final model object
if (final_model == "Elastic Net") {
  saveRDS(elastic_net_model, "outputs/reports/final_model_elastic_net.rds")
  cat("Elastic Net model saved as final_model_elastic_net.rds\n")
} else if (final_model == "Neural Network") {
  saveRDS(nn_model, "outputs/reports/final_model_neural_net.rds")
  cat("Neural Network model saved as final_model_neural_net.rds\n")
} else if (final_model == "XGBoost") {
  saveRDS(xgb_model, "outputs/reports/final_model_xgboost.rds")
  cat("XGBoost model saved as final_model_xgboost.rds\n")
} else if (final_model == "Ensemble Stacked") {
  saveRDS(meta_model, "outputs/reports/final_model_ensemble_stack.rds")
  cat("Stacked ensemble model saved as final_model_ensemble_stack.rds\n")
} else if (final_model == "Stratified XGBoost") {
  walk2(quartile_models, 1:4, ~ saveRDS(.x, paste0("model_q", .y, ".rds")))
  cat("Stratified XGBoost models saved as model_q1.rds to model_q4.rds\n")
}

# ====================================================================== #
# CRISP-DM Step 6: Deployment – Scoring Function
# ====================================================================== #

# Define consistent normalization function using training min/max
normalize <- function(x, min_val, max_val) {
  rng <- max_val - min_val
  if (rng == 0 || is.na(rng)) return(rep(0, length(x)))
  (x - min_val) / rng
}

# Define scoring function for new data
score_new_data <- function(new_df) {
  # Log which model is being used
  cat("Scoring new data using", final_model, "model...\n")
  
  # Preprocess new data using your modular pipeline
  new_df <- prep_data(new_df, reference_cols = predictors)
  
  if (final_model == "Elastic Net") {
    new_matrix <- new_df %>% as.matrix()
    pred <- predict(elastic_net_model, s = best_lambda, newx = new_matrix)
    return(tibble(PredictedRevenue = as.vector(pred)))
    
  } else if (final_model == "Neural Network") {
    new_nn <- new_df %>% mutate(across(everything(), ~normalize(.x, min_train, 
                                                                max_train)))
    pred <- compute(nn_model, new_nn)$net.result
    pred <- pred * (max_train - min_train) + min_train
    return(tibble(PredictedRevenue = as.vector(pred)))
    
  } else if (final_model == "XGBoost") {
    new_matrix <- xgb.DMatrix(data = as.matrix(new_df))
    pred <- predict(xgb_model, new_matrix)
    return(tibble(PredictedRevenue = as.vector(pred)))
    
  } else if (final_model == "Ensemble Stacked") {
    new_preds <- tibble(
      enet = predict(elastic_net_model, s = best_lambda, newx = new_df %>% 
                       as.matrix()),
      xgb  = predict(xgb_model, xgb.DMatrix(data = as.matrix(new_df)))
    )
    pred <- predict(meta_model, new_preds)
    return(tibble(PredictedRevenue = as.vector(pred)))
    
  } else if (final_model == "Stratified XGBoost") {
    quartile <- ntile(predict(xgb_model, xgb.DMatrix(data = as.matrix(new_df))),
                      4)
    model <- readRDS(paste0("model_q", quartile, ".rds"))
    pred <- predict(model, xgb.DMatrix(data = as.matrix(new_df)))
    return(tibble(PredictedRevenue = as.vector(pred)))
    
  } else {
    stop("No valid model selected for scoring.")
  }
}
