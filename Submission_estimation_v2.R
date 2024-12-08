## This script does the foloowing:
# Estimate ATT and Robust Standard Errors: For each outcome and alpha value, estimate the ATT and robust standard errors.

# Heterogeneity Analysis: Extract the coefficients from regression model for each outcome variable.

# Compile Results: Combine all results into a summary table.

library(dplyr)
library(glmnet)
library(sandwich)
library(lmtest)

# Function to estimate ATT, its robust standard errors, and extract coefficients
estimate_ATT_with_robust_se_and_heterogeneity <- function(data, outcome_var, treatment_var, alpha = 1) {
  # Calculate the first difference of the outcome variable and replace NA with 0
  data <- data %>%
    group_by(id) %>%
    arrange(year) %>%
    mutate(del_y = get(outcome_var) - lag(get(outcome_var))) %>%
    mutate(del_y = ifelse(is.na(del_y), 0, del_y)) %>%
    filter(year == 2020)
  
  # Extract the outcome variable
  Y <- data$del_y
  
  # Create covariate matrix X (removing outcome variable and treatment indicator)
  X <- data %>% select(-all_of(outcome_var), -all_of(treatment_var), -del_y)
  X_matrix <- model.matrix(~ .^2, data = X) # Pairwise interaction terms
  
  # Define treatment indicator
  D <- data[[treatment_var]]  # Extract the treatment variable column as a vector
  
  # Compute double LASSO estimator of the ATE
  
  # Find subsample of control individuals
  idx_control <- which(D == 0)
  
  # Estimate gamma_del_Y using LASSO
  lasso_gamma_del_Y <- cv.glmnet(X_matrix[idx_control, ], Y[idx_control], alpha = alpha)
  gamma_del_Y_hat <- predict(lasso_gamma_del_Y, s = "lambda.min", newx = X_matrix)
  
  # Estimate gamma_p using LASSO
  lasso_gamma_p_hat <- cv.glmnet(X_matrix, D, alpha = alpha)
  gamma_p_hat <- predict(lasso_gamma_p_hat, s = "lambda.min", newx = X_matrix)
  
  # Estimate E(D)
  pd <- mean(D)
  
  # Compute the ATT
  ATT <- mean((Y - gamma_del_Y_hat) * (D - gamma_p_hat) / (pd * (1 - gamma_p_hat)))
  
  # Compute residuals
  residuals <- (Y - gamma_del_Y_hat) * (D - gamma_p_hat) / (pd * (1 - gamma_p_hat)) - ATT
  
  # Calculate robust standard errors
  robust_se <- sqrt(vcovHC(lm(residuals ~ 1), type = "HC")[1, 1])
  
  # Extract coefficients for heterogeneity analysis
  coef_gamma_del_Y <- coef(lasso_gamma_del_Y, s = "lambda.min")
  coef_gamma_del_Y_df <- as.data.frame(as.matrix(coef_gamma_del_Y))
  colnames(coef_gamma_del_Y_df) <- c("Coefficient")
  
  return(list(ATT_estimate = ATT, robust_se = robust_se, heterogeneity = coef_gamma_del_Y_df))
}

# Define a wrapper function to apply to multiple outcomes and different alphas
apply_estimate_ATT_with_different_alphas <- function(data, outcomes, treatment_var, alphas) {
  all_results <- list()
  summary_table <- data.frame(
    Outcome = character(),
    Alpha = numeric(),
    ATT_Estimate = numeric(),
    Robust_SE = numeric()
  )
  
  for (alpha in alphas) {
    cat("Running analysis with alpha = ", alpha, "\n")
    for (outcome in outcomes) {
      res <- estimate_ATT_with_robust_se_and_heterogeneity(data, outcome, treatment_var, alpha)
      all_results[[paste0(outcome, "_alpha_", alpha)]] <- res
      
      # Append results to summary table
      summary_table <- rbind(summary_table, data.frame(
        Outcome = outcome,
        Alpha = alpha,
        ATT_Estimate = res$ATT_estimate,
        Robust_SE = res$robust_se
      ))
    }
  }
  
  return(list(all_results = all_results, summary_table = summary_table))
}

# Load dataset for further analysis
load("~/Courses/ML_Econ/Exam/data_v2.RData")

# Define outcomes and treatment variable
outcomes <- c("income", "employed", "fulltime", "job_satisfaction")
treatment_var <- "treat"

# Define alphas to test
alphas <- c(0, 0.5, 1)

# Apply the function to all outcomes and different alphas
results <- apply_estimate_ATT_with_different_alphas(data, outcomes, treatment_var, alphas)

# Print the summary table
print(results$summary_table)

# Extending summary table with statistical significance:
summary_table <- results$summary_table %>% mutate(t_val = ATT_Estimate/Robust_SE)
summary_table

# Save the results (optional)
save(results, file = "all_results_all_outcomes.RData")

# Taking a closer look at the heterogeneity analysis ####
# Income: 
# Initialize an empty data frame to store the combined results
income_hetero_TE <- data.frame()

# Extract and combine heterogeneity results for each alpha
for (alpha in c(0, 0.5, 1)) {
  # Access the heterogeneity data frame for the given alpha
  heterogeneity_df <- results[["all_results"]][[paste0("income_alpha_", alpha)]][["heterogeneity"]]
  
  # Add the alpha value as a new column
  heterogeneity_df$Alpha <- alpha
  
  # Combine results
  income_hetero_TE <- rbind(income_hetero_TE, heterogeneity_df)
}
# Print the combined results
income_hetero_TE

income_hetero_TE %>% arrange(desc(abs(Coefficient)))

# Employment:
# Initialize an empty data frame to store the combined results
employed_hetero_TE <- data.frame()

# Extract and combine heterogeneity results for each alpha
for (alpha in c(0, 0.5, 1)) {
  # Access the heterogeneity data frame for the given alpha
  heterogeneity_df <- results[["all_results"]][[paste0("employed_alpha_", alpha)]][["heterogeneity"]]
  
  # Add the alpha value as a new column
  heterogeneity_df$Alpha <- alpha
  
  # Combine results
  employed_hetero_TE <- rbind(employed_hetero_TE, heterogeneity_df)
}
# Print the combined results
employed_hetero_TE

employed_hetero_TE %>% arrange(desc(abs(Coefficient)))

# Fulltime:
# Initialize an empty data frame to store the combined results
fulltime_hetero_TE <- data.frame()

# Extract and combine heterogeneity results for each alpha
for (alpha in c(0, 0.5, 1)) {
  # Access the heterogeneity data frame for the given alpha
  heterogeneity_df <- results[["all_results"]][[paste0("fulltime_alpha_", alpha)]][["heterogeneity"]]
  
  # Add the alpha value as a new column
  heterogeneity_df$Alpha <- alpha
  
  # Combine results
  fulltime_hetero_TE <- rbind(fulltime_hetero_TE, heterogeneity_df)
}
# Print the combined results
fulltime_hetero_TE

fulltime_hetero_TE %>% arrange(desc(abs(Coefficient)))

# job_satisfaction: 
# Initialize an empty data frame to store the combined results
job_satisfaction_hetero_TE <- data.frame()

# Extract and combine heterogeneity results for each alpha
for (alpha in c(0, 0.5, 1)) {
  # Access the heterogeneity data frame for the given alpha
  heterogeneity_df <- results[["all_results"]][[paste0("job_satisfaction_alpha_", alpha)]][["heterogeneity"]]
  
  # Add the alpha value as a new column
  heterogeneity_df$Alpha <- alpha
  
  # Combine results
  job_satisfaction_hetero_TE <- rbind(job_satisfaction_hetero_TE, heterogeneity_df)
}
# Print the combined results
job_satisfaction_hetero_TE

job_satisfaction_hetero_TE %>% arrange(desc(abs(Coefficient)))
