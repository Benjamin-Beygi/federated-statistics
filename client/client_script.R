# --------------------------------------------------
# CLIENT SIDE SCRIPT
# --------------------------------------------------

# Load server functions (simulating remote call)
source(file.path("server", "server_functions.R"))

cat("Client connected to server.\n\n")

# ----------------------------------
# Request summary statistics
# ----------------------------------

cat("Requesting summary of age...\n")
age_summary <- server_summary("age")
print(age_summary)

cat("\nRequesting summary of sex...\n")
sex_summary <- server_summary("sex")
print(sex_summary)

# ----------------------------------
# Request logistic regression
# ----------------------------------

cat("\nRequesting logistic regression...\n")
model_results <- server_logistic("y ~ age + sex + x1 + x2")

cat("\nReceived regression coefficients:\n")
print(model_results$coefficients)

cat("\nReceived odds ratios:\n")
print(model_results$odds_ratios)