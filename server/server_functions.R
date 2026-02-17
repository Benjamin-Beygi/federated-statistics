# =========================================================
# SERVER SIDE (data never leaves this environment)
# =========================================================

# Load data (relative path from server folder)
server_load_data <- function() {
  
  data_path <- file.path("server", "data", "demo_dataset.csv")
  
  if (!file.exists(data_path)) {
    stop("Dataset not found on server.")
  }
  
  df <- read.csv(data_path)
  
  # Ensure proper types
  df$sex <- factor(df$sex)
  df$x2  <- factor(df$x2)
  
  return(df)
}

# ---------------------------------------------------------
# 1. Secure summary statistics
# ---------------------------------------------------------

server_summary <- function(variable) {
  
  df <- server_load_data()
  
  if (!variable %in% names(df)) {
    stop("Variable not found.")
  }
  
  x <- df[[variable]]
  
  if (is.numeric(x)) {
    return(list(
      mean = mean(x),
      sd   = sd(x),
      n    = length(x)
    ))
  }
  
  if (is.factor(x)) {
    return(table(x))
  }
  
  stop("Unsupported variable type.")
}

# ---------------------------------------------------------
# 2. Secure logistic regression
# ---------------------------------------------------------

server_logistic <- function(formula_string) {
  
  df <- read.csv(file.path("server", "data", "demo_dataset.csv"))
  
  # Convert categorical variables properly
  df$sex <- factor(df$sex)
  df$x2  <- factor(df$x2)
  
  fit <- glm(as.formula(formula_string),
             data = df,
             family = binomial)
  
  coef_table <- summary(fit)$coefficients
  odds_ratios <- exp(coef(fit))
  
  # Return structured object
  return(list(
    coefficients = coef_table,
    odds_ratios = odds_ratios,
    n = nrow(df)
  ))
}