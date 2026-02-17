# server/server_factory.R

make_server <- function(site_csv_path) {
  # Load site-specific data (server keeps raw data private)
  df_site <- read.csv(site_csv_path)
  
  # ---- server-side functions (API) ----
  server_summary <- function(var) {
    if (!var %in% names(df_site)) stop("Variable not found on server.")
    x <- df_site[[var]]
    
    if (is.numeric(x)) {
      list(mean = mean(x, na.rm = TRUE),
           sd   = sd(x, na.rm = TRUE),
           n    = sum(!is.na(x)))
    } else {
      table(x, useNA = "ifany")
    }
  }
  
  server_logistic <- function(formula_str) {
    # Note: server runs model internally; client never sees df_site
    f <- as.formula(formula_str)
    fit <- glm(f, data = df_site, family = binomial)
    coefs <- summary(fit)$coefficients
    list(
      coefficients = coefs,
      odds_ratios  = exp(coef(fit)),
      n            = nrow(df_site)
    )
  }
  
  # Return an object with "endpoints"
  list(
    summary  = server_summary,
    logistic = server_logistic
  )
}