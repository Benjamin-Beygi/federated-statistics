# client/client_fed_lr_http.R
# Federated Logistic Regression over HTTP (Plumber transport)
# Prints:
#  - Model-based inference
#  - Cluster-robust (hospital-clustered) inference
#  - Leave-One-Hospital-Out Jackknife inference (small-G robust)

source(file.path("client", "fed_engine.R"))
source(file.path("client", "http_server_adapter.R"))

cat("Client connecting over HTTP...\n")

# ---------------------------------------------------
# Define hospital endpoints
# ---------------------------------------------------

servers <- list(
  create_http_server("http://127.0.0.1:8001"),
  create_http_server("http://127.0.0.1:8002")
)

formula <- y ~ age + sex + x1 + x2

# ---------------------------------------------------
# Safety check: identical design matrices
# ---------------------------------------------------

terms1 <- servers[[1]]$termnames(formula)
for (i in 2:length(servers)) {
  stopifnot(identical(terms1, servers[[i]]$termnames(formula)))
}

# ---------------------------------------------------
# Fit federated model
# ---------------------------------------------------

fit_fed <- fed_logistic_newton(
  formula = formula,
  servers = servers,
  max_iter = 50,
  tol_ll = 1e-8,
  verbose = TRUE,
  robust_cluster = TRUE
)

beta <- fit_fed$coefficients

# ===================================================
# 1) MODEL-BASED INFERENCE
# ===================================================

se   <- fit_fed$se
zval <- beta / se
pval <- 2 * (1 - pnorm(abs(zval)))

results_model <- data.frame(
  Estimate = beta,
  StdError = se,
  z = zval,
  p = pval,
  OR = exp(beta),
  CI_lower = exp(beta - 1.96 * se),
  CI_upper = exp(beta + 1.96 * se)
)

cat("\n====================================================\n")
cat("Federated GLM Results (HTTP) — Model-based SE\n")
cat("====================================================\n")
print(round(results_model, 6))

# ===================================================
# 2) CLUSTER-ROBUST (HOSPITAL-LEVEL) INFERENCE
# ===================================================

if (!is.null(fit_fed$se_robust)) {
  
  seR   <- fit_fed$se_robust
  zR    <- beta / seR
  
  pR_norm <- 2 * (1 - pnorm(abs(zR)))
  dfR <- fit_fed$df_robust
  pR_t <- 2 * (1 - pt(abs(zR), df = dfR))
  
  results_robust <- data.frame(
    Estimate = beta,
    RobustSE = seR,
    z = zR,
    p_norm = pR_norm,
    p_t_dfGm1 = pR_t,
    OR = exp(beta),
    CI_lower = exp(beta - 1.96 * seR),
    CI_upper = exp(beta + 1.96 * seR)
  )
  
  cat("\n====================================================\n")
  cat("Federated GLM Results (HTTP) — Cluster-Robust SE\n")
  cat("====================================================\n")
  cat(sprintf("Clusters (G) = %d, df = %d\n",
              fit_fed$clusters, fit_fed$df_robust))
  
  print(round(results_robust, 6))
}

# ===================================================
# 3) LEAVE-ONE-HOSPITAL-OUT JACKKNIFE (Small-G Robust)
# ===================================================

cat("\n====================================================\n")
cat("Federated GLM Results (HTTP) — Leave-One-Hospital-Out Jackknife\n")
cat("====================================================\n")

G <- length(servers)
beta_full <- beta

beta_minus <- matrix(NA, nrow = G, ncol = length(beta_full))
colnames(beta_minus) <- names(beta_full)

for (g in seq_len(G)) {
  
  cat("Re-fitting without hospital", g, "...\n")
  
  servers_minus_g <- servers[-g]
  
  fit_minus <- fed_logistic_newton(
    formula = formula,
    servers = servers_minus_g,
    max_iter = 50,
    tol_ll = 1e-8,
    verbose = FALSE,
    robust_cluster = FALSE
  )
  
  beta_minus[g, ] <- fit_minus$coefficients
}

beta_bar <- colMeans(beta_minus)

# Jackknife variance formula
var_jk <- (G - 1) / G * colSums(
  (beta_minus - 
     matrix(beta_bar,
            nrow = G,
            ncol = length(beta_bar),
            byrow = TRUE))^2
)

se_jk <- sqrt(var_jk)

z_jk <- beta_full / se_jk
p_jk <- 2 * (1 - pnorm(abs(z_jk)))

ci_lower_jk <- beta_full - 1.96 * se_jk
ci_upper_jk <- beta_full + 1.96 * se_jk

jk_table <- data.frame(
  Estimate = beta_full,
  JackknifeSE = se_jk,
  z = z_jk,
  p = p_jk,
  OR = exp(beta_full),
  CI_lower = exp(ci_lower_jk),
  CI_upper = exp(ci_upper_jk)
)

print(round(jk_table, 6))