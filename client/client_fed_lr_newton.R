# client/client_fed_lr_newton.R
# Federated Logistic Regression using dynamic server factory

# ---- Load engine ----
source(file.path("client", "fed_engine.R"))

# ---- Load server factory ----
source(file.path("server", "server_factory.R"))

cat("Client starting servers via factory...\n")

# ---- Create servers dynamically from data files ----
servers <- list(
  create_server(file.path("server", "data", "site_A.csv")),
  create_server(file.path("server", "data", "site_B.csv"))
)

cat("Number of servers:", length(servers), "\n")

# ---- Specify model ----
formula <- y ~ age + sex + x1 + x2

# ---- Safety check: identical term ordering ----
terms1 <- servers[[1]]$termnames(formula)
for (i in 2:length(servers)) {
  stopifnot(identical(terms1, servers[[i]]$termnames(formula)))
}

# ---- Run federated Newton ----
fit_fed <- fed_logistic_newton(
  formula = formula,
  servers = servers,
  max_iter = 30,
  tol = 1e-6,
  ridge = 1e-8,
  verbose = TRUE
)

beta <- fit_fed$coefficients
se   <- fit_fed$se

zval <- beta / se
pval <- 2 * (1 - pnorm(abs(zval)))

results <- data.frame(
  Estimate = beta,
  StdError = se,
  z = zval,
  p = pval,
  OR = exp(beta),
  CI_lower = exp(beta - 1.96 * se),
  CI_upper = exp(beta + 1.96 * se)
)

cat("\nFederated GLM Results:\n")
print(round(results, 6))

# ---- Optional pooled check (LOCAL ONLY) ----
df_all <- read.csv(file.path("server", "data", "demo_dataset.csv"))
fit_pool <- glm(formula, data = df_all, family = binomial)

cat("\n--- Sanity check: pooled glm coefficients ---\n")
print(coef(fit_pool))

cat("\n--- Difference (federated - pooled) ---\n")
print(beta - coef(fit_pool))