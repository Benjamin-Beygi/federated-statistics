# client/client_fed_lr_http.R
# Federated Logistic Regression over HTTP (Plumber transport)

source(file.path("client", "fed_engine.R"))
source(file.path("client", "http_server_adapter.R"))

cat("Client connecting over HTTP...\n")

servers <- list(
  create_http_server("http://127.0.0.1:8001"),
  create_http_server("http://127.0.0.1:8002")
)

formula <- y ~ age + sex + x1 + x2

# Safety: termnames must match
terms1 <- servers[[1]]$termnames(formula)
for (i in 2:length(servers)) {
  stopifnot(identical(terms1, servers[[i]]$termnames(formula)))
}

fit_fed <- fed_logistic_newton(
  formula = formula,
  servers = servers,
  max_iter = 30,
  tol = 1e-4,
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

cat("\nFederated GLM Results (HTTP):\n")
print(round(results, 6))