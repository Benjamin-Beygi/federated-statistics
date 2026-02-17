# client/client_fed_lr_newton.R
# Federated Logistic Regression via Newton-Raphson / IRLS
# Client sends beta -> servers return gradient/Hessian -> client updates beta

# ---- Start "servers" (in real life these would be remote calls) ----
source(file.path("server", "server_A.R"))
source(file.path("server", "server_B.R"))

cat("Client connected to Server A and Server B.\n")

# ---- Specify the model (must be same on both servers) ----
formula <- y ~ age + sex + x1 + x2

# ---- Ensure both servers have identical term ordering ----
termsA <- serverA_termnames(formula)
termsB <- serverB_termnames(formula)

stopifnot(identical(termsA, termsB))
termnames <- termsA

p <- length(termnames)
beta <- rep(0, p)  # initialize at zero

cat("\n=============================\n")
cat("Federated Logistic Regression (Newton/IRLS)\n")
cat("=============================\n")
cat("Terms:\n")
print(termnames)

# ---- Newton loop settings ----
max_iter <- 30
tol      <- 1e-6
ridge    <- 1e-8     # stabilizer for near-singular Hessian
verbose  <- TRUE

for (iter in 1:max_iter) {
  
  # Ask each server for local grad/Hess at current beta
  resA <- serverA_grad_hess(formula, beta)
  resB <- serverB_grad_hess(formula, beta)
  
  # Aggregate (federated sum)
  grad <- resA$grad + resB$grad
  hess <- resA$hess + resB$hess
  ll   <- resA$ll   + resB$ll
  nTot <- resA$n    + resB$n
  
  # Ridge stabilize Hessian: H - ridge*I (remember H is negative definite)
  H_stab <- hess - diag(ridge, p)
  
  # Newton step: beta_new = beta - solve(H) %*% grad
  step <- as.vector(solve(H_stab, grad))
  beta_new <- beta - step
  
  # Convergence diagnostics
  step_norm <- sqrt(sum((beta_new - beta)^2))
  grad_norm <- sqrt(sum(grad^2))
  
  if (verbose) {
    cat(sprintf(
      "Iter %02d | n=%d | ll=%.3f | step_norm=%.6g | grad_norm=%.6g\n",
      iter, nTot, ll, step_norm, grad_norm
    ))
  }
  
  beta <- beta_new
  
  if (step_norm < tol) {
    cat("Converged.\n")
    break
  }
}

# ---- Output ----
names(beta) <- termnames
cat("\nFinal federated beta:\n")
print(beta)

cat("\nOdds ratios:\n")
print(exp(beta))

# ---- Optional: Compare to pooled glm for sanity check (client DOES NOT need raw data in real life)
# Here we do it only locally as a correctness test.
df_all <- read.csv(file.path("server", "data", "demo_dataset.csv"))
fit_pool <- glm(formula, data = df_all, family = binomial)

cat("\n--- Sanity check: pooled glm coefficients ---\n")
print(coef(fit_pool))

cat("\n--- Difference (federated - pooled) ---\n")
print(beta - coef(fit_pool))