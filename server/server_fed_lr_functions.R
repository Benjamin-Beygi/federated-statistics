# server/server_fed_lr_functions.R
# Federated logistic regression building blocks:
# - Each server computes gradient + Hessian for given beta (no raw data leaves server)

.build_design <- function(data, formula) {
  mf <- model.frame(formula, data = data, na.action = na.omit)
  y  <- model.response(mf)  # numeric 0/1
  X  <- model.matrix(formula, data = mf)
  list(X = X, y = y)
}

.server_grad_hess <- function(data, formula, beta) {
  des <- .build_design(data, formula)
  X <- des$X
  y <- des$y
  
  # linear predictor and probabilities
  eta <- as.vector(X %*% beta)
  p   <- 1 / (1 + exp(-eta))
  
  # gradient (score): X^T (y - p)
  g <- as.vector(t(X) %*% (y - p))
  
  # Hessian: -X^T W X  where W = diag(p*(1-p))
  w <- as.vector(p * (1 - p))
  # Efficient: t(X) %*% (X * w)
  H <- - crossprod(X, X * w)
  
  list(
    n = length(y),
    grad = g,
    hess = H,
    # optional: local log-likelihood, useful for debugging convergence
    ll = sum(y * log(p + 1e-12) + (1 - y) * log(1 - p + 1e-12))
  )
}

# Helper for IVW/compat: returns term names for beta vector
.server_termnames <- function(data, formula) {
  des <- .build_design(data, formula)
  colnames(des$X)
}