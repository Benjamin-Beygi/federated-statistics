# server/server_federated_glm.R

# Helper: ensure factors have consistent levels
standardize_factors <- function(df) {
  df$sex <- factor(df$sex, levels = c("F", "M"))
  df$x2  <- factor(df$x2,  levels = c("A", "B", "C"))
  df
}

# Build X and y consistently
build_design <- function(df, formula_str) {
  df <- standardize_factors(df)
  formula_obj <- as.formula(formula_str)
  
  X <- model.matrix(formula_obj, data = df)  # includes intercept by default
  y <- model.response(model.frame(formula_obj, data = df))
  list(X = X, y = y)
}

# Core federated computation: gradient + Hessian at current beta
# Returns list(grad=..., hess=..., n=..., colnames=...)
server_grad_hess <- function(formula_str, beta) {
  if (!exists("df_server")) stop("df_server not loaded on server.")
  des <- build_design(df_server, formula_str)
  
  X <- des$X
  y <- des$y
  
  beta <- as.numeric(beta)
  if (length(beta) != ncol(X)) {
    stop(sprintf("beta length %d does not match X columns %d", length(beta), ncol(X)))
  }
  
  eta <- as.vector(X %*% beta)
  p   <- 1 / (1 + exp(-eta))
  
  # gradient: X^T (y - p)
  grad <- as.vector(t(X) %*% (y - p))
  
  # Hessian: - X^T W X  (W = p(1-p))
  w <- as.vector(p * (1 - p))
  WX <- X * w
  hess <- - crossprod(X, WX)   # equivalent to -t(X) %*% (W*X)
  
  list(
    grad = grad,
    hess = hess,
    n = nrow(X),
    colnames = colnames(X)
  )
}