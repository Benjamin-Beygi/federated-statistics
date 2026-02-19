# server/server_fed_lr_functions.R
# ==========================================================
# Server-side statistical primitives (no raw data leaves site)
# Hardened for exact equivalence:
# - Logistic regression: gradient + Hessian + log-likelihood
# - Linear regression: sufficient statistics (XtX, Xty, yTy)
# - Numeric summary
# - Grouped numeric summaries (Welch t-test)
# - 2x2 counts for chi-square (binary x binary)  <-- NEW, SAFE
# ==========================================================

.build_design <- function(data, formula) {
  mf <- model.frame(formula, data = data, na.action = na.omit)
  y  <- model.response(mf)
  X  <- model.matrix(formula, data = mf)
  list(X = X, y = y)
}

# ---------- Logistic regression building block ----------
.server_grad_hess <- function(data, formula, beta) {
  des <- .build_design(data, formula)
  X <- des$X
  y <- as.numeric(des$y)
  
  eta <- as.vector(X %*% beta)
  p   <- 1 / (1 + exp(-eta))
  
  g <- as.vector(crossprod(X, (y - p)))
  
  w <- as.vector(p * (1 - p))
  H <- - crossprod(X, X * w)
  
  ll <- sum(y * log(p + 1e-12) + (1 - y) * log(1 - p + 1e-12))
  
  list(n = length(y), grad = g, hess = H, ll = ll)
}

.server_termnames <- function(data, formula) {
  des <- .build_design(data, formula)
  colnames(des$X)
}

# ---------- Linear regression sufficient statistics ----------
.server_lm_suffstats <- function(data, formula) {
  des <- .build_design(data, formula)
  X <- des$X
  y <- as.numeric(des$y)
  
  XtX <- crossprod(X)
  Xty <- as.vector(crossprod(X, y))
  yTy <- sum(y * y)
  
  list(
    n = length(y),
    termnames = colnames(X),
    XtX = XtX,
    Xty = Xty,
    yTy = yTy
  )
}

# ---------- Numeric summary ----------
.server_numeric_summary <- function(data, varname) {
  x <- data[[varname]]
  if (is.null(x)) stop(paste("Unknown variable:", varname))
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  list(
    type  = "numeric",
    n     = length(x),
    sum   = sum(x),
    sumsq = sum(x^2)
  )
}

# ---------- Grouped numeric summaries ----------
# Returns list(level -> {n,sum,sumsq})
.server_group_numeric_summary <- function(data, varname, groupvar) {
  x <- data[[varname]]
  g <- data[[groupvar]]
  
  if (is.null(x)) stop(paste("Unknown variable:", varname))
  if (is.null(g)) stop(paste("Unknown group variable:", groupvar))
  
  x <- as.numeric(x)
  g <- as.character(g)
  
  ok <- !(is.na(x) | is.na(g))
  x <- x[ok]
  g <- g[ok]
  
  levs <- sort(unique(g))
  stats <- vector("list", length(levs))
  names(stats) <- levs
  
  for (lv in levs) {
    xi <- x[g == lv]
    stats[[lv]] <- list(
      n = length(xi),
      sum = sum(xi),
      sumsq = sum(xi^2)
    )
  }
  
  list(type="group_numeric", levels=levs, stats=stats)
}

# ---------- NEW: exact 2x2 counts for binary x binary ----------
# Input variables must be binary-coded 0/1 (numeric or logical-like).
# Returns counts n00, n01, n10, n11 for (x, y) in {0,1}^2 with NA removed.
.server_2x2_counts <- function(data, xvar, yvar) {
  x <- data[[xvar]]
  y <- data[[yvar]]
  
  if (is.null(x)) stop(paste("Unknown variable:", xvar))
  if (is.null(y)) stop(paste("Unknown variable:", yvar))
  
  x <- suppressWarnings(as.numeric(x))
  y <- suppressWarnings(as.numeric(y))
  
  ok <- !(is.na(x) | is.na(y))
  x <- x[ok]
  y <- y[ok]
  
  # Validate values in {0,1}
  if (any(!(x %in% c(0,1))) || any(!(y %in% c(0,1)))) {
    stop("2x2_counts requires binary 0/1 variables.")
  }
  
  n00 <- sum(x == 0 & y == 0)
  n01 <- sum(x == 0 & y == 1)
  n10 <- sum(x == 1 & y == 0)
  n11 <- sum(x == 1 & y == 1)
  
  list(
    type = "2x2",
    xvar = xvar,
    yvar = yvar,
    n00 = n00,
    n01 = n01,
    n10 = n10,
    n11 = n11,
    n = length(x)
  )
}