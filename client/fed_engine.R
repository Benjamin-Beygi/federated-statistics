# client/fed_engine.R
# Federated Newton/IRLS engine with variance estimation

fed_logistic_newton <- function(formula,
                                servers,
                                beta_init = NULL,
                                max_iter = 30,
                                tol = 1e-6,
                                ridge = 1e-8,
                                verbose = TRUE) {
  
  termnames <- servers[[1]]$termnames(formula)
  p <- length(termnames)
  
  if (is.null(beta_init)) {
    beta <- rep(0, p)
  } else {
    stopifnot(length(beta_init) == p)
    beta <- as.numeric(beta_init)
  }
  names(beta) <- termnames
  
  if (verbose) {
    cat("\n=============================\n")
    cat("Federated Logistic Regression (Newton/IRLS)\n")
    cat("=============================\n")
    cat("Servers:", length(servers), "\n")
    cat("Terms:\n")
    print(termnames)
  }
  
  for (iter in 1:max_iter) {
    
    grad <- rep(0, p)
    hess <- matrix(0, p, p)
    ll   <- 0
    nTot <- 0
    
    for (srv in servers) {
      res <- srv$grad_hess(formula, unname(beta))
      
      grad <- grad + res$grad
      hess <- hess + res$hess
      ll   <- ll   + res$ll
      nTot <- nTot + res$n
    }
    
    H_stab <- hess - diag(ridge, p)
    step   <- as.vector(solve(H_stab, grad))
    beta_new <- unname(beta) - step
    names(beta_new) <- termnames
    
    step_norm <- sqrt(sum((beta_new - unname(beta))^2))
    
    if (verbose) {
      cat(sprintf(
        "Iter %02d | n=%d | ll=%.3f | step_norm=%.6g\n",
        iter, nTot, ll, step_norm
      ))
    }
    
    beta <- beta_new
    
    if (step_norm < tol) {
      if (verbose) cat("Converged.\n")
      break
    }
  }
  
  # ---- Variance-Covariance Matrix ----
  # Recompute final Hessian at converged beta
  final_hess <- matrix(0, p, p)
  
  for (srv in servers) {
    res <- srv$grad_hess(formula, unname(beta))
    final_hess <- final_hess + res$hess
  }
  
  # Observed Fisher information = -H
  info_matrix <- -final_hess
  
  vcov_beta <- solve(info_matrix)
  rownames(vcov_beta) <- termnames
  colnames(vcov_beta) <- termnames
  
  se_beta <- sqrt(diag(vcov_beta))
  
  list(
    coefficients = beta,
    vcov = vcov_beta,
    se = se_beta
  )
}