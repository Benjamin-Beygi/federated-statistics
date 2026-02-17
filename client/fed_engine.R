# client/fed_engine.R
# Federated Damped Newton with LL-based convergence

fed_logistic_newton <- function(formula,
                                servers,
                                beta_init = NULL,
                                max_iter = 50,
                                tol_ll = 1e-8,
                                ridge = 1e-6,
                                verbose = TRUE,
                                robust_cluster = TRUE) {
  
  termnames <- servers[[1]]$termnames(formula)
  p <- length(termnames)
  
  beta <- if (is.null(beta_init)) rep(0, p) else as.numeric(beta_init)
  names(beta) <- termnames
  
  if (verbose) {
    cat("\n=============================\n")
    cat("Federated Logistic Regression (Damped Newton)\n")
    cat("=============================\n")
    cat("Servers:", length(servers), "\n")
    cat("Terms:\n")
    print(termnames)
  }
  
  federated_ll <- function(beta_vec) {
    ll_sum <- 0
    for (srv in servers) {
      res <- srv$grad_hess(formula, unname(beta_vec))
      ll_sum <- ll_sum + res$ll
    }
    ll_sum
  }
  
  ll_old <- federated_ll(beta)
  nTot_last <- NA_integer_
  
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
    
    nTot_last <- nTot
    
    H_stab <- hess - diag(ridge, p)
    step_full <- as.vector(solve(H_stab, grad))
    
    alpha <- 1
    ll_tol <- 1e-10
    
    repeat {
      beta_try <- unname(beta) - alpha * step_full
      names(beta_try) <- termnames
      ll_new <- federated_ll(beta_try)
      
      if (ll_new >= ll - ll_tol || alpha < 1e-4) {
        beta_new <- beta_try
        ll_acc <- ll_new
        break
      }
      
      alpha <- alpha / 2
    }
    
    step_norm <- sqrt(sum((beta_new - beta)^2))
    
    if (verbose) {
      cat(sprintf(
        "Iter %02d | ll=%.6f | step_norm=%.6g | alpha=%.4g\n",
        iter, ll, step_norm, alpha
      ))
    }
    
    # ---- LL-based convergence ----
    if (abs(ll_acc - ll_old) < tol_ll) {
      if (verbose) cat("Converged (LL stabilized).\n")
      beta <- beta_new
      break
    }
    
    beta <- beta_new
    ll_old <- ll_acc
  }
  
  # Final variance
  final_hess <- matrix(0, p, p)
  cluster_scores <- vector("list", length(servers))
  
  for (i in seq_along(servers)) {
    res <- servers[[i]]$grad_hess(formula, unname(beta))
    final_hess <- final_hess + res$hess
    cluster_scores[[i]] <- as.numeric(res$grad)
  }
  
  info_matrix <- -final_hess
  vcov_model <- solve(info_matrix)
  se_model <- sqrt(diag(vcov_model))
  
  out <- list(
    coefficients = beta,
    vcov = vcov_model,
    se = se_model
  )
  
  if (robust_cluster) {
    G <- length(cluster_scores)
    N <- as.numeric(nTot_last)
    
    if (G >= 2) {
      meat <- matrix(0, p, p)
      for (g in seq_len(G)) {
        u <- matrix(cluster_scores[[g]], ncol = 1)
        meat <- meat + (u %*% t(u))
      }
      
      cr1 <- (G / (G - 1)) * ((N - 1) / (N - p))
      vcov_robust <- cr1 * (vcov_model %*% meat %*% vcov_model)
      se_robust <- sqrt(diag(vcov_robust))
      
      out$vcov_robust <- vcov_robust
      out$se_robust <- se_robust
      out$clusters <- G
      out$df_robust <- G - 1
    }
  }
  
  out
}