# client/fed_engine.R
# ==========================================================
# Exact Distributed Logistic Regression via Newton-Raphson
# ==========================================================

fed_logistic_newton <- function(formula,
                                servers,
                                beta_init = NULL,
                                max_iter = 50,
                                tol_score = 1e-8,
                                tol_ll = 1e-10,
                                verbose = TRUE,
                                robust_cluster = TRUE) {
  
  termnames <- servers[[1]]$termnames(formula)
  p <- length(termnames)
  
  beta <- if (is.null(beta_init)) rep(0, p) else as.numeric(beta_init)
  names(beta) <- termnames
  
  if (verbose) {
    cat("\n====================================\n")
    cat("Exact Federated Logistic Regression\n")
    cat("====================================\n")
    cat("Servers:", length(servers), "\n")
    cat("Parameters:", p, "\n\n")
  }
  
  # -----------------------------------------
  # Helper: compute global log-likelihood
  # -----------------------------------------
  federated_ll <- function(beta_vec) {
    ll_sum <- 0
    for (srv in servers) {
      res <- srv$grad_hess(formula, unname(beta_vec))
      ll_sum <- ll_sum + res$ll
    }
    ll_sum
  }
  
  ll_old <- federated_ll(beta)
  
  # -----------------------------------------
  # Newton iterations
  # -----------------------------------------
  for (iter in 1:max_iter) {
    
    grad <- rep(0, p)
    hess <- matrix(0, p, p)
    Ntot <- 0
    
    for (srv in servers) {
      res <- srv$grad_hess(formula, unname(beta))
      grad <- grad + res$grad
      hess <- hess + res$hess
      Ntot <- Ntot + res$n
    }
    
    score_norm <- sqrt(sum(grad^2))
    
    if (verbose) {
      cat(sprintf("Iter %02d | score_norm = %.6g\n",
                  iter, score_norm))
    }
    
    # ---- Convergence based on score ----
    if (score_norm < tol_score) {
      if (verbose) cat("Converged (score norm).\n")
      break
    }
    
    # ---- Newton step ----
    step <- solve(hess, grad)
    beta_new <- beta - step
    names(beta_new) <- termnames
    
    ll_new <- federated_ll(beta_new)
    
    if (abs(ll_new - ll_old) < tol_ll) {
      if (verbose) cat("Converged (LL stabilized).\n")
      beta <- beta_new
      break
    }
    
    beta <- beta_new
    ll_old <- ll_new
  }
  
  # -----------------------------------------
  # Final Hessian + score per cluster
  # -----------------------------------------
  final_hess <- matrix(0, p, p)
  cluster_scores <- vector("list", length(servers))
  
  for (i in seq_along(servers)) {
    res <- servers[[i]]$grad_hess(formula, unname(beta))
    final_hess <- final_hess + res$hess
    cluster_scores[[i]] <- as.numeric(res$grad)
  }
  
  # Fisher information
  info_matrix <- -final_hess
  vcov_model <- solve(info_matrix)
  se_model <- sqrt(diag(vcov_model))
  
  out <- list(
    coefficients = beta,
    vcov = vcov_model,
    se = se_model,
    logLik = ll_old,
    N = Ntot
  )
  
  # -----------------------------------------
  # Cluster-robust variance (CR1)
  # -----------------------------------------
  if (robust_cluster) {
    
    G <- length(cluster_scores)
    
    if (G >= 2) {
      
      meat <- matrix(0, p, p)
      
      for (g in seq_len(G)) {
        u <- matrix(cluster_scores[[g]], ncol = 1)
        meat <- meat + u %*% t(u)
      }
      
      vcov_robust <- vcov_model %*% meat %*% vcov_model
      
      # CR1 small sample correction
      cr1 <- (G / (G - 1)) * ((Ntot - 1) / (Ntot - p))
      vcov_robust <- cr1 * vcov_robust
      
      out$vcov_robust <- vcov_robust
      out$se_robust <- sqrt(diag(vcov_robust))
      out$clusters <- G
      out$df_robust <- G - 1
    }
  }
  
  return(out)
}