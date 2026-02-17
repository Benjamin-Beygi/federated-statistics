# client/client_fed_summary.R
# Federated summary statistics with disclosure control

source(file.path("server", "server_factory.R"))

# Create servers
servers <- list(
  create_server(file.path("server", "data", "site_A.csv")),
  create_server(file.path("server", "data", "site_B.csv"))
)

cat("Federated summary demo\n")

# ----------------------------
# NUMERIC EXAMPLE
# ----------------------------
varname <- "age"

total_n   <- 0
total_sum <- 0
total_ss  <- 0

for (srv in servers) {
  res <- srv$summary(varname)
  
  stopifnot(res$type == "numeric")
  
  total_n   <- total_n   + res$n
  total_sum <- total_sum + res$sum
  total_ss  <- total_ss  + res$sumsq
}

mean_fed <- total_sum / total_n
var_fed  <- (total_ss - total_sum^2 / total_n) / (total_n - 1)
sd_fed   <- sqrt(var_fed)

cat("\nFederated numeric summary for:", varname, "\n")
cat("n =", total_n, "\n")
cat("mean =", mean_fed, "\n")
cat("sd =", sd_fed, "\n")


# ----------------------------
# CATEGORICAL EXAMPLE
# ----------------------------
varname <- "sex"

combined_counts <- list()

for (srv in servers) {
  res <- srv$summary(varname)
  
  stopifnot(res$type == "categorical")
  
  for (lvl in names(res$counts)) {
    val <- res$counts[[lvl]]
    
    if (is.na(val)) next  # skip suppressed cells
    
    if (is.null(combined_counts[[lvl]])) {
      combined_counts[[lvl]] <- val
    } else {
      combined_counts[[lvl]] <- combined_counts[[lvl]] + val
    }
  }
}

cat("\nFederated categorical summary for:", varname, "\n")
print(combined_counts)