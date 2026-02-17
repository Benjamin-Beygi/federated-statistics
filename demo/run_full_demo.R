# demo/run_full_demo.R
# One-command federated demo runner

library(callr)
library(httr)

project_root <- normalizePath(".")

cat("Starting federated demo...\n")

# -----------------------------
# Helper: start hospital server
# -----------------------------
start_hospital <- function(site_csv, port) {
  
  r_bg(
    function(site_csv, port, project_root) {
      
      setwd(project_root)
      
      library(plumber)
      
      Sys.setenv(SITE_CSV = site_csv)
      
      pr <- plumber::plumb("server/api_server.R")
      pr$run(host = "127.0.0.1", port = port)
      
    },
    args = list(
      site_csv = site_csv,
      port = port,
      project_root = project_root
    ),
    supervise = TRUE
  )
}

# -----------------------------
# Start Hospital A & B
# -----------------------------
cat("Launching Hospital A...\n")
hospA <- start_hospital("server/data/site_A.csv", 8001)

cat("Launching Hospital B...\n")
hospB <- start_hospital("server/data/site_B.csv", 8002)

# -----------------------------
# Wait until servers respond
# -----------------------------
wait_until_alive <- function(port, timeout = 10) {
  t0 <- Sys.time()
  repeat {
    try({
      r <- GET(paste0("http://127.0.0.1:", port, "/health"))
      if (status_code(r) == 200) return(TRUE)
    }, silent = TRUE)
    
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout) {
      stop(paste("Server on port", port, "did not start in time"))
    }
    
    Sys.sleep(0.5)
  }
}

cat("Waiting for Hospital A...\n")
wait_until_alive(8001)

cat("Waiting for Hospital B...\n")
wait_until_alive(8002)

cat("Both hospitals are live.\n")

# -----------------------------
# Run Federated Client
# -----------------------------
source("client/client_fed_lr_http.R")

cat("\nDemo complete.\n")

cat("\nServers are still running in background.\n")
cat("To stop them, run:\n")
cat("hospA$kill(); hospB$kill()\n")