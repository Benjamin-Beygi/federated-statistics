# server/api_server.R

library(plumber)

# We are inside /server when plumber parses this file
source("server_factory.R")

site_csv <- Sys.getenv("SITE_CSV", unset = NA)
if (is.na(site_csv) || site_csv == "") {
  stop("SITE_CSV environment variable is not set.")
}

srv <- create_server(site_csv)

#* @get /health
function() {
  list(status = "ok", site_csv = site_csv)
}

#* @post /termnames
#* @param formula
function(formula) {
  srv$termnames(as.formula(formula))
}

#* @post /grad_hess
#* @param formula
#* @param beta
function(formula, beta) {
  beta <- as.numeric(beta)
  srv$grad_hess(as.formula(formula), beta)
}

#* @post /summary
#* @param varname
function(varname) {
  srv$summary(varname)
}