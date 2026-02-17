# client/http_server_adapter.R
# Wrap a remote Plumber server into the same interface expected by fed_engine.R

library(httr)
library(jsonlite)

create_http_server <- function(base_url) {
  
  post_json <- function(path, payload) {
    r <- POST(
      url = paste0(base_url, path),
      body = toJSON(payload, auto_unbox = TRUE),
      encode = "json"
    )
    
    if (http_error(r)) {
      msg <- content(r, as = "text", encoding = "UTF-8")
      stop(sprintf("HTTP error from %s%s: %s", base_url, path, msg))
    }
    
    content(r, as = "parsed", encoding = "UTF-8")
  }
  
  list(
    termnames = function(formula) {
      # Make sure this is a character vector (not a list)
      tn <- post_json("/termnames", list(formula = deparse(formula)))
      as.character(unlist(tn))
    },
    
    grad_hess = function(formula, beta) {
      res <- post_json("/grad_hess", list(formula = deparse(formula), beta = beta))
      
      # Force expected types
      res$grad <- as.numeric(res$grad)
      res$hess <- matrix(unlist(res$hess), nrow = length(res$grad), byrow = TRUE)
      res$n    <- as.integer(res$n)
      res$ll   <- as.numeric(res$ll)
      res
    },
    
    summary = function(varname) {
      post_json("/summary", list(varname = varname))
    }
  )
}