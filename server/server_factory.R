# server/server_factory.R
# --------------------------------------------------------
# Server factory: builds a "site server" interface
# --------------------------------------------------------

source("server/server_fed_lr_functions.R")

create_server <- function(data_path,
                          min_n = 20) {
  
  if (!file.exists(data_path)) {
    data_path <- file.path("..", data_path)
  }
  if (!file.exists(data_path)) stop("Data file not found: ", data_path)
  
  df <- read.csv(
    data_path,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA", "NaN", "NULL")
  )
  
  if (nrow(df) < min_n) {
    stop("Privacy violation: insufficient sample size (n < min_n).")
  }
  
  list(
    termnames = function(formula) {
      .server_termnames(df, formula)
    },
    
    grad_hess = function(formula, beta) {
      .server_grad_hess(df, formula, beta)
    },
    
    summary_numeric = function(varname) {
      .server_numeric_summary(df, varname)
    },
    
    group_summaries = function(varname, groupvar) {
      .server_group_numeric_summary(df, varname, groupvar)
    },
    
    lm_suffstats = function(formula) {
      .server_lm_suffstats(df, formula)
    },
    
    # binary 2x2 counts
    counts_2x2 = function(xvar, yvar) {
      .server_2x2_counts(df, xvar, yvar)
    }
  )
}