# server/server_factory.R

# Load helper functions (relative to /server)
source("server_fed_lr_functions.R")

create_server <- function(data_path,
                          min_n = 20,
                          min_cell = 5) {
  
  # Make dataset path relative to project root
  if (!file.exists(data_path)) {
    data_path <- file.path("..", data_path)
  }
  
  df <- read.csv(data_path)
  
  if (nrow(df) < min_n) {
    stop("Privacy violation: insufficient sample size.")
  }
  
  list(
    termnames = function(formula) {
      .server_termnames(df, formula)
    },
    
    grad_hess = function(formula, beta) {
      .server_grad_hess(df, formula, beta)
    },
    
    summary = function(varname) {
      x <- df[[varname]]
      
      if (is.numeric(x)) {
        return(list(
          type = "numeric",
          n = length(x),
          sum = sum(x),
          sumsq = sum(x^2)
        ))
      }
      
      if (is.factor(x) || is.character(x)) {
        tab <- table(x)
        tab[tab < min_cell] <- NA
        
        return(list(
          type = "categorical",
          counts = as.list(tab)
        ))
      }
      
      stop("Unsupported variable type.")
    }
  )
}