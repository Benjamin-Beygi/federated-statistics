# server/server_B.R
# Loads/creates Server B dataset and exposes "serverB_*" functions.

source(file.path("server", "server_fed_lr_functions.R"))

df_all <- read.csv(file.path("server", "data", "demo_dataset.csv"))

# Split into A/B (stable split)
set.seed(1)
idxA <- sample(seq_len(nrow(df_all)), size = floor(0.5 * nrow(df_all)))
df_B <- df_all[-idxA, ]

serverB_termnames <- function(formula) .server_termnames(df_B, formula)

serverB_grad_hess <- function(formula, beta) {
  .server_grad_hess(df_B, formula, beta)
}