# server/server_A.R
# Loads/creates Server A dataset and exposes "serverA_*" functions.

source(file.path("server", "server_fed_lr_functions.R"))

df_all <- read.csv(file.path("server", "data", "demo_dataset.csv"))

# Split into A/B (stable split)
set.seed(1)
idxA <- sample(seq_len(nrow(df_all)), size = floor(0.5 * nrow(df_all)))
df_A <- df_all[idxA, ]

serverA_termnames <- function(formula) .server_termnames(df_A, formula)

serverA_grad_hess <- function(formula, beta) {
  .server_grad_hess(df_A, formula, beta)
}