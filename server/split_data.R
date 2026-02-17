# Load full dataset
df <- read.csv(file.path("server", "data", "demo_dataset.csv"))

set.seed(42)

# Randomly split into two sites
site_id <- sample(c("A", "B"), nrow(df), replace = TRUE)

df_A <- df[site_id == "A", ]
df_B <- df[site_id == "B", ]

# Create directory
dir.create("server/data", showWarnings = FALSE)

write.csv(df_A, file.path("server", "data", "site_A.csv"), row.names = FALSE)
write.csv(df_B, file.path("server", "data", "site_B.csv"), row.names = FALSE)

cat("Split complete.\n")
cat("Site A n =", nrow(df_A), "\n")
cat("Site B n =", nrow(df_B), "\n")