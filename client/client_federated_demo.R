cat("\n=============================\n")
cat("Federated Logistic Regression\n")
cat("=============================\n\n")

# Each server runs model locally
fitA <- serverA$logistic("y ~ age + sex + x1 + x2")
fitB <- serverB$logistic("y ~ age + sex + x1 + x2")

cat("Server A coefficients:\n")
print(fitA$coefficients)

cat("\nServer B coefficients:\n")
print(fitB$coefficients)

# Extract coefficient matrices
coefA <- fitA$coefficients
coefB <- fitB$coefficients

# Inverse-variance weighted pooling
varA <- fitA$coefficients[,2]^2
varB <- fitB$coefficients[,2]^2

weightA <- 1 / varA
weightB <- 1 / varB

betaA <- coefA[,1]
betaB <- coefB[,1]

beta_ivw <- (weightA * betaA + weightB * betaB) /
  (weightA + weightB)

cat("\n--- Federated pooled coefficients (inverse variance weighted) ---\n")
print(beta_ivw)

cat("\n--- Federated pooled odds ratios (IVW) ---\n")
print(exp(beta_ivw))