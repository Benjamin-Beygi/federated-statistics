set.seed(123)

n <- 2000

age <- rnorm(n, mean = 50, sd = 10)
sex <- sample(c("M", "F"), n, replace = TRUE)
x1  <- rnorm(n)
x2  <- sample(c("A", "B", "C"), n, replace = TRUE)

linpred <- -2 +
  0.03 * age +
  0.6 * (sex == "F") +
  0.4 * x1 -
  0.3 * (x2 == "B")

prob <- plogis(linpred)
y <- rbinom(n, 1, prob)

df <- data.frame(
  id  = 1:n,
  age = round(age, 1),
  sex = factor(sex),
  x1  = round(x1, 3),
  x2  = factor(x2),
  y   = y
)

out_file <- file.path("server", "data", "demo_dataset.csv")
write.csv(df, out_file, row.names = FALSE)

file.exists(out_file)

# ====================
# sanity checks 
# str(df)
# summary(df)
# mean(df$y)                # aproximate prevalence
# with(df, table(sex))
# with(df, table(x2))

# ====================
# sanity check: fit a logistic regression model to see if the coefficients are
#               close to the ones we used to generate the data
#
# fit <- glm(y ~ age + sex + x1 + x2, data = df, family = binomial)
# summary(fit)
# exp(coef(fit))