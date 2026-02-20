# demo/run_swespine_victor_demo.R
# ==========================================================
# Swespine Victor Demo (Central vs Simulated Federated)
# Reads only: sheet = "Raw_data"
#
# Outputs:
# - Mean age, BMI, sex distribution
# - Welch t-test age (radiculopathy vs myelopathy)
# - Chi-square sex vs diagnosis group  (binary 2x2 exact)
# - Linear regression: NRS arm 12m ~ age + sexM
# - Logistic regression: satisfied 12m ~ age + sexM
#
# Hardening for exact equivalence:
# - No factor predictors in models (sexM numeric)
# - Chi-square computed from exact binary 2x2 counts (no string drift)
# ==========================================================

suppressPackageStartupMessages({
  library(readxl)
})

source(file.path("server", "server_factory.R"))
source(file.path("client", "fed_engine.R"))

# ----------------------------
# Config
# ----------------------------
xlsx_path <- file.path("server", "data", "Cervical_Swespine_ZN.xlsx")
sheet_name <- "Raw_data"

AGE_COL_CANDIDATES <- c("Age", "age", "Alder", "Ålder", "PreopHR_Alder", "PreopHR_AlderAr")

COL_CLINIC <- "DG_Klinik"
COL_PNR <- "PAT_Pnr"
COL_DIAG <- "OpHR_Diagnos1"
COL_BMI_H <- "PreopHR_Langd"
COL_BMI_W <- "PreopHR_Vikt"
COL_NRS_ARM_12M <- "UppHR_NRSArm_1"
COL_SAT_12M <- "UppHR_InstRes_1"

RADIC_CODE <- 1
MYELO_CODE <- 10
MIN_SITE_N <- 20

# ----------------------------
# Utility: sex from PNR
# ----------------------------
sex_from_pnr <- function(pnr_vec) {
  p <- gsub("[^0-9]", "", as.character(pnr_vec))
  p[nchar(p) < 10] <- NA
  last_digit <- suppressWarnings(as.integer(substr(p, nchar(p), nchar(p))))
  ifelse(is.na(last_digit), NA, ifelse(last_digit %% 2 == 1, "M", "F"))
}

# ----------------------------
# Federated helpers
# ----------------------------
fed_numeric <- function(servers, varname) {
  N <- 0; S <- 0; SS <- 0
  for (srv in servers) {
    res <- srv$summary_numeric(varname)
    stopifnot(res$type == "numeric")
    N <- N + res$n
    S <- S + res$sum
    SS <- SS + res$sumsq
  }
  mean <- S / N
  var <- (SS - S^2 / N) / (N - 1)
  sd <- sqrt(var)
  list(n = N, mean = mean, sd = sd)
}

fed_welch_t <- function(servers, varname, groupvar, g1, g2) {
  n1 <- s1 <- ss1 <- 0
  n2 <- s2 <- ss2 <- 0
  
  for (srv in servers) {
    res <- srv$group_summaries(varname, groupvar)
    st <- res$stats
    
    if (!is.null(st[[as.character(g1)]])) {
      n1 <- n1 + st[[as.character(g1)]]$n
      s1 <- s1 + st[[as.character(g1)]]$sum
      ss1 <- ss1 + st[[as.character(g1)]]$sumsq
    }
    if (!is.null(st[[as.character(g2)]])) {
      n2 <- n2 + st[[as.character(g2)]]$n
      s2 <- s2 + st[[as.character(g2)]]$sum
      ss2 <- ss2 + st[[as.character(g2)]]$sumsq
    }
  }
  
  m1 <- s1 / n1
  m2 <- s2 / n2
  v1 <- (ss1 - s1^2 / n1) / (n1 - 1)
  v2 <- (ss2 - s2^2 / n2) / (n2 - 1)
  
  se <- sqrt(v1/n1 + v2/n2)
  tval <- (m1 - m2) / se
  
  df <- (v1/n1 + v2/n2)^2 / ((v1^2)/((n1^2)*(n1-1)) + (v2^2)/((n2^2)*(n2-1)))
  p <- 2 * (1 - pt(abs(tval), df = df))
  
  list(n1=n1, n2=n2, m1=m1, m2=m2, t=tval, df=df, p=p)
}

# federated chi-square from 2x2 counts
fed_chisq_2x2 <- function(servers, xvar, yvar, correct = TRUE) {
  n00 <- n01 <- n10 <- n11 <- 0
  N <- 0
  
  for (srv in servers) {
    res <- srv$counts_2x2(xvar, yvar)
    stopifnot(res$type == "2x2")
    n00 <- n00 + res$n00
    n01 <- n01 + res$n01
    n10 <- n10 + res$n10
    n11 <- n11 + res$n11
    N <- N + res$n
  }
  
  tab <- matrix(c(n00, n01, n10, n11), nrow = 2, byrow = TRUE)
  rownames(tab) <- c(paste0(xvar,"=0"), paste0(xvar,"=1"))
  colnames(tab) <- c(paste0(yvar,"=0"), paste0(yvar,"=1"))
  
  tst <- suppressWarnings(chisq.test(tab, correct = correct))
  list(table = tab, statistic = unname(tst$statistic), df = unname(tst$parameter), p = unname(tst$p.value), N = N)
}

fed_lm <- function(servers, formula) {
  XtX <- NULL
  Xty <- NULL
  yTy <- 0
  N <- 0
  termnames <- NULL
  
  for (srv in servers) {
    res <- srv$lm_suffstats(formula)
    if (is.null(termnames)) termnames <- res$termnames
    if (is.null(XtX)) {
      XtX <- res$XtX
      Xty <- res$Xty
    } else {
      XtX <- XtX + res$XtX
      Xty <- Xty + res$Xty
    }
    yTy <- yTy + res$yTy
    N <- N + res$n
  }
  
  beta <- as.vector(solve(XtX, Xty))
  names(beta) <- termnames
  p <- length(beta)
  
  RSS <- yTy - 2 * sum(beta * Xty) + as.numeric(t(beta) %*% XtX %*% beta)
  sigma2 <- RSS / (N - p)
  vcov <- sigma2 * solve(XtX)
  se <- sqrt(diag(vcov))
  
  tval <- beta / se
  pval <- 2 * (1 - pt(abs(tval), df = N - p))
  
  list(beta = beta, se = se, t = tval, p = pval, N = N, df = N - p, vcov = vcov)
}

# ----------------------------
# 1) Load Swespine (Raw_data only)
# ----------------------------
if (!file.exists(xlsx_path)) stop("Missing Swespine Excel file at: ", xlsx_path)

cat("Reading Swespine Excel:\n  ", xlsx_path, "\nSheet:\n  ", sheet_name, "\n")
raw <- read_excel(xlsx_path, sheet = sheet_name)
raw <- as.data.frame(raw, stringsAsFactors = FALSE)

required_cols <- c(COL_CLINIC, COL_PNR, COL_DIAG, COL_BMI_H, COL_BMI_W, COL_NRS_ARM_12M, COL_SAT_12M)
missing <- setdiff(required_cols, names(raw))
if (length(missing) > 0) stop("Missing required columns in Raw_data: ", paste(missing, collapse = ", "))

# ----------------------------
# 2) Build analysis dataframe (NO dates)
# ----------------------------
age_col <- AGE_COL_CANDIDATES[AGE_COL_CANDIDATES %in% names(raw)][1]
if (is.na(age_col) || is.null(age_col)) {
  stop("No age column detected. Set AGE_COL_CANDIDATES to the correct column name.")
}

age <- suppressWarnings(as.numeric(raw[[age_col]]))

sex <- sex_from_pnr(raw[[COL_PNR]])
sexM <- ifelse(sex == "M", 1, ifelse(sex == "F", 0, NA))

diag_code <- suppressWarnings(as.numeric(raw[[COL_DIAG]]))
diag_grp <- ifelse(diag_code == RADIC_CODE, "radiculopathy",
                   ifelse(diag_code == MYELO_CODE, "myelopathy", NA))
diag_myelo <- ifelse(diag_code == MYELO_CODE, 1,
                     ifelse(diag_code == RADIC_CODE, 0, NA))

h_cm <- suppressWarnings(as.numeric(raw[[COL_BMI_H]]))
w_kg <- suppressWarnings(as.numeric(raw[[COL_BMI_W]]))
bmi <- w_kg / ((h_cm / 100)^2)
bmi[!is.finite(bmi)] <- NA
bmi[bmi < 10 | bmi > 80] <- NA

nrs_arm_12m <- suppressWarnings(as.numeric(raw[[COL_NRS_ARM_12M]]))
nrs_arm_12m[nrs_arm_12m < 0 | nrs_arm_12m > 10] <- NA

sat_raw <- suppressWarnings(as.numeric(raw[[COL_SAT_12M]]))
satisfied_12m <- ifelse(sat_raw == 1, 1,
                        ifelse(sat_raw %in% c(2, 3), 0, NA))

clinic <- as.character(raw[[COL_CLINIC]])
clinic[clinic == ""] <- NA

df <- data.frame(
  clinic = clinic,
  diag_grp = diag_grp,
  diag_myelo = diag_myelo,
  age = age,
  sex = sex,
  sexM = sexM,
  bmi = bmi,
  nrs_arm_12m = nrs_arm_12m,
  satisfied_12m = satisfied_12m,
  stringsAsFactors = FALSE
)

# Filter to Victor demo universe (same for all stats except BMI which can be NA)
df <- df[!is.na(df$clinic) & !is.na(df$age) & !is.na(df$sexM) & !is.na(df$diag_grp) & !is.na(df$diag_myelo), ]

cat("Rows after basic filtering:", nrow(df), "\n")
cat("Clinics (raw):", length(unique(df$clinic)), "\n")
cat("Diag groups:", paste(sort(unique(df$diag_grp)), collapse = ", "), "\n")

tab_clinic <- table(df$clinic)
keep_clinics <- names(tab_clinic)[tab_clinic >= MIN_SITE_N]
df2 <- df[df$clinic %in% keep_clinics, ]

cat("Clinics kept (n >= ", MIN_SITE_N, "): ", length(unique(df2$clinic)), "\n", sep = "")
cat("Rows kept:", nrow(df2), "\n")

# ----------------------------
# 3) Write site CSVs
# ----------------------------
site_dir <- file.path("server", "data", "swespine_sites")
dir.create(site_dir, recursive = TRUE, showWarnings = FALSE)

site_files <- c()
for (cl in sort(unique(df2$clinic))) {
  dcl <- df2[df2$clinic == cl, c("clinic","diag_grp","diag_myelo","age","sex","sexM","bmi","nrs_arm_12m","satisfied_12m")]
  f <- file.path(site_dir, paste0("clinic_", gsub("[^A-Za-z0-9_\\-]", "_", cl), ".csv"))
  write.csv(dcl, f, row.names = FALSE)
  site_files <- c(site_files, f)
}

cat("Wrote site files:", length(site_files), "\n")

# ----------------------------
# 4) Create servers
# ----------------------------
servers <- lapply(site_files, create_server)
cat("Federated servers:", length(servers), "\n")

# ----------------------------
# 5) CENTRAL analyses
# ----------------------------
cat("\n==============================\n")
cat("CENTRAL (Pooled) Results\n")
cat("==============================\n")

central_age <- mean(df2$age, na.rm = TRUE)
central_bmi <- mean(df2$bmi, na.rm = TRUE)
central_sex_tab <- table(df2$sex, useNA = "no")

cat("\nCentral mean age:", central_age, "\n")
cat("Central mean BMI:", central_bmi, "\n")
cat("Central sex distribution:\n")
print(central_sex_tab)

df_tt <- df2[df2$diag_grp %in% c("radiculopathy", "myelopathy"), ]
tt_c <- t.test(age ~ diag_grp, data = df_tt)
cat("\nCentral Welch t-test: age (radic vs myelo)\n")
print(tt_c)

# Central chi-square computed on binary columns (EXACT MATCH TARGET)
tab_c <- table(df2$sexM, df2$diag_myelo)   # rows: sexM 0/1, cols: myelo 0/1
chisq_c <- chisq.test(tab_c, correct = TRUE)
cat("\nCentral chi-square (binary 2x2): sexM vs diag_myelo (Yates)\n")
print(chisq_c)

df_lm <- df2[!is.na(df2$nrs_arm_12m), ]
lm_c <- lm(nrs_arm_12m ~ age + sexM, data = df_lm)
cat("\nCentral LM: NRS arm 12m ~ age + sexM\n")
print(summary(lm_c)$coefficients)

df_glm <- df2[!is.na(df2$satisfied_12m), ]
glm_c <- glm(satisfied_12m ~ age + sexM, data = df_glm, family = binomial)
cat("\nCentral GLM: satisfied 12m ~ age + sexM\n")
print(summary(glm_c)$coefficients)

# ----------------------------
# 6) FEDERATED analyses
# ----------------------------
cat("\n==============================\n")
cat("FEDERATED (Simulated) Results\n")
cat("==============================\n")

fed_age <- fed_numeric(servers, "age")
fed_bmi <- fed_numeric(servers, "bmi")

cat("\nFederated mean age:", fed_age$mean, "(n=", fed_age$n, ")\n")
cat("Federated mean BMI:", fed_bmi$mean, "(n=", fed_bmi$n, ")\n")

tt_f <- fed_welch_t(servers, "age", "diag_grp", "radiculopathy", "myelopathy")
cat("\nFederated Welch t-test: age (radic vs myelo)\n")
print(tt_f)

chisq_f <- fed_chisq_2x2(servers, "sexM", "diag_myelo", correct = TRUE)
cat("\nFederated chi-square (binary 2x2): sexM vs diag_myelo (Yates)\n")
cat("X^2 =", chisq_f$statistic, " df =", chisq_f$df, " p =", chisq_f$p, "\n")
cat("Combined table:\n")
print(chisq_f$table)

lm_f <- fed_lm(servers, nrs_arm_12m ~ age + sexM)
cat("\nFederated LM: NRS arm 12m ~ age + sexM\n")
print(cbind(Estimate = lm_f$beta, StdError = lm_f$se, t = lm_f$t, p = lm_f$p))

glm_f <- fed_logistic_newton(
  formula = satisfied_12m ~ age + sexM,
  servers = servers,
  max_iter = 50,
  tol_score = 1e-8,
  tol_ll = 1e-10,
  verbose = TRUE,
  robust_cluster = TRUE
)

cat("\nFederated GLM: satisfied 12m ~ age + sexM\n")
beta_f <- glm_f$coefficients
se_f <- glm_f$se
print(cbind(Estimate = beta_f, StdError = se_f, z = beta_f / se_f))

# ----------------------------
# 7) Equivalence checks
# ----------------------------
cat("\n==============================\n")
cat("EQUIVALENCE CHECKS\n")
cat("==============================\n")

cat("\nMean age diff (fed - central):", fed_age$mean - central_age, "\n")
cat("Mean BMI diff (fed - central):", fed_bmi$mean - central_bmi, "\n")

beta_lm_c <- coef(lm_c)
beta_lm_f <- lm_f$beta[names(beta_lm_c)]
cat("\nLM coef diff (fed - central):\n")
print(beta_lm_f - beta_lm_c)
cat("LM max abs diff:\n")
print(max(abs(beta_lm_f - beta_lm_c), na.rm = TRUE))

beta_glm_c <- coef(glm_c)
beta_glm_f <- beta_f[names(beta_glm_c)]
cat("\nGLM coef diff (fed - central):\n")
print(beta_glm_f - beta_glm_c)
cat("GLM max abs diff:\n")
print(max(abs(beta_glm_f - beta_glm_c), na.rm = TRUE))

cat("\nChi-square central vs federated:\n")
cat("Central X^2:", unname(chisq_c$statistic), "\n")
cat("Fed     X^2:", chisq_f$statistic, "\n")
cat("Diff:", chisq_f$statistic - unname(chisq_c$statistic), "\n")

cat("\nDone.\n")









# ==========================================================
# Visual comparison tables (gt)
# ==========================================================

suppressPackageStartupMessages({
  library(gt)
  library(dplyr)
})

fmt <- function(x) sprintf("%.6f", x)

# ==========================================================
# linear regression table
# ==========================================================

lm_c_coef <- summary(lm_c)$coefficients
lm_f_coef <- cbind(
  Estimate = lm_f$beta,
  StdError = lm_f$se,
  t = lm_f$t,
  p = lm_f$p
)

terms_lm <- intersect(rownames(lm_c_coef), rownames(lm_f_coef))

lm_table <- tibble(
  Term = terms_lm,
  
  `Central\nEstimate` = lm_c_coef[terms_lm, "Estimate"],
  `Federated\nEstimate` = lm_f_coef[terms_lm, "Estimate"],
  `Abs Diff` = abs(lm_c_coef[terms_lm, "Estimate"] -
                     lm_f_coef[terms_lm, "Estimate"]),
  
  `Central\nSE` = lm_c_coef[terms_lm, "Std. Error"],
  `Federated\nSE` = lm_f_coef[terms_lm, "StdError"],
  
  `Central\np` = lm_c_coef[terms_lm, "Pr(>|t|)"],
  `Federated\np` = lm_f_coef[terms_lm, "p"]
)

lm_table_gt <-
  lm_table %>%
  gt() %>%
  fmt_number(columns = -Term, decimals = 6) %>%
  tab_header(
    title = "Linear Regression Comparison",
    subtitle = "Outcome: NRS arm pain at 12 months"
  ) %>%
  cols_label(
    Term = "Term"
  ) %>%
  tab_spanner(
    label = "Estimates",
    columns = c(`Central\nEstimate`, `Federated\nEstimate`, `Abs Diff`)
  ) %>%
  tab_spanner(
    label = "Standard Errors",
    columns = c(`Central\nSE`, `Federated\nSE`)
  ) %>%
  tab_spanner(
    label = "p-values",
    columns = c(`Central\np`, `Federated\np`)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

print(lm_table_gt)



# ==========================================================
# logistic regression table
# ==========================================================

glm_c_coef <- summary(glm_c)$coefficients
glm_f_coef <- cbind(
  Estimate = beta_f,
  StdError = se_f,
  z = beta_f / se_f,
  p = 2 * (1 - pnorm(abs(beta_f / se_f)))
)

terms_glm <- intersect(rownames(glm_c_coef), rownames(glm_f_coef))

glm_table <- tibble(
  Term = terms_glm,
  
  `Central\nLog-OR` = glm_c_coef[terms_glm, "Estimate"],
  `Federated\nLog-OR` = glm_f_coef[terms_glm, "Estimate"],
  `Abs Diff` = abs(glm_c_coef[terms_glm, "Estimate"] -
                     glm_f_coef[terms_glm, "Estimate"]),
  
  `Central\nSE` = glm_c_coef[terms_glm, "Std. Error"],
  `Federated\nSE` = glm_f_coef[terms_glm, "StdError"],
  
  `Central\np` = glm_c_coef[terms_glm, "Pr(>|z|)"],
  `Federated\np` = glm_f_coef[terms_glm, "p"]
)

glm_table_gt <-
  glm_table %>%
  gt() %>%
  fmt_number(columns = -Term, decimals = 6) %>%
  tab_header(
    title = "Logistic Regression Comparison",
    subtitle = "Outcome: Satisfaction at 12 months"
  ) %>%
  tab_spanner(
    label = "Log-Odds Ratios",
    columns = c(`Central\nLog-OR`, `Federated\nLog-OR`, `Abs Diff`)
  ) %>%
  tab_spanner(
    label = "Standard Errors",
    columns = c(`Central\nSE`, `Federated\nSE`)
  ) %>%
  tab_spanner(
    label = "p-values",
    columns = c(`Central\np`, `Federated\np`)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

print(glm_table_gt)