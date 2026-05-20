# =============================================================================
# EMPIRICAL ILLUSTRATION: HADI Diagnostics Applied to SCL-90-R Data
# Paper: "When Is Hard Class Assignment Defensible?"
# Script: empirical_illustration_SCL90R.R
# Date:   2026-05-04
#
# Requires: mclust, readxl, dplyr, tidyr, ggplot2, e1071, clue
# Data:     4-SCL-90.xls  (9 subscales: SOM OCD IS DEP ANX HOS PHOB PAR PSY)
# =============================================================================

# ── 0.  PATHS & PACKAGES ─────────────────────────────────────────────────────

## Set your data path here  (note the trailing space in "Academic article ")
DATA_PATH <- "/Users/chenxiaohui/Desktop/writing articles/Academic article /HADA STUDY/4-SCL-90.xls"

## Output folder (script's own directory by default; change if needed)
OUT_DIR <- file.path(
  "/Users/chenxiaohui/Desktop/writing articles",
  "Academic article /Bayesian in Psychometrics /output/submiting files"
)
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

## Install missing packages automatically
pkg_needed <- c("mclust", "readxl", "dplyr", "tidyr", "ggplot2", "e1071", "clue", "scales")
pkg_miss   <- pkg_needed[!pkg_needed %in% installed.packages()[, "Package"]]
if (length(pkg_miss)) install.packages(pkg_miss, repos = "https://cloud.r-project.org")

suppressPackageStartupMessages({
  library(mclust)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(e1071)
  library(clue)
  library(scales)
})

cat("\n=== HADI Empirical Illustration: SCL-90-R ===\n\n")

# ── SETTINGS ─────────────────────────────────────────────────────────────────

SUBSCALES  <- c("SOM", "OCD", "IS", "DEP", "ANX", "HOS", "PHOB", "PAR", "PSY")
K_RANGE    <- 1:6          # LPA solutions to fit
K_SELECT   <- 4L         # Set to integer (e.g., 3L) to override auto-selection
THETA      <- 0.70         # Ambiguity threshold: p_ik < THETA = ambiguous
MODEL_TYPE <- "EII"        # mclust covariance model (spherical, equal variance)
SEED       <- 20240101

# Concern thresholds
THR_A  <- c(low = 0.20, mod = 0.40)   # HADI-A: proportion ambiguous
THR_P  <- c(low = 0.05, mod = 0.10)   # HADI-P: pp gap (proportion scale)
THR_M  <- c(low = 0.05, mod = 0.10)   # HADI-M: SD units
THR_S  <- c(low = 0.05, mod = 0.15)   # HADI-S: SII proportion

concern_level <- function(value, thr) {
  if (is.na(value)) return("Unknown")
  if (value < thr["low"]) "Low" else if (value < thr["mod"]) "Moderate" else "High"
}

# ── HELPER: entropy ──────────────────────────────────────────────────────────

calc_entropy <- function(post_mat) {
  # Relative entropy (Ramaswamy et al. 1993)
  N <- nrow(post_mat)
  K <- ncol(post_mat)
  eps  <- .Machine$double.eps
  H_i  <- -rowSums(post_mat * log(post_mat + eps))
  H_max <- log(K)
  1 - sum(H_i) / (N * H_max)
}

# ── HELPER: BCV (between-class variance ratio) ───────────────────────────────

calc_bcv <- function(profiles, weights) {
  # profiles: K x J matrix of class means
  # weights:  length-K vector of class sizes/proportions
  K  <- nrow(profiles)
  J  <- ncol(profiles)
  w  <- weights / sum(weights)
  grand <- colSums(profiles * w)
  # Between-class variance (weighted) averaged over items
  bcv <- mean(colSums(t(t(profiles) - grand)^2 * w))
  bcv
}

# ── SCL-90-R SCORING KEY ─────────────────────────────────────────────────────
# Standard item-to-subscale mapping (Derogatis, 1994)
# Items scored 0-4; subscale score = mean of constituent items

SCL90R_KEY <- list(
  SOM  = c(1,4,12,27,40,42,48,49,52,53,56,58),
  OCD  = c(3,9,10,28,38,45,46,51,55,65),
  IS   = c(6,21,34,36,37,41,61,69,73),
  DEP  = c(5,14,15,20,22,26,29,30,31,32,54,71,79),
  ANX  = c(2,17,23,33,39,57,72,78,80,86),
  HOS  = c(11,24,63,67,74,81),
  PHOB = c(13,25,47,50,70,75,82),
  PAR  = c(8,18,43,68,76,83),
  PSY  = c(7,16,35,62,77,84,85,87,88,90)
)

# ═════════════════════════════════════════════════════════════════════════════
# STEP 1:  LOAD DATA & SCORE SUBSCALES
# ═════════════════════════════════════════════════════════════════════════════

cat("Step 1: Loading data and scoring SCL-90-R subscales...\n")

raw <- suppressWarnings(read_excel(DATA_PATH))
cat(sprintf("  Rows in file: %d  |  Columns: %d\n", nrow(raw), ncol(raw)))

# Detect item columns — accept formats: "1Q", "Q1", "item1", "SCL1", "q1" etc.
item_cols <- names(raw)[grepl("^[0-9]+Q$|^Q[0-9]+$|^[Ii]tem[0-9]+$|^SCL[0-9]+$|^q[0-9]+$",
                               names(raw))]

if (length(item_cols) < 90) {
  # Fallback: columns whose name is purely numeric (e.g., "1","2"...)
  item_cols2 <- names(raw)[grepl("^[0-9]+$", names(raw))]
  if (length(item_cols2) >= 90) item_cols <- item_cols2
}

if (length(item_cols) < 90)
  stop(sprintf(
    "Expected 90 item columns but found %d. Column names: %s",
    length(item_cols), paste(names(raw), collapse = ", ")
  ))

# Extract item number from column name and sort
item_nums <- as.integer(gsub("[^0-9]", "", item_cols))
ord       <- order(item_nums)
item_cols <- item_cols[ord]
item_nums <- item_nums[ord]

# Build item matrix (rows = persons, cols = items 1..90)
items_raw <- raw[, item_cols, drop = FALSE]
items_mat <- suppressWarnings(apply(items_raw, 2, as.numeric))
colnames(items_mat) <- paste0("i", item_nums)

cat(sprintf("  Item columns identified: %d (items %d to %d)\n",
            length(item_cols), min(item_nums), max(item_nums)))

# Score subscales: mean of constituent items
scored <- as.data.frame(lapply(names(SCL90R_KEY), function(sub) {
  idx <- match(paste0("i", SCL90R_KEY[[sub]]), colnames(items_mat))
  rowMeans(items_mat[, idx, drop = FALSE], na.rm = FALSE)
}))
names(scored) <- names(SCL90R_KEY)

# Listwise deletion on subscale scores
dat_full <- scored
dat      <- dat_full[complete.cases(dat_full), ]
dat_mat  <- as.matrix(dat)

N_raw    <- nrow(dat_full)
N_ana    <- nrow(dat)
J        <- ncol(dat)

cat(sprintf("  Persons in file: %d  |  Complete cases: %d  |  Subscales scored: %d\n",
            N_raw, N_ana, J))
cat(sprintf("  Subscales: %s\n", paste(SUBSCALES, collapse = ", ")))


# ═════════════════════════════════════════════════════════════════════════════
# STEP 2:  DESCRIPTIVE STATISTICS
# ═════════════════════════════════════════════════════════════════════════════

cat("\nStep 2: Computing descriptive statistics...\n")

desc_stats <- data.frame(
  Subscale = SUBSCALES,
  N        = apply(dat_mat, 2, function(x) sum(!is.na(x))),
  Mean     = apply(dat_mat, 2, mean,     na.rm = TRUE),
  SD       = apply(dat_mat, 2, sd,       na.rm = TRUE),
  Min      = apply(dat_mat, 2, min,      na.rm = TRUE),
  Max      = apply(dat_mat, 2, max,      na.rm = TRUE),
  Skewness = apply(dat_mat, 2, function(x) skewness(x, na.rm = TRUE)),
  Kurtosis = apply(dat_mat, 2, function(x) kurtosis(x, na.rm = TRUE))
)
desc_stats[, 3:8] <- round(desc_stats[, 3:8], 3)

print(desc_stats, row.names = FALSE)

write.csv(desc_stats,
          file.path(OUT_DIR, "descriptive_statistics.csv"),
          row.names = FALSE)
cat("  -> descriptive_statistics.csv saved.\n")


# ═════════════════════════════════════════════════════════════════════════════
# STEP 3:  FIT LPA K = 1 – 6  (mclust EII)
# ═════════════════════════════════════════════════════════════════════════════

cat("\nStep 3: Fitting LPA models K = 1 to", max(K_RANGE), "(EII)...\n")

set.seed(SEED)
lpa_fits <- vector("list", max(K_RANGE))

for (k in K_RANGE) {
  cat(sprintf("  Fitting K = %d ...", k))
  lpa_fits[[k]] <- Mclust(dat_mat, G = k, modelNames = MODEL_TYPE,
                           verbose = FALSE)
  bic_val <- if (is.null(lpa_fits[[k]])) NA else lpa_fits[[k]]$BIC[1, 1]
  cat(sprintf("  BIC = %.2f\n", bic_val))
}


# ═════════════════════════════════════════════════════════════════════════════
# STEP 4:  MODEL FIT TABLE (BIC, ICL, Entropy, Class sizes)
# ═════════════════════════════════════════════════════════════════════════════

cat("\nStep 4: Building model-fit table...\n")

fit_rows <- lapply(K_RANGE, function(k) {
  fit <- lpa_fits[[k]]
  if (is.null(fit)) return(NULL)

  bic_k <- fit$BIC[1, 1]

  # ICL = BIC − 2 * E(entropy contribution)
  # mclust doesn't compute ICL directly; approximate:
  if (k == 1) {
    icl_k   <- bic_k
    ent_k   <- NA
    min_app <- 1.000
    sizes   <- paste0(N_ana, " (100%)")
  } else {
    post_k  <- fit$z
    H_i     <- -rowSums(post_k * log(post_k + .Machine$double.eps))
    icl_k   <- bic_k - 2 * sum(H_i)
    ent_k   <- round(calc_entropy(post_k), 3)
    cls_n   <- table(fit$classification)
    cls_pct <- round(100 * cls_n / N_ana, 1)
    sizes   <- paste(sprintf("%d (%.1f%%)", cls_n, cls_pct), collapse = " | ")
    min_app <- round(max(diag(table(fit$classification, max.col(post_k,
                               ties.method = "first")))) / N_ana, 3)
  }

  data.frame(
    K          = k,
    BIC        = round(bic_k, 2),
    ICL        = round(icl_k, 2),
    Entropy    = ent_k,
    Class_sizes = sizes,
    stringsAsFactors = FALSE
  )
})
fit_table <- do.call(rbind, fit_rows)

cat("\n  Model fit summary:\n")
print(fit_table, row.names = FALSE)

write.csv(fit_table,
          file.path(OUT_DIR, "model_fit_table.csv"),
          row.names = FALSE)
cat("  -> model_fit_table.csv saved.\n")


# ═════════════════════════════════════════════════════════════════════════════
# STEP 5:  SELECT K
# ═════════════════════════════════════════════════════════════════════════════

cat("\nStep 5: Selecting K...\n")

if (!is.null(K_SELECT) && K_SELECT %in% K_RANGE) {
  k_chosen <- K_SELECT
  cat(sprintf("  K = %d (manually selected via K_SELECT)\n", k_chosen))
} else {
  # Auto: highest BIC among solutions with Entropy >= .80 (if any)
  bic_vec <- sapply(K_RANGE, function(k) {
    if (is.null(lpa_fits[[k]])) return(-Inf)
    lpa_fits[[k]]$BIC[1, 1]
  })
  ent_vec <- sapply(K_RANGE, function(k) {
    fit <- lpa_fits[[k]]
    if (is.null(fit) || k == 1) return(NA)
    calc_entropy(fit$z)
  })

  # Prefer solutions with adequate entropy; fall back to best BIC
  adequate <- which(!is.na(ent_vec) & ent_vec >= 0.70)
  if (length(adequate) > 0) {
    k_chosen <- K_RANGE[adequate[which.max(bic_vec[adequate])]]
    cat(sprintf("  K = %d chosen: highest BIC among solutions with Entropy >= .70\n",
                k_chosen))
  } else {
    k_chosen <- K_RANGE[which.max(bic_vec)]
    cat(sprintf("  K = %d chosen: highest BIC overall (no solution reached Entropy >= .70)\n",
                k_chosen))
  }
  cat("  NOTE: Review fit_table and set K_SELECT manually if preferred.\n")
}

fit_sel <- lpa_fits[[k_chosen]]
K        <- k_chosen


# ═════════════════════════════════════════════════════════════════════════════
# STEP 6:  POSTERIOR MATRIX & HARD ASSIGNMENTS
# ═════════════════════════════════════════════════════════════════════════════

cat(sprintf("\nStep 6: Extracting posteriors and hard assignments (K = %d)...\n", K))

post_mat <- fit_sel$z          # N x K posterior probabilities
hard_cls <- fit_sel$classification  # integer 1..K (modal assignment)

cat(sprintf("  Posterior matrix: %d x %d\n", nrow(post_mat), ncol(post_mat)))
cat("  Hard class counts:\n")
print(table(hard_cls))


# ═════════════════════════════════════════════════════════════════════════════
# STEP 7:  CLASSIFICATION QUALITY  (Entropy, AvePP)
# ═════════════════════════════════════════════════════════════════════════════

cat("\nStep 7: Classification quality indices...\n")

## Global entropy
global_entropy <- calc_entropy(post_mat)

## AvePP per class: mean max-posterior of persons assigned to that class
avepp_by_class <- sapply(1:K, function(k) {
  idx <- which(hard_cls == k)
  if (length(idx) == 0) return(NA_real_)
  mean(apply(post_mat[idx, , drop = FALSE], 1, max))
})
avepp_min <- min(avepp_by_class, na.rm = TRUE)

cat(sprintf("  Global entropy:  %.3f\n", global_entropy))
for (k in 1:K)
  cat(sprintf("  AvePP class %d:   %.3f\n", k, avepp_by_class[k]))
cat(sprintf("  AvePP_min:       %.3f  [benchmark >= 0.70]\n", avepp_min))


# ═════════════════════════════════════════════════════════════════════════════
# STEP 8:  HADI-A  (Ambiguity Burden)
# ═════════════════════════════════════════════════════════════════════════════

cat(sprintf("\nStep 8: HADI-A (ambiguity burden, theta = %.2f)...\n", THETA))

max_post   <- apply(post_mat, 1, max)
ambig_flag <- max_post < THETA

OAB <- mean(ambig_flag)                   # overall ambiguity burden
FAB <- mean(max_post[ambig_flag] < THETA) # fraction below THETA (always 1 if computed this way)
# More meaningful FAB: mean max-posterior of ambiguous persons
FAB_mean_p <- if (sum(ambig_flag) > 0) mean(max_post[ambig_flag]) else NA

cat(sprintf("  OAB (overall ambiguity burden):         %.3f  (%.1f%% of persons)\n",
            OAB, OAB * 100))
cat(sprintf("  Mean max-posterior among ambiguous:     %.3f\n", FAB_mean_p))
cat(sprintf("  Concern level (OAB):  %s  [Low<20%% | Mod 20-40%% | High>=40%%]\n",
            concern_level(OAB, THR_A)))


# ═════════════════════════════════════════════════════════════════════════════
# STEP 9:  HADI-P  (Prevalence Distortion)
# ═════════════════════════════════════════════════════════════════════════════

cat("\nStep 9: HADI-P (prevalence distortion)...\n")

# Hard-assigned prevalences
prev_hard <- as.numeric(table(hard_cls)) / N_ana

# Posterior-weighted prevalences (ML reference)
prev_wt   <- colMeans(post_mat)

# Signed distortion per class (hard − weighted)
hadi_p_by_class <- prev_hard - prev_wt

# Scalar: max absolute distortion
hadi_p_max <- max(abs(hadi_p_by_class))

cat("  Class prevalences (hard-assigned vs. posterior-weighted):\n")
prev_df <- data.frame(
  Class    = 1:K,
  Hard_pct = round(prev_hard * 100, 2),
  Wt_pct   = round(prev_wt   * 100, 2),
  Gap_pp   = round(hadi_p_by_class * 100, 2)
)
print(prev_df, row.names = FALSE)
cat(sprintf("  max|HADI-P|:  %.4f (%.2f pp)\n", hadi_p_max, hadi_p_max * 100))
cat(sprintf("  Concern level (HADI-P):  %s  [Low<5pp | Mod 5-10pp | High>=10pp]\n",
            concern_level(hadi_p_max, THR_P)))


# ═════════════════════════════════════════════════════════════════════════════
# STEP 10:  HADI-M  (Profile Mean Distortion)
# ═════════════════════════════════════════════════════════════════════════════

cat("\nStep 10: HADI-M (profile mean distortion)...\n")

# Hard-assigned profile means  (K x J)
mu_hard <- t(sapply(1:K, function(k) {
  idx <- which(hard_cls == k)
  if (length(idx) == 0) rep(NA, J) else colMeans(dat_mat[idx, , drop = FALSE])
}))
rownames(mu_hard) <- paste0("C", 1:K)
colnames(mu_hard) <- SUBSCALES

# Posterior-weighted profile means  (K x J)
mu_wt <- t(sapply(1:K, function(k) {
  w   <- post_mat[, k]
  colSums(dat_mat * w) / sum(w)
}))
rownames(mu_wt) <- paste0("C", 1:K)
colnames(mu_wt) <- SUBSCALES

# Pooled SD per subscale (from full sample)
sd_pool <- apply(dat_mat, 2, sd)

# Absolute difference in SD units per class x subscale
delta_sd <- abs(mu_hard - mu_wt) / matrix(sd_pool, nrow = K, ncol = J, byrow = TRUE)

# Overall HADI-M: mean across all K*J cells
hadi_m <- mean(delta_sd, na.rm = TRUE)

# Per-class mean distortion
hadi_m_by_class <- rowMeans(delta_sd, na.rm = TRUE)

cat(sprintf("  Overall HADI-M:  %.4f SD units\n", hadi_m))
for (k in 1:K)
  cat(sprintf("  Class %d HADI-M:  %.4f SD\n", k, hadi_m_by_class[k]))
cat(sprintf("  Concern level (HADI-M):  %s  [Low<0.05SD | Mod 0.05-0.10SD | High>=0.10SD]\n",
            concern_level(hadi_m, THR_M)))


# ═════════════════════════════════════════════════════════════════════════════
# STEP 11:  HADI-S  (Separation Inflation Index)
# ═════════════════════════════════════════════════════════════════════════════

cat("\nStep 11: HADI-S (separation inflation)...\n")

# BCV hard-assigned
n_hard <- as.numeric(table(hard_cls))
bcv_hard <- calc_bcv(mu_hard, n_hard)

# BCV posterior-weighted
bcv_wt <- calc_bcv(mu_wt, prev_wt)

# SII = (BCV_hard − BCV_wt) / BCV_wt   [inflation fraction]
sii <- (bcv_hard - bcv_wt) / bcv_wt

cat(sprintf("  BCV hard-assigned:       %.4f\n", bcv_hard))
cat(sprintf("  BCV posterior-weighted:  %.4f\n", bcv_wt))
cat(sprintf("  SII (inflation):         %.4f  (%.1f%%)\n", sii, sii * 100))
cat(sprintf("  Concern level (HADI-S):  %s  [Low<5%% | Mod 5-15%% | High>=15%%]\n",
            concern_level(abs(sii), THR_S)))


# ═════════════════════════════════════════════════════════════════════════════
# STEP 12:  HADI-C  (Composite via max-pooling)
# ═════════════════════════════════════════════════════════════════════════════

cat("\nStep 12: Composite HADI-C (max-pooling)...\n")

concern_A <- concern_level(OAB,        THR_A)
concern_P <- concern_level(hadi_p_max, THR_P)
concern_M <- concern_level(hadi_m,     THR_M)
concern_S <- concern_level(abs(sii),   THR_S)

# Ordinal max: High > Moderate > Low
concern_ord <- function(x) match(x, c("Low", "Moderate", "High"))
concerns_vec <- c(concern_A, concern_P, concern_M, concern_S)
hadi_c <- concerns_vec[which.max(sapply(concerns_vec, concern_ord))]

cat(sprintf("  HADI-A concern: %s\n", concern_A))
cat(sprintf("  HADI-P concern: %s\n", concern_P))
cat(sprintf("  HADI-M concern: %s\n", concern_M))
cat(sprintf("  HADI-S concern: %s\n", concern_S))
cat(sprintf("  HADI-C (composite, max-pooling): %s\n", hadi_c))

action_msg <- switch(hadi_c,
  "Low"      = "Hard assignment is defensible. Proceed with standard reporting.",
  "Moderate" = "Hard assignment introduces moderate distortion. Report both hard and weighted estimates; flag uncertainty.",
  "High"     = "Hard assignment is NOT recommended. Use posterior-weighted estimation for all downstream analyses.",
  "Hard assignment concern level undetermined."
)
cat(sprintf("\n  Interpretation: %s\n", action_msg))


# ═════════════════════════════════════════════════════════════════════════════
# STEP 13:  EXPORT DIAGNOSTIC SUMMARY & PROFILE TABLE
# ═════════════════════════════════════════════════════════════════════════════

cat("\nStep 13: Exporting diagnostic tables...\n")

## Table 1: HADI diagnostic summary
hadi_summary <- data.frame(
  Index       = c("Entropy", "AvePP_min",
                  "HADI-A (OAB)", "HADI-P (max|gap|)", "HADI-M (mean delta SD)", "HADI-S (SII)",
                  "HADI-C (composite)"),
  Value       = c(round(global_entropy, 3), round(avepp_min, 3),
                  sprintf("%.3f (%.1f%%)", OAB, OAB * 100),
                  sprintf("%.4f (%.2f pp)", hadi_p_max, hadi_p_max * 100),
                  sprintf("%.4f SD", hadi_m),
                  sprintf("%.4f (%.1f%%)", sii, sii * 100),
                  hadi_c),
  Concern     = c(ifelse(global_entropy >= .80, "—", "Low entropy"),
                  ifelse(avepp_min >= .70, "—", "Low AvePP"),
                  concern_A, concern_P, concern_M, concern_S, hadi_c),
  Benchmark   = c("≥ .80", "≥ .70",
                  "Low < 20% | Mod 20-40% | High ≥ 40%",
                  "Low < 5 pp | Mod 5-10 pp | High ≥ 10 pp",
                  "Low < 0.05 SD | Mod 0.05-0.10 SD | High ≥ 0.10 SD",
                  "Low < 5% | Mod 5-15% | High ≥ 15%",
                  "max(A, P, M, S)"),
  stringsAsFactors = FALSE
)

write.csv(hadi_summary,
          file.path(OUT_DIR, "hadi_diagnostic_summary.csv"),
          row.names = FALSE)
cat("  -> hadi_diagnostic_summary.csv saved.\n")

## Table 2: Hard vs weighted profiles (long format for all classes)
prof_rows <- lapply(1:K, function(k) {
  data.frame(
    Class         = k,
    Subscale      = SUBSCALES,
    Mean_hard     = round(mu_hard[k, ], 3),
    Mean_weighted = round(mu_wt[k, ], 3),
    Delta_SD      = round(delta_sd[k, ], 4),
    stringsAsFactors = FALSE
  )
})
prof_table <- do.call(rbind, prof_rows)

write.csv(prof_table,
          file.path(OUT_DIR, "hard_vs_weighted_profiles.csv"),
          row.names = FALSE)
cat("  -> hard_vs_weighted_profiles.csv saved.\n")

## Print summary to console
cat("\n  HADI Diagnostic Summary:\n")
print(hadi_summary, row.names = FALSE)
cat("\n  Hard vs. Weighted Profile Means (first few rows):\n")
print(head(prof_table, min(9, nrow(prof_table))), row.names = FALSE)


# ═════════════════════════════════════════════════════════════════════════════
# STEP 14: PROFILE COMPARISON FIGURE
# ═════════════════════════════════════════════════════════════════════════════

cat("\nStep 14: Creating profile comparison figure...\n")

## Build long-format data for plotting
fig_data <- bind_rows(
  # Hard-assigned profiles
  as.data.frame(mu_hard) %>%
    mutate(Class = paste0("Class ", 1:K), Estimation = "Hard-Assigned") %>%
    pivot_longer(cols = all_of(SUBSCALES), names_to = "Subscale", values_to = "Mean"),
  # Posterior-weighted profiles
  as.data.frame(mu_wt) %>%
    mutate(Class = paste0("Class ", 1:K), Estimation = "Posterior-Weighted") %>%
    pivot_longer(cols = all_of(SUBSCALES), names_to = "Subscale", values_to = "Mean")
)

# Factor for ordered subscales
fig_data$Subscale <- factor(fig_data$Subscale, levels = SUBSCALES)
fig_data$Estimation <- factor(fig_data$Estimation,
                               levels = c("Hard-Assigned", "Posterior-Weighted"))

# Class-level annotation: n and prevalence distortion
class_labels <- sapply(1:K, function(k) {
  sprintf("Class %d\nn = %d (%.1f%%)\ngap = %.2f pp",
          k, n_hard[k], prev_hard[k] * 100, hadi_p_by_class[k] * 100)
})
names(class_labels) <- paste0("Class ", 1:K)

# Caption text
cap_txt <- sprintf(
  paste("SCL-90-R latent profile analysis (K = %d, EII model; N = %d).",
        "Hard-assigned (solid) vs. posterior-weighted (dashed) class means.",
        "HADI-C = %s  |  Entropy = %.3f  |  AvePP_min = %.3f  |  SII = %.1f%%"),
  K, N_ana, hadi_c, global_entropy, avepp_min, sii * 100
)

pal_estimation <- c("Hard-Assigned"       = "#C0392B",
                    "Posterior-Weighted"   = "#2D6A4F")
lty_estimation <- c("Hard-Assigned"       = "solid",
                    "Posterior-Weighted"   = "dashed")

p_profile <- ggplot(fig_data, aes(x = Subscale, y = Mean,
                                   colour = Estimation,
                                   linetype = Estimation,
                                   group = interaction(Class, Estimation))) +
  geom_line(linewidth = 0.9, alpha = 0.9) +
  geom_point(size = 2.2, alpha = 0.9) +
  facet_wrap(~ Class, nrow = 1, labeller = labeller(Class = class_labels)) +
  scale_colour_manual(values = pal_estimation, name = "Estimation method") +
  scale_linetype_manual(values = lty_estimation, name = "Estimation method") +
  labs(
    title    = "SCL-90-R Latent Profile Comparison: Hard-Assigned vs. Posterior-Weighted Means",
    subtitle = sprintf("K = %d classes | HADI-C = %s | Entropy = %.3f | max|HADI-P| = %.2f pp | HADI-M = %.3f SD | SII = %.1f%%",
                       K, hadi_c, global_entropy, hadi_p_max * 100, hadi_m, sii * 100),
    x        = "SCL-90-R Subscale",
    y        = "Mean Score",
    caption  = cap_txt
  ) +
  theme_bw(base_size = 11, base_family = "serif") +
  theme(
    plot.title    = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 8.5, colour = "grey30"),
    plot.caption  = element_text(size = 7.5, colour = "grey40", hjust = 0),
    axis.text.x   = element_text(angle = 45, hjust = 1, size = 9),
    strip.text    = element_text(size = 8),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

fig_path <- file.path(OUT_DIR, "Figure_Profile_Comparison.png")
ggsave(fig_path, p_profile,
       width = max(7, K * 2.5), height = 5, dpi = 300, units = "in")
cat(sprintf("  -> Figure_Profile_Comparison.png saved.\n"))

# Also save as PDF for publication
pdf_path <- file.path(OUT_DIR, "Figure_Profile_Comparison.pdf")
ggsave(pdf_path, p_profile,
       width = max(7, K * 2.5), height = 5, units = "in")
cat(sprintf("  -> Figure_Profile_Comparison.pdf saved.\n"))


# ═════════════════════════════════════════════════════════════════════════════
# STEP 15: FULL CONSOLE REPORT
# ═════════════════════════════════════════════════════════════════════════════

cat("\n")
cat(strrep("=", 70), "\n")
cat("HADI EMPIRICAL ILLUSTRATION — COMPLETE RESULTS SUMMARY\n")
cat(strrep("=", 70), "\n")
cat(sprintf("Dataset:    SCL-90-R  (N = %d complete cases, %d subscales)\n", N_ana, J))
cat(sprintf("Model:      LPA EII,  K = %d classes selected\n", K))
cat(strrep("-", 70), "\n")
cat("CLASSIFICATION QUALITY\n")
cat(sprintf("  Entropy:           %.3f  %s\n",
            global_entropy, ifelse(global_entropy >= .80, "[Good]", "[<.80 — borderline]")))
cat(sprintf("  AvePP_min:         %.3f  %s\n",
            avepp_min, ifelse(avepp_min >= .70, "[Good]", "[<.70 — concern]")))
cat(strrep("-", 70), "\n")
cat("ASSIGNMENT ADEQUACY (HADI)\n")
cat(sprintf("  HADI-A (OAB):      %.3f (%.1f%%)  — Concern: %s\n",
            OAB, OAB * 100, concern_A))
cat(sprintf("  HADI-P (max|gap|): %.4f (%.2f pp) — Concern: %s\n",
            hadi_p_max, hadi_p_max * 100, concern_P))
cat(sprintf("  HADI-M (mean ΔSD): %.4f SD         — Concern: %s\n",
            hadi_m, concern_M))
cat(sprintf("  HADI-S (SII):      %.4f (%.1f%%)  — Concern: %s\n",
            sii, sii * 100, concern_S))
cat(strrep("-", 70), "\n")
cat(sprintf("  HADI-C (composite): %s\n", hadi_c))
cat(sprintf("  Action:  %s\n", action_msg))
cat(strrep("=", 70), "\n")
cat("\nFiles saved to:\n")
cat(sprintf("  %s\n", OUT_DIR))
cat("  - descriptive_statistics.csv\n")
cat("  - model_fit_table.csv\n")
cat("  - hadi_diagnostic_summary.csv\n")
cat("  - hard_vs_weighted_profiles.csv\n")
cat("  - Figure_Profile_Comparison.png\n")
cat("  - Figure_Profile_Comparison.pdf\n")
cat("\nDone.\n")
