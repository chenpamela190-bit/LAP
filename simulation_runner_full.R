# =============================================================================
# simulation_runner_full.R
# Full Simulation for HADI: Hard-Assignment Distortion Index
# =============================================================================
#
# ─── HOW TO RUN (read this before anything else) ──────────────────────────────
#
# STEP 1 — Open this file in RStudio.
#
# STEP 2 — Edit ONLY the USER SETTINGS block (Section 0, lines below).
#           Everything else can stay as-is.
#
# STEP 3 — Run the script using SOURCE, not line-by-line Run:
#             Keyboard shortcut:  Cmd + Shift + S  (Mac)
#                                 Ctrl + Shift + S  (Windows)
#             Or: click the "Source" button at the top-right of the editor.
#
#           WHY SOURCE and not Run?
#           "Run" executes only the selected line(s). "Source" executes the
#           entire file top-to-bottom, so all functions are defined before
#           the simulation loop tries to use them.
#
# STEP 4 — Always start with RUN_MODE <- "test" first.
#           This runs 5 reps per condition (~15 min) to check for errors.
#           Only switch to "medium" or "full" after the test completes cleanly.
#
# ESTIMATED RUN TIMES (on a typical modern laptop, single core):
#   "test"   :  5 reps × 162 conditions ≈  12–15 minutes
#   "medium" : 200 reps × 162 conditions ≈   6–7 hours
#   "full"   : 500 reps × 162 conditions ≈ 15–18 hours
#
# TIP: For "medium" and "full" runs, start the script before bed or at the
# start of a working day, and let it run in the background.
#
# =============================================================================


# ╔═════════════════════════════════════════════════════════════════════════════╗
# ║                    SECTION 0 — USER SETTINGS                               ║
# ║         ← EDIT ONLY THIS BLOCK. Do not change anything else. →             ║
# ╚═════════════════════════════════════════════════════════════════════════════╝

# ── Run mode ──────────────────────────────────────────────────────────────────
# "test"   → 5 reps per condition.  Run this first to check for errors.
# "medium" → 200 reps per condition. Use for exploratory / manuscript-draft results.
# "full"   → 500 reps per condition. Use for final publication-ready results.
RUN_MODE <- "medium"   # <── change this when you are ready

# ── Random seed ───────────────────────────────────────────────────────────────
# Use the same seed every time to reproduce identical results.
MASTER_SEED <- 2025

# ── Script folder ─────────────────────────────────────────────────────────────
# Leave as NULL to auto-detect (recommended). If auto-detect fails, paste the
# full folder path between the quotes below, e.g.:
#   SCRIPT_DIR <- "/Users/yourname/Desktop/my_project"
SCRIPT_DIR <- NULL

# ── Save a checkpoint every N conditions? ────────────────────────────────────
# The script saves progress to disk every CHECKPOINT_EVERY conditions.
# If R crashes halfway, you will not lose everything — only the last
# CHECKPOINT_EVERY conditions of work.
CHECKPOINT_EVERY <- 10   # save progress every 10 conditions (recommended)

# ╚═════════════════════════════════════════════════════════════════════════════╝
#   END OF USER SETTINGS
# ╔═════════════════════════════════════════════════════════════════════════════╝


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 1 ── Setup: working directory, packages, source hadi_functions.R
# ─────────────────────────────────────────────────────────────────────────────

# ── 1a. Set working directory ─────────────────────────────────────────────────
# Try three methods in order: rstudioapi → sys.frame → hard-coded path.

if (is.null(SCRIPT_DIR)) {
  # Method 1: rstudioapi (works when file is open in the RStudio editor)
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    path_try <- tryCatch(
      dirname(rstudioapi::getSourceEditorContext()$path),
      error = function(e) NULL
    )
    if (!is.null(path_try) && nchar(path_try) > 1 && dir.exists(path_try))
      SCRIPT_DIR <- path_try
  }
}

if (is.null(SCRIPT_DIR)) {
  # Method 2: sys.frame (works when called via source())
  path_try <- tryCatch({
    p <- NULL
    for (fr in rev(sys.frames())) {
      sf <- attr(attr(fr, "srcref"), "srcfile")$filename
      if (!is.null(sf) && nchar(sf) > 1) { p <- dirname(sf); break }
    }
    p
  }, error = function(e) NULL)
  if (!is.null(path_try) && dir.exists(path_try)) SCRIPT_DIR <- path_try
}

if (is.null(SCRIPT_DIR)) {
  # Method 3: hard-coded fallback for this machine
  hard_path <- paste0("/Users/chenxiaohui/Desktop/writing articles/",
                      "Academic article /Bayesian in Psychometrics /",
                      "output/submiting files")
  if (dir.exists(hard_path)) SCRIPT_DIR <- hard_path
}

if (is.null(SCRIPT_DIR)) {
  stop("Cannot find the script folder. Please set SCRIPT_DIR manually in Section 0.")
}
setwd(SCRIPT_DIR)
message("Working directory: ", getwd())

# ── 1b. Install and load packages ─────────────────────────────────────────────
for (pkg in c("mclust", "clue")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing: ", pkg)
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  library(pkg, character.only = TRUE)
}

# ── 1c. Source hadi_functions.R ───────────────────────────────────────────────
hadi_path <- file.path(SCRIPT_DIR, "hadi_functions.R")
if (!file.exists(hadi_path))
  stop("Cannot find hadi_functions.R in: ", SCRIPT_DIR)
source(hadi_path)
message("hadi_functions.R loaded.")

# ── 1d. Create output folder ──────────────────────────────────────────────────
results_dir <- file.path(SCRIPT_DIR, "results")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 2 ── Simulation design (162 conditions)
# ─────────────────────────────────────────────────────────────────────────────
# Design factors:
#   N (sample size)       : 300, 1000, 3000
#   delta (class spacing) : 0.5, 0.8, 1.0, 1.2, 1.5, 3.0
#   balance (class sizes) : balanced / moderate imbalance / severe imbalance
#   sigma2 (within noise) : 0.5, 1.0, 2.0  (= high / medium / low reliability)
#
# Fixed parameters:
#   K = 3 classes, J = 9 indicators
#   Profile means: Class k has mean (k-1)*delta on all J indicators (parallel)
#   Within-class covariance: sigma2 * I_J  (spherical, equal across classes)
#
# Effective Mahalanobis distance between adjacent classes:
#   D^2 = J * delta^2 / sigma2
#   Example: delta=1.0, sigma2=1.0 → D^2 = 9.00 (D = 3.00)

K_TRUE <- 3   # number of latent classes
J_IND  <- 9   # number of indicators

# Reps per condition depend on run mode
N_REPS <- switch(RUN_MODE,
  test   =   5,
  medium = 200,
  full   = 500,
  stop("RUN_MODE must be 'test', 'medium', or 'full'.")
)
message(sprintf("Run mode: %s  |  %d reps per condition", RUN_MODE, N_REPS))

# ── Design factor lists ───────────────────────────────────────────────────────
sample_sizes <- c(300, 1000, 3000)

separation_levels <- list(
  list(label = "d050", delta = 0.50),   # D^2 = 2.25 / 4.50 / 1.12 across noise levels
  list(label = "d080", delta = 0.80),
  list(label = "d100", delta = 1.00),
  list(label = "d120", delta = 1.20),
  list(label = "d150", delta = 1.50),
  list(label = "d300", delta = 3.00)
)

balance_levels <- list(
  list(label = "balanced",  pi = c(1/3, 1/3, 1/3)),
  list(label = "mod_imbal", pi = c(0.60, 0.30, 0.10)),
  list(label = "sev_imbal", pi = c(0.75, 0.20, 0.05))
)

# sigma2 = within-class variance (all J indicators, all K classes)
# Reliability interpretation: Rel ≈ delta^2 / (delta^2 + sigma2) for adjacent classes
noise_levels <- list(
  list(label = "high_rel", sigma2 = 0.50),   # high indicator reliability
  list(label = "med_rel",  sigma2 = 1.00),   # medium reliability (pilot baseline)
  list(label = "low_rel",  sigma2 = 2.00)    # low indicator reliability
)

# ── Build full factorial condition table ──────────────────────────────────────
conditions <- vector("list", 0)
for (N in sample_sizes) {
  for (sep in separation_levels) {
    for (bal in balance_levels) {
      for (noi in noise_levels) {
        D2 <- J_IND * sep$delta^2 / noi$sigma2   # Mahalanobis distance squared
        cid <- paste0("N", N, "_", sep$label, "_", bal$label, "_", noi$label)
        conditions <- c(conditions, list(list(
          condition_id = cid,
          N            = N,
          delta        = sep$delta,
          balance      = bal$label,
          pi           = bal$pi,
          noise        = noi$label,
          sigma2       = noi$sigma2,
          D2_adjacent  = round(D2, 3)
        )))
      }
    }
  }
}

n_conditions  <- length(conditions)
total_reps    <- n_conditions * N_REPS
message(sprintf("Design: %d conditions × %d reps = %d total replications",
                n_conditions, N_REPS, total_reps))


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 3 ── Helper functions
# ─────────────────────────────────────────────────────────────────────────────
# These are the same functions as in simulation_runner_pilot.R, with two
# improvements from the pilot review:
#   (1) fit_lpa()    : min mixing-weight threshold raised to 0.01
#   (2) match_labels(): returns match_cost_max for label-switch detection

# ── 3a. generate_data() ──────────────────────────────────────────────────────
# Draws true class labels and indicator data for one replication.
# Y_i | class=k  ~  MVN( mu_k ,  sigma2 * I_J )
# mu_k = rep((k-1)*delta, J)   [parallel profiles]

generate_data <- function(N, K, J, pi, delta, sigma2) {
  # True class mean vectors (J × K matrix; each column = one class)
  true_means <- matrix(0, nrow = J, ncol = K)
  for (k in seq_len(K)) true_means[, k] <- (k - 1) * delta

  # Assign participants to classes (multinomial draw)
  true_labels <- sample(seq_len(K), size = N, replace = TRUE, prob = pi)

  # Generate indicator data class by class
  # Since Sigma = sigma2 * I, each indicator is independent N(mu_kj, sigma2)
  Y <- matrix(NA_real_, nrow = N, ncol = J,
              dimnames = list(NULL, paste0("Y", seq_len(J))))
  for (k in seq_len(K)) {
    idx <- which(true_labels == k)
    if (length(idx) == 0L) next
    noise <- matrix(rnorm(length(idx) * J, mean = 0, sd = sqrt(sigma2)),
                    nrow = length(idx), ncol = J)
    Y[idx, ] <- noise + matrix(rep(true_means[, k], each = length(idx)),
                               nrow = length(idx))
  }
  list(Y = Y, true_labels = true_labels, true_means = true_means)
}

# ── 3b. fit_lpa() ────────────────────────────────────────────────────────────
# Fits a 3-class EII Gaussian mixture model using mclust.
# "EII" = spherical, equal-variance covariance — matches our DGP exactly.
# Returns a list: converged (logical), fit (mclust object), fail_reason (string).

fit_lpa <- function(Y, K) {
  fit <- tryCatch(
    mclust::Mclust(Y, G = K, modelNames = "EII", verbose = FALSE),
    error   = function(e) NULL,
    warning = function(w) suppressWarnings(
      mclust::Mclust(Y, G = K, modelNames = "EII", verbose = FALSE))
  )

  if (is.null(fit))
    return(list(converged = FALSE, fit = NULL,
                fail_reason = "mclust returned NULL"))

  if (is.null(fit$G) || fit$G != K)
    return(list(converged = FALSE, fit = fit,
                fail_reason = paste0("found ", fit$G, " classes, expected ", K)))

  # Raised from 0.005 to 0.01 after pilot: catches poorly-estimated small classes
  min_pi <- min(fit$parameters$pro, na.rm = TRUE)
  if (min_pi < 0.01)
    return(list(converged = FALSE, fit = fit,
                fail_reason = paste0("degenerate: min weight = ",
                                     round(min_pi, 5))))

  list(converged = TRUE, fit = fit, fail_reason = NA_character_)
}

# ── 3c. match_labels() ───────────────────────────────────────────────────────
# Corrects label switching by aligning estimated class means to true DGP means.
# Uses the Hungarian algorithm (clue::solve_LSAP) for the optimal assignment.
# Returns: reordered posterior matrix + match_cost_max (quality diagnostic).

match_labels <- function(fit, true_means, K) {
  est_means <- fit$parameters$mean   # J × K matrix from mclust

  # Build K × K cost matrix: cost[i,j] = distance from true class i to est class j
  cost_mat <- matrix(NA_real_, K, K)
  for (i in seq_len(K))
    for (j in seq_len(K))
      cost_mat[i, j] <- sqrt(sum((true_means[, i] - est_means[, j])^2))

  # Hungarian algorithm for optimal matching
  if (requireNamespace("clue", quietly = TRUE)) {
    perm <- as.integer(clue::solve_LSAP(cost_mat))
  } else {
    # Fallback: exhaustive search (K=3 only, 6 permutations)
    perms <- list(c(1L,2L,3L),c(1L,3L,2L),c(2L,1L,3L),
                  c(2L,3L,1L),c(3L,1L,2L),c(3L,2L,1L))
    costs <- sapply(perms, function(p) sum(diag(cost_mat[, p, drop=FALSE])))
    perm  <- perms[[which.min(costs)]]
  }

  P_matched <- fit$z[, perm, drop = FALSE]
  colnames(P_matched) <- paste0("Class", seq_len(K))

  # match_cost_max: worst Euclidean distance in the matched pairs
  # Large values signal a label-switching failure or badly wrong local optimum
  match_cost_max <- max(diag(cost_mat[, perm, drop = FALSE]))

  list(posteriors     = P_matched,
       permutation    = perm,
       match_cost_max = match_cost_max)
}

# ── 3d. compute_entropy() ────────────────────────────────────────────────────
# Mean Shannon entropy across N participants.
# H_i = -sum_k(p_ik * log(p_ik))  ;  max = log(K) ≈ 1.099 for K=3

compute_entropy <- function(P) {
  P_safe <- pmax(P, 1e-300)
  mean(-rowSums(P_safe * log(P_safe)), na.rm = TRUE)
}

# ── 3e. run_one_replication() ─────────────────────────────────────────────────
# Runs one complete replication: generate → fit → label-match → HADI → metrics.
# Returns a named list (one row of the results CSV).

run_one_replication <- function(cond, rep, seed) {
  set.seed(seed)

  # Step 1: Generate data
  dat <- generate_data(N = cond$N, K = K_TRUE, J = J_IND,
                       pi = cond$pi, delta = cond$delta, sigma2 = cond$sigma2)

  # Step 2: Fit LPA
  lpa <- fit_lpa(dat$Y, K = K_TRUE)

  # Base row (always returned, even on failure)
  base <- list(
    condition_id = cond$condition_id,
    N            = cond$N,
    delta        = cond$delta,
    balance      = cond$balance,
    noise        = cond$noise,
    sigma2       = cond$sigma2,
    D2_adjacent  = cond$D2_adjacent,
    pi1          = cond$pi[1],
    pi2          = cond$pi[2],
    pi3          = cond$pi[3],
    rep          = rep,
    converged    = as.integer(lpa$converged),
    fail_reason  = if (lpa$converged) NA_character_ else lpa$fail_reason
  )

  # NA template for all metrics (used when fit fails)
  na_m <- list(
    match_cost_max       = NA_real_,
    label_switch_suspect = NA_integer_,
    accuracy             = NA_real_,
    mean_entropy         = NA_real_,
    oab = NA_real_, fab = NA_real_, hadi_a_cont = NA_real_,
    max_hadi_p_pp = NA_real_,  concern_p    = NA_character_,
    hadi_m_mean   = NA_real_,  concern_m    = NA_character_,
    sii_mean      = NA_real_,  sii_direction = NA_character_,
    concern_s     = NA_character_,
    avepp_1 = NA_real_, avepp_2 = NA_real_, avepp_3 = NA_real_,
    avepp_min  = NA_real_,  concern_avepp = NA_character_,
    hadi_score = NA_real_,  hadi_c        = NA_character_
  )

  if (!lpa$converged) return(c(base, na_m))

  # Step 3: Correct label switching
  matched <- match_labels(lpa$fit, dat$true_means, K = K_TRUE)

  # Label-switch suspect: worst match distance > half a between-class gap
  gap_threshold        <- 0.5 * cond$delta * sqrt(J_IND)
  label_switch_suspect <- as.integer(matched$match_cost_max > gap_threshold)

  P <- matched$posteriors   # N × K, aligned to true classes

  # Step 4: Truth-referenced accuracy
  accuracy <- mean(max.col(P, ties.method = "first") == dat$true_labels,
                   na.rm = TRUE)

  # Step 5: Shannon entropy
  mean_entropy <- compute_entropy(P)

  # Step 6: Run HADI
  # standardize_indicators = TRUE → HADI-M expressed in SD units,
  # directly comparable to the published M_cut thresholds (.05 / .10 SD)
  hadi_res <- tryCatch(
    hadi(posteriors             = P,
         indicators             = dat$Y,
         threshold              = 0.70,
         focal_class            = "auto",
         standardize_indicators = TRUE),
    error = function(e) {
      message("  hadi() error: ", e$message)
      NULL
    }
  )

  if (is.null(hadi_res)) return(c(base, na_m))

  # Step 7: Collect output
  av <- hadi_res$avepp$values

  c(base, list(
    match_cost_max       = matched$match_cost_max,
    label_switch_suspect = label_switch_suspect,
    accuracy             = accuracy,
    mean_entropy         = mean_entropy,
    oab          = hadi_res$hadi_a$oab,
    fab          = hadi_res$hadi_a$fab,
    hadi_a_cont  = hadi_res$hadi_a$hadi_a_cont,
    max_hadi_p_pp = hadi_res$hadi_p$max_abs_pi * 100,
    concern_p     = hadi_res$hadi_p$concern,
    hadi_m_mean   = hadi_res$hadi_m$overall_distortion,
    concern_m     = hadi_res$hadi_m$concern,
    sii_mean      = hadi_res$hadi_s$sii_mean,
    sii_direction = hadi_res$hadi_s$sii_direction,
    concern_s     = hadi_res$hadi_s$concern,
    avepp_1       = unname(av[1]),
    avepp_2       = unname(av[2]),
    avepp_3       = unname(av[3]),
    avepp_min     = min(av, na.rm = TRUE),
    concern_avepp = hadi_res$avepp$concern,
    hadi_score    = hadi_res$hadi_c$score,
    hadi_c        = hadi_res$hadi_c$concern
  ))
}


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 4 ── Main simulation loop
# ─────────────────────────────────────────────────────────────────────────────
# Loops over all conditions and replications.
# Saves a checkpoint to disk every CHECKPOINT_EVERY conditions so that a crash
# does not lose all progress.

cat("\n")
cat("═══════════════════════════════════════════════════════════════════\n")
cat(sprintf("  HADI FULL SIMULATION  |  mode: %s\n", toupper(RUN_MODE)))
cat(sprintf("  %d conditions × %d reps = %d replications\n",
            n_conditions, N_REPS, total_reps))
cat("═══════════════════════════════════════════════════════════════════\n\n")

results_list <- vector("list", total_reps)
row_idx      <- 0L
t_all_start  <- proc.time()

# Checkpoint file: overwritten after every CHECKPOINT_EVERY conditions
checkpoint_path <- file.path(results_dir,
                             paste0("checkpoint_", RUN_MODE, ".csv"))

for (ci in seq_along(conditions)) {
  cond         <- conditions[[ci]]
  t_cond_start <- proc.time()
  n_fail       <- 0L

  for (rep in seq_len(N_REPS)) {
    row_idx <- row_idx + 1L
    # Unique seed per condition × replication (avoids collisions)
    rep_seed <- MASTER_SEED + ci * 10000L + rep
    results_list[[row_idx]] <- run_one_replication(cond, rep, rep_seed)
    if (results_list[[row_idx]]$converged == 0L) n_fail <- n_fail + 1L
  }

  # Progress report
  elapsed_cond  <- (proc.time() - t_cond_start)[["elapsed"]]
  elapsed_total <- (proc.time() - t_all_start)[["elapsed"]]
  pct_done      <- 100 * ci / n_conditions
  est_remaining <- elapsed_total / ci * (n_conditions - ci)
  cat(sprintf("[%3d/%d | %5.1f%%]  %-52s  fail:%2d/%d  %.0fs | ETA %.0f min\n",
              ci, n_conditions, pct_done,
              substr(cond$condition_id, 1, 52),
              n_fail, N_REPS, elapsed_cond,
              est_remaining / 60))

  # ── Checkpoint save ─────────────────────────────────────────────────────────
  if (ci %% CHECKPOINT_EVERY == 0L || ci == n_conditions) {
    # Convert completed rows to a data frame and write to disk
    done_rows <- results_list[seq_len(row_idx)]
    col_nms   <- names(done_rows[[1]])
    chk_df    <- as.data.frame(
      lapply(setNames(col_nms, col_nms), function(nm)
        sapply(done_rows, function(r) {
          v <- r[[nm]]
          if (is.null(v) || length(v) == 0) NA else unname(v[[1]])
        })),
      stringsAsFactors = FALSE
    )
    write.csv(chk_df, checkpoint_path, row.names = FALSE)
    message(sprintf("  → Checkpoint saved (%d conditions done)", ci))
  }
}

elapsed_all <- (proc.time() - t_all_start)[["elapsed"]]
cat(sprintf("\nAll replications done in %.1f min.\n\n", elapsed_all / 60))


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 5 ── Save final results (raw + clean)
# ─────────────────────────────────────────────────────────────────────────────

# Convert full results list to data frame
col_nms    <- names(results_list[[1]])
results_df <- as.data.frame(
  lapply(setNames(col_nms, col_nms), function(nm)
    sapply(results_list, function(r) {
      v <- r[[nm]]
      if (is.null(v) || length(v) == 0) NA else unname(v[[1]])
    })),
  stringsAsFactors = FALSE
)

# Coerce numeric columns
num_cols <- c("N", "delta", "sigma2", "D2_adjacent",
              "pi1", "pi2", "pi3", "rep", "converged",
              "match_cost_max", "label_switch_suspect",
              "accuracy", "mean_entropy",
              "oab", "fab", "hadi_a_cont",
              "max_hadi_p_pp", "hadi_m_mean",
              "sii_mean", "avepp_1", "avepp_2", "avepp_3", "avepp_min",
              "hadi_score")
for (col in num_cols)
  if (col %in% names(results_df))
    results_df[[col]] <- as.numeric(results_df[[col]])

# ── File 1: Full raw results (every replication, including failures) ──────────
raw_path <- file.path(results_dir, paste0("full_results_raw_", RUN_MODE, ".csv"))
write.csv(results_df, raw_path, row.names = FALSE)
cat(sprintf("Full raw results saved:  %s\n", raw_path))
cat(sprintf("  Rows: %d  |  Cols: %d\n", nrow(results_df), ncol(results_df)))

# ── File 2: Clean results (converged reps only; suspects are KEPT but flagged) ──
# "Clean" = mclust converged with K classes and no degenerate weights.
# label_switch_suspect = 1 rows are retained — use them for suspect-rate reporting
# and sensitivity checks. Filter them out in your own analysis if needed.
#
# WHY keep suspects here?
#   Removing them caused severe selection bias: hard conditions (small delta,
#   severe imbalance, low reliability) had near-100% suspect rates, leaving
#   those cells empty and reversing the HADI-P sanity check.
#   Keeping them preserves all 162 conditions in the summary table.
clean_df  <- results_df[
  !is.na(results_df$converged) &
  results_df$converged == 1L, ]

clean_path <- file.path(results_dir, paste0("full_results_clean_", RUN_MODE, ".csv"))
write.csv(clean_df, clean_path, row.names = FALSE)
cat(sprintf("Clean results saved:     %s\n", clean_path))
cat(sprintf("  Rows: %d  (converged; removed %d non-converged reps)\n",
            nrow(clean_df), nrow(results_df) - nrow(clean_df)))
n_suspect_clean <- sum(clean_df$label_switch_suspect == 1L, na.rm = TRUE)
cat(sprintf("  Of these, %d reps (%.1f%%) are label_switch_suspect = 1 (flagged, not removed)\n\n",
            n_suspect_clean, 100 * n_suspect_clean / nrow(clean_df)))


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 6 ── Summary analysis
# ─────────────────────────────────────────────────────────────────────────────

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  SIMULATION SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# ── 6a. Convergence and suspect rates ─────────────────────────────────────────
cat("── Convergence and label-switch suspect rates ──────────────────────\n")
rates <- aggregate(
  cbind(converged, label_switch_suspect) ~ N + delta + balance + noise,
  data = results_df,
  FUN  = function(x) round(mean(x, na.rm = TRUE), 3)
)
names(rates)[names(rates) == "converged"]            <- "conv_rate"
names(rates)[names(rates) == "label_switch_suspect"] <- "suspect_rate"
print(rates[order(rates$N, rates$delta, rates$balance, rates$noise), ],
      row.names = FALSE)
cat("\n")

# Save condition-level rates
rates_path <- file.path(results_dir, paste0("full_condition_log_", RUN_MODE, ".csv"))
write.csv(rates, rates_path, row.names = FALSE)
cat(sprintf("Condition log saved: %s\n\n", rates_path))

# ── 6b. HADI-C concern proportions by condition (clean results) ───────────────
cat("── HADI-C concern proportions (all converged reps) ─────────────────\n")
cat("   Columns show proportion of reps in each HADI-C category.\n")
cat("   suspect_rate = proportion of converged reps flagged as label-switch suspect.\n")
cat("   Use non_suspect_n to assess how many clean reps back each cell.\n\n")

concern_tab <- aggregate(hadi_c ~ N + delta + balance + noise,
  data = clean_df,
  FUN  = function(x) c(
    Low      = round(mean(x == "Low",      na.rm = TRUE), 3),
    Moderate = round(mean(x == "Moderate", na.rm = TRUE), 3),
    High     = round(mean(x == "High",     na.rm = TRUE), 3)
  )
)
concern_flat <- cbind(
  concern_tab[, c("N", "delta", "balance", "noise")],
  as.data.frame(concern_tab$hadi_c)
)
names(concern_flat)[5:7] <- c("prop_Low", "prop_Moderate", "prop_High")
concern_flat[, 5:7] <- lapply(concern_flat[, 5:7], round, 3)

# Add suspect rate and non-suspect N per condition
susp_tab <- aggregate(label_switch_suspect ~ N + delta + balance + noise,
  data = clean_df,
  FUN  = function(x) round(mean(x == 1L, na.rm = TRUE), 3)
)
names(susp_tab)[5] <- "suspect_rate"
nonsusp_n <- aggregate(label_switch_suspect ~ N + delta + balance + noise,
  data = clean_df,
  FUN  = function(x) sum(x == 0L, na.rm = TRUE)
)
names(nonsusp_n)[5] <- "non_suspect_n"
concern_flat <- merge(concern_flat, susp_tab,   by = c("N","delta","balance","noise"))
concern_flat <- merge(concern_flat, nonsusp_n,  by = c("N","delta","balance","noise"))

cat("  (showing first 20 rows; full table saved to summary CSV)\n")
print(head(concern_flat[order(concern_flat$N, concern_flat$delta,
                              concern_flat$balance, concern_flat$noise), ], 20),
      row.names = FALSE)
cat("\n")

# ── 6c. Moderate-band activation check ────────────────────────────────────────
cat("── Moderate-band activation check ─────────────────────────────────\n")
cat("   (How often does the Moderate band appear at component level?)\n")
for (comp in c("concern_p","concern_m","concern_s","concern_avepp","hadi_c")) {
  if (comp %in% names(clean_df)) {
    ct <- table(clean_df[[comp]])
    n  <- sum(ct)
    pct_mod  <- round(100 * (ct["Moderate"] / n), 1)
    pct_high <- round(100 * (ct["High"]     / n), 1)
    cat(sprintf("  %-16s  Low=%d  Moderate=%d(%s%%)  High=%d(%s%%)\n",
                comp,
                ct["Low"]      %||% 0L,
                ct["Moderate"] %||% 0L, ifelse(is.na(pct_mod),  "0.0", pct_mod),
                ct["High"]     %||% 0L, ifelse(is.na(pct_high), "0.0", pct_high)))
  }
}
cat("\n")

# ── 6d. Means of HADI metrics by separation and noise (clean) ─────────────────
cat("── Mean HADI metrics by separation (delta) and noise level ─────────\n")
metric_means <- aggregate(
  cbind(accuracy, oab, max_hadi_p_pp, hadi_m_mean, sii_mean,
        avepp_min, hadi_score, mean_entropy) ~ delta + noise,
  data = clean_df,
  FUN  = function(x) round(mean(x, na.rm = TRUE), 4)
)
print(metric_means[order(metric_means$delta, metric_means$noise), ],
      row.names = FALSE)
cat("\n")

# ── 6e. Save full summary ──────────────────────────────────────────────────────
summary_df <- merge(rates, concern_flat, by = c("N", "delta", "balance", "noise"))
summary_path <- file.path(results_dir,
                          paste0("full_summary_", RUN_MODE, ".csv"))
write.csv(summary_df, summary_path, row.names = FALSE)
cat(sprintf("Full summary saved: %s\n\n", summary_path))

# ── 6f. Sanity checks ────────────────────────────────────────────────────────
# All three checks use clean_df (all converged reps, suspects included).
# This avoids the selection-bias reversal that occurred when suspects were removed:
# severe-imbalance conditions had near-100% suspect rates, leaving only easy reps
# in the filtered set, which paradoxically showed LOWER HADI-P than balanced conditions.
cat("── Sanity checks (all converged reps) ───────────────────────────────\n")

acc_sep <- aggregate(accuracy ~ delta, data = clean_df,
                     FUN = function(x) round(mean(x, na.rm=TRUE), 4))
cat("  Accuracy by delta (expected: rises with delta):\n")
print(acc_sep[order(acc_sep$delta), ], row.names = FALSE)

oab_noi <- aggregate(oab ~ noise, data = clean_df,
                     FUN = function(x) round(mean(x, na.rm=TRUE), 4))
cat("  OAB by noise level (expected: low_rel > med_rel > high_rel):\n")
print(oab_noi, row.names = FALSE)

p_bal <- aggregate(max_hadi_p_pp ~ balance, data = clean_df,
                   FUN = function(x) round(mean(x, na.rm=TRUE), 4))
cat("  max|HADI-P| by balance (expected: sev_imbal > mod_imbal > balanced):\n")
print(p_bal, row.names = FALSE)

# Also show suspect-only vs non-suspect breakdown for HADI-P (diagnostic)
if (any(clean_df$label_switch_suspect == 1L, na.rm = TRUE)) {
  cat("\n  max|HADI-P| split by suspect flag (diagnostic):\n")
  p_susp <- aggregate(max_hadi_p_pp ~ balance + label_switch_suspect,
                      data = clean_df,
                      FUN = function(x) round(mean(x, na.rm=TRUE), 4))
  p_susp$label_switch_suspect <- ifelse(p_susp$label_switch_suspect == 1L,
                                        "suspect", "clean")
  names(p_susp)[3] <- "mean_HADI_P_pp"
  print(p_susp[order(p_susp$balance, p_susp$label_switch_suspect), ],
        row.names = FALSE)
}
cat("\n")

cat("═══════════════════════════════════════════════════════════════════\n")
cat(sprintf("  SIMULATION COMPLETE  |  mode: %s\n", toupper(RUN_MODE)))
cat(sprintf("  Raw results:   %s\n", raw_path))
cat(sprintf("  Clean results: %s\n", clean_path))
cat(sprintf("  Summary:       %s\n", summary_path))
cat("═══════════════════════════════════════════════════════════════════\n\n")


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 7 ── Helper for safe NULL coalescing used in Section 6
# ─────────────────────────────────────────────────────────────────────────────
# R doesn't have a built-in %||% operator; define it here.
# (Placed at the end so it doesn't clutter the top of the script.)
`%||%` <- function(a, b) if (!is.null(a) && !is.na(a)) a else b


# =============================================================================
# NOTES FOR THE MANUSCRIPT
# =============================================================================
#
# OUTPUT FILES — what each file contains
# ─────────────────────────────────────────────────────────────────────────────
# full_results_raw_[mode].csv
#   Every replication, including convergence failures and label-switch suspects.
#   Use this for reporting failure rates and for sensitivity checks.
#   Key columns: converged, fail_reason, label_switch_suspect, match_cost_max
#
# full_results_clean_[mode].csv
#   All converged replications. label_switch_suspect suspects are KEPT (flagged,
#   not removed) to prevent selection bias in hard conditions.
#   Use this as the primary analysis file.
#   To replicate the "non-suspect only" subset: filter where label_switch_suspect == 0.
#
# full_condition_log_[mode].csv
#   One row per condition: convergence rate and suspect rate.
#   Useful for identifying problematic design cells (low sep + low rel).
#
# full_summary_[mode].csv
#   Convergence rates + HADI-C proportions merged into one table.
#   Ready to paste into manuscript supplementary tables.
#
# checkpoint_[mode].csv
#   Intermediate save updated every CHECKPOINT_EVERY conditions.
#   If the script crashes, this contains all results up to the last checkpoint.
#   Rename it to full_results_raw_[mode].csv to continue analysis.
#
# WHAT TO CHECK FIRST
# ─────────────────────────────────────────────────────────────────────────────
# 1. full_condition_log: convergence rate < 0.90 in any condition?
#    Expected: low failures for delta >= 1.0; more for delta = 0.5 + low_rel.
# 2. full_summary: does HADI-C = Moderate appear for intermediate delta values
#    (delta = 0.8, 1.0, 1.2)? If yes, the threshold calibration is working.
# 3. Sanity checks (Section 6f): all three directional patterns should hold.
#
# DECIDING WHEN THE FULL SIMULATION IS READY
# ─────────────────────────────────────────────────────────────────────────────
# Run "test" first (5 reps). If it completes without errors, run "medium"
# (200 reps). After reviewing "medium" results:
#   • If HADI-C distributions are stable (proportions change < 5% between
#     adjacent delta levels), "medium" results are sufficient for manuscript.
#   • If results look noisy (prop_High jumps unexpectedly between conditions),
#     run "full" (500 reps) for more stable estimates.
#
# LABEL_SWITCH_SUSPECT CASES
# ─────────────────────────────────────────────────────────────────────────────
# A replication is flagged as label_switch_suspect = 1 when the worst
# matched class pair (true vs. estimated mean) is more than half a
# between-class gap apart (threshold = 0.5 * delta * sqrt(J)).
#
# IMPORTANT: Suspects are KEPT in full_results_clean to avoid selection bias.
# When suspects were removed, conditions with near-100% suspect rates (small
# delta + severe imbalance + low reliability) became completely empty in the
# summary table, and the HADI-P sanity check reversed direction — an artifact
# of only the easiest reps surviving the filter.
#
# For sensitivity analysis you can still filter: clean_df[clean_df$label_switch_suspect==0,]
#
# In the manuscript, report suspect rates descriptively:
#   "X% of converged replications were flagged as possible label-switching
#    failures (match_cost_max > 0.5 * delta * sqrt(J)). Flagging was most
#    common in conditions with low separation (delta ≤ 0.8) combined with
#    severe class imbalance (pi_min = 0.05) and low indicator reliability
#    (sigma^2 = 2), where effective separation D^2 ≤ 1.6. These conditions
#    represent genuinely ambiguous situations; the high suspect rate is itself
#    informative about non-identifiability. All converged replications were
#    retained for analysis, with label_switch_suspect reported as a covariate."
#
# FAILED / DEGENERATE SOLUTIONS
# ─────────────────────────────────────────────────────────────────────────────
# A replication is marked converged = 0 when mclust either (a) returns NULL,
# (b) finds fewer than K=3 classes, or (c) produces a solution with any
# estimated mixing weight below 0.01 (degenerate class).
# In the manuscript, report:
#   "Across all conditions, X% of replications failed to converge to a
#    valid K=3 solution. Failure rates were highest in conditions combining
#    low separation (delta = 0.5) with low indicator reliability (sigma^2 = 2),
#    where effective Mahalanobis distance D^2 = 1.12. These replications were
#    excluded from all HADI analyses. The convergence failure rate by condition
#    is reported in Supplementary Table S1."
# =============================================================================
