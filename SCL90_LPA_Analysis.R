# =============================================================================
# Hard Assignment vs. Probability-Weighted Summarization in LPA
# A Methodological Study Using the SCL-90
#
# Purpose : Full analysis pipeline for journal submission
# Author  : [Your Name]
# Date    : April 2026
# Target  : Psychological Methods / Multivariate Behavioral Research
# =============================================================================
#
# PAPER OUTPUTS PRODUCED BY THIS SCRIPT
#   Empirical
#     Table 1  – Sample descriptives & subscale means
#     Table 2  – LPA model-fit comparison (2-5 classes)
#     Table 3  – Posterior uncertainty metrics for focal solution
#     Table 4  – Hard vs. weighted class prevalence & severe-class size
#     Table 5  – Hard vs. weighted class-specific profile means
#     Table 6  – Ambiguity burden by class
#     Table 7  – Recommended reporting framework / diagnostic guide
#   Simulation
#     Table 8  – Simulation results: prevalence bias & distortion by condition
#   Figures
#     Fig 1 – Conceptual diagram (described; rendered externally)
#     Fig 2 – Profile plots: hard-assigned vs. probability-weighted
#     Fig 3 – Distribution of maximum posterior probabilities
#     Fig 4 – Ambiguity burden by class (barplot)
#     Fig 5 – Simulation heatmap: distortion under different conditions
# =============================================================================


# ── 0. SETUP ─────────────────────────────────────────────────────────────────

pkgs <- c("readxl", "mclust", "tidyLPA", "ggplot2", "patchwork",
          "viridis", "kableExtra", "gt", "MASS", "mvtnorm",
          "dplyr", "tidyr", "tibble", "purrr", "scales")

invisible(lapply(pkgs, function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  suppressPackageStartupMessages(library(p, character.only = TRUE))
}))

# Ensure dplyr::select is used (MASS also exports select)
select  <- dplyr::select
filter  <- dplyr::filter
mutate  <- dplyr::mutate
rename  <- dplyr::rename
arrange <- dplyr::arrange
summarise <- dplyr::summarise

set.seed(20260412)          # reproducibility
options(scipen = 999, digits = 4)

# Output directory
OUT <- "/Users/chenxiaohui/Desktop/writing articles/Academic article /Bayesian in Psychometrics /output/"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

cat("=== Setup complete ===\n")


# ── 1. DATA LOADING & PREPARATION ────────────────────────────────────────────

# ── Load raw data
raw <- read_excel(
  "/Users/chenxiaohui/Desktop/writing articles/Academic article /Bayesian in Psychometrics /SCL90 dataset.xls"
)

# QC Step 1: Record raw N
qc_raw_N <- nrow(raw)
cat("QC Step 1 — Raw dataset N:", qc_raw_N, "\n")

# QC Step 2: Force items to numeric
item_cols   <- paste0("Q", 1:90)
raw[item_cols] <- lapply(raw[item_cols], as.numeric)

# QC Step 3: Structural checks
# Remove cases with impossible age values (keep 15–35 for college sample)
raw_structural <- raw
if ("AGE" %in% names(raw_structural)) {
  age_vals <- suppressWarnings(as.numeric(raw_structural$AGE))
  invalid_age <- !is.na(age_vals) & (age_vals < 15 | age_vals > 35)
  raw_structural <- raw_structural[!invalid_age | is.na(age_vals), ]
}
qc_after_structural <- nrow(raw_structural)
cat("QC Step 3 — After structural checks N:", qc_after_structural,
    "| Excluded:", qc_raw_N - qc_after_structural, "\n")

# QC Step 4: Compute subscale scores
subscale_items <- list(
  SOM  = c(1,4,12,27,40,42,48,49,52,53,56,58),
  OC   = c(3,9,10,28,38,45,46,51,55,65),
  IS   = c(6,21,34,36,37,41,61,69),
  DEP  = c(5,14,15,20,22,26,29,32,54,71,79),
  ANX  = c(2,17,23,33,39,57,72,78,80,86),
  HOS  = c(11,24,63,67,74,81),
  PHOB = c(13,25,47,50,70,75,82),
  PAR  = c(8,18,43,68,76,83),
  PSY  = c(7,16,35,62,77,84,85,87,88,90)
)
subscale_labels <- c(
  SOM  = "Somatization",        OC   = "Obsessive-Compulsive",
  IS   = "Interpersonal Sensitivity", DEP = "Depression",
  ANX  = "Anxiety",             HOS  = "Hostility",
  PHOB = "Phobic Anxiety",      PAR  = "Paranoid Ideation",
  PSY  = "Psychoticism"
)
subscale_names <- names(subscale_items)

for (sub in subscale_names) {
  cols <- paste0("Q", subscale_items[[sub]])
  raw_structural[[sub]] <- rowMeans(raw_structural[cols], na.rm = TRUE)
}

# QC Step 5: Complete-case exclusion on subscales
has_missing <- !complete.cases(raw_structural[subscale_names])
qc_missing_N <- sum(has_missing)

dat <- raw_structural[!has_missing,
                      c("StudentsID", "GENDER", "AGE", subscale_names)]
qc_final_N <- nrow(dat)

cat("QC Step 5 — Missing on subscales:", qc_missing_N,
    "| Final analytic N:", qc_final_N, "\n")

# QC Summary table (used for Figure 2 and Methods text)
qc_summary <- data.frame(
  Step       = c("Raw dataset",
                 "After structural checks",
                 "Excluded (structural)",
                 "Excluded (missing subscales)",
                 "Final analytic sample"),
  N          = c(qc_raw_N,
                 qc_after_structural,
                 qc_raw_N - qc_after_structural,
                 qc_missing_N,
                 qc_final_N)
)
cat("\n=== QC FLOW SUMMARY ===\n")
print(qc_summary)
write.csv(qc_summary, file.path(OUT, "QC_Flow_Summary.csv"), row.names = FALSE)



# ── 2. TABLE 1: DESCRIPTIVE STATISTICS ───────────────────────────────────────

desc_table <- dat %>%
  select(all_of(subscale_names)) %>%
  summarise(across(everything(), list(
    M    = ~ mean(., na.rm = TRUE),
    SD   = ~ sd(., na.rm = TRUE),
    Min  = ~ min(., na.rm = TRUE),
    Max  = ~ max(., na.rm = TRUE),
    Skew = ~ mean((. - mean(.))^3, na.rm = TRUE) / sd(., na.rm = TRUE)^3
  ))) %>%
  pivot_longer(everything(),
               names_to  = c("Subscale", ".value"),
               names_sep = "_") %>%
  mutate(Label = subscale_labels[Subscale],
         across(where(is.numeric), ~ round(., 3))) %>%
  select(Subscale, Label, M, SD, Min, Max, Skew)

print(desc_table)

cor_matrix <- round(cor(dat[subscale_names], use = "complete.obs"), 3)
print(cor_matrix)

write.csv(desc_table,  file.path(OUT, "Table1_Descriptives.csv"),   row.names = FALSE)
write.csv(cor_matrix,  file.path(OUT, "Table1b_Correlations.csv"),  row.names = TRUE)
cat("Table 1 saved.\n")



# ── 3. LPA MODEL FITTING ─────────────────────────────────────────────────────
#
# Model: "EEI" (equal variances, zero covariances) via mclust
# – This is the standard model for psychometric profile analysis
#   (cf. Masyn 2013; Pastor et al. 2007; Nylund-Gibson & Choi 2018)
# – Corresponds to tidyLPA model = 1
# – Sensitivity check with model = 6 (VVV) is run separately
#
# For N = 59K, mclust handles this efficiently.
# We fit 2–5 classes as preregistered.
# ─────────────────────────────────────────────────────────────────────────────



lpa_data   <- dat[subscale_names]

cat("Fitting LPA K = 2 to 5 (may take 2-5 minutes)...\n")
lpa_models <- tidyLPA::estimate_profiles(lpa_data, n_profiles = 2:5, models = 1)

fit_stats  <- get_fit(lpa_models)
print(as.data.frame(fit_stats))
write.csv(as.data.frame(fit_stats), file.path(OUT, "Table2_ModelFit.csv"), row.names = FALSE)
cat("Table 2 saved.\n")



# ── 4. FOCAL SOLUTION SELECTION ──────────────────────────────────────────────
#
# Selection criteria (preregistered order):
#  1. BIC  – lower is better
#  2. aBIC – lower is better
#  3. Entropy ≥ .80 preferred
#  4. Smallest class ≥ 1% of N
#  5. Theoretical interpretability
# ─────────────────────────────────────────────────────────────────────────────

# Print all solutions' class sizes for inspection
K_FOCAL     <- 4    # change after reviewing Table 2 if needed

focal_key   <- paste0("model_1_class_", K_FOCAL)
focal_model <- lpa_models[[focal_key]]
focal_data  <- get_data(focal_model)

post_raw_cols <- grep("^CPROB", names(focal_data), value = TRUE)
dat_with_post <- cbind(dat, focal_data[, c("Class", post_raw_cols)])

post_cols <- paste0("post_", 1:K_FOCAL)
for (i in seq_along(post_raw_cols)) {
  names(dat_with_post)[names(dat_with_post) == post_raw_cols[i]] <- post_cols[i]
}

# Clean hard class vector (root fix applied from the start)
hard_class_vec <- as.integer(focal_data$Class)
dat_with_post$hard_class <- hard_class_vec

cat("Class sizes:\n")
print(round(prop.table(table(hard_class_vec)) * 100, 1))


# ── 5. AMBIGUITY METRICS ─────────────────────────────────────────────────────
#
# For each person, compute the six preregistered uncertainty indices.
# These form the empirical core of the methodological contribution.
# ─────────────────────────────────────────────────────────────────────────────

post_mat <- as.matrix(dat_with_post[, post_cols])

dat_with_post$max_post     <- apply(post_mat, 1, max)
dat_with_post$sec_post     <- apply(post_mat, 1, function(x) sort(x, decreasing=TRUE)[2])
dat_with_post$top2_margin  <- dat_with_post$max_post - dat_with_post$sec_post
dat_with_post$entropy_i    <- -rowSums(post_mat * log(pmax(post_mat, 1e-10)))
dat_with_post$ambig_70     <- (dat_with_post$max_post < .70)
dat_with_post$ambig_80     <- (dat_with_post$max_post < .80)
dat_with_post$ambig_margin <- (dat_with_post$top2_margin < .15)

# Identify severe class
class_overall_means <- sapply(sort(unique(hard_class_vec)), function(k) {
  mean(unlist(dat_with_post[hard_class_vec == k, subscale_names]))
})
SEVERE_CLASS <- which.max(class_overall_means)
dat_with_post$in_severe <- (hard_class_vec == SEVERE_CLASS)

cat("Severe class:", SEVERE_CLASS, "\n")
cat("Overall ambiguity burden (max_post < .70):",
    round(mean(dat_with_post$ambig_70) * 100, 2), "%\n")



# ── 6. TABLE 3: POSTERIOR UNCERTAINTY METRICS ────────────────────────────────
# ── TABLE 3: Uncertainty metrics by class
table3 <- data.frame(
  Class        = sort(unique(hard_class_vec)),
  N            = as.integer(table(hard_class_vec)),
  Pct_sample   = as.numeric(table(hard_class_vec)) / length(hard_class_vec) * 100,
  AvePP        = tapply(dat_with_post$max_post,    hard_class_vec, mean) * 100,
  Pct_lt70     = tapply(dat_with_post$ambig_70,    hard_class_vec, mean) * 100,
  Pct_lt80     = tapply(dat_with_post$ambig_80,    hard_class_vec, mean) * 100,
  Pct_margin15 = tapply(dat_with_post$ambig_margin,hard_class_vec, mean) * 100,
  Mean_entropy = tapply(dat_with_post$entropy_i,   hard_class_vec, mean)
)
table3$Is_severe <- (table3$Class == SEVERE_CLASS)
table3[, sapply(table3, is.numeric)] <-
  round(table3[, sapply(table3, is.numeric)], 2)

cat("\n=== TABLE 3: Uncertainty Metrics ===\n")
print(table3)
write.csv(table3, file.path(OUT, "Table3_UncertaintyMetrics.csv"), row.names = FALSE)

# ── TABLE 4: Hard vs. weighted prevalence
hard_tab  <- table(hard_class_vec)
prev_hard <- data.frame(
  Class     = as.integer(names(hard_tab)),
  n_hard    = as.integer(hard_tab),
  prev_hard = as.numeric(hard_tab) / sum(hard_tab) * 100
)

prev_weighted <- dat_with_post %>%
  summarise(across(all_of(post_cols), mean)) %>%
  pivot_longer(everything(),
               names_to     = "post_col",
               names_prefix = "post_",
               values_to    = "prev_weighted") %>%
  mutate(Class         = as.integer(post_col),
         prev_weighted = prev_weighted * 100) %>%
  select(Class, prev_weighted)

table4 <- prev_hard %>%
  left_join(prev_weighted, by = "Class") %>%
  mutate(
    PII       = round(prev_hard - prev_weighted, 3),  # Prevalence Inflation Index
    rel_diff  = round(PII / prev_weighted * 100, 2),
    Is_severe = (Class == SEVERE_CLASS)
  )

cat("\n=== TABLE 4: Hard vs. Weighted Prevalence ===\n")
print(table4)
write.csv(table4, file.path(OUT, "Table4_Prevalence_Comparison.csv"), row.names = FALSE)

# ── TABLE 5: Hard vs. weighted profile means
profile_hard <- dat_with_post %>%
  group_by(Class = hard_class) %>%
  summarise(across(all_of(subscale_names), mean, .names = "{.col}_hard"),
            .groups = "drop")

profile_weighted_list <- lapply(1:K_FOCAL, function(k) {
  wts   <- dat_with_post[[post_cols[k]]]
  means <- sapply(subscale_names, function(s)
    weighted.mean(dat_with_post[[s]], w = wts, na.rm = TRUE))
  df    <- as.data.frame(t(means))
  names(df) <- paste0(subscale_names, "_wtd")
  df$Class  <- k
  df
})
profile_weighted <- do.call(rbind, profile_weighted_list)

table5 <- profile_hard %>%
  left_join(profile_weighted, by = "Class") %>%
  arrange(Class)

table5_long <- table5 %>%
  pivot_longer(-Class,
               names_to  = c("Subscale", "Method"),
               names_sep = "_",
               values_to = "Mean") %>%
  pivot_wider(names_from = Method, values_from = Mean) %>%
  mutate(
    Diff  = round(hard - wtd, 4),
    Label = subscale_labels[Subscale]
  ) %>%
  rename(Hard = hard, Weighted = wtd) %>%
  arrange(Class, Subscale) %>%
  mutate(across(c(Hard, Weighted), ~ round(., 4)))

cat("\n=== TABLE 5: Hard vs. Weighted Profile Means ===\n")
print(table5_long, n = Inf)
write.csv(table5_long, file.path(OUT, "Table5_ProfileMeans.csv"), row.names = FALSE)

# ── TABLE 6: Ambiguity burden by class
table6 <- table3  # already computed above — extend with severe-class certainty
sev_certainty <- mean(dat_with_post$max_post[dat_with_post$in_severe]) * 100
cat(sprintf("\nSevere-class certainty index (mean max_post in Class %d): %.2f%%\n",
            SEVERE_CLASS, sev_certainty))
write.csv(table6, file.path(OUT, "Table6_AmbiguityByClass.csv"), row.names = FALSE)

# ── NEW Metric 12: Profile Discrepancy Index (PDI)
# Average absolute difference between hard and weighted class means per class
pdi_by_class <- table5_long %>%
  group_by(Class) %>%
  summarise(
    PDI = mean(abs(Diff), na.rm = TRUE),
    Max_diff = max(abs(Diff), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

cat("\n=== Profile Discrepancy Index (PDI) by Class ===\n")
print(pdi_by_class)
write.csv(pdi_by_class, file.path(OUT, "Table5c_ProfileDiscrepancyIndex.csv"),
          row.names = FALSE)

# ── Separation Inflation Index
sep_inflation <- sapply(subscale_names, function(s) {
  x          <- dat_with_post[[s]]
  hard_means <- tapply(x, hard_class_vec, mean, na.rm = TRUE)
  hard_ns    <- tapply(x, hard_class_vec, length)
  gm_hard    <- weighted.mean(hard_means, hard_ns)
  bcv_hard   <- weighted.mean((hard_means - gm_hard)^2, hard_ns)
  
  wtd_means <- sapply(1:K_FOCAL, function(k)
    weighted.mean(x, w = dat_with_post[[post_cols[k]]], na.rm = TRUE))
  wtd_ns    <- sapply(1:K_FOCAL, function(k) sum(dat_with_post[[post_cols[k]]]))
  gm_wtd    <- mean(x, na.rm = TRUE)
  bcv_wtd   <- weighted.mean((wtd_means - gm_wtd)^2, wtd_ns)
  
  c(BCV_hard      = round(bcv_hard, 4),
    BCV_wtd       = round(bcv_wtd, 4),
    SII           = round(bcv_hard - bcv_wtd, 4),
    Pct_inflation = round((bcv_hard / bcv_wtd - 1) * 100, 2))
}) %>%
  t() %>% as.data.frame() %>%
  rownames_to_column("Subscale") %>%
  mutate(Label = subscale_labels[Subscale])

cat("\n=== Separation Inflation Index ===\n")
print(sep_inflation)
write.csv(sep_inflation, file.path(OUT, "Table5b_SeparationInflation.csv"),
          row.names = FALSE)


#BLOCK 7 — Figures 3, 4, 5 (renumbered per new design)#
theme_paper <- theme_classic(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 13),
        strip.background = element_blank(),
        legend.position  = "bottom",
        axis.text        = element_text(color = "black"),
        panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3))

class_role <- setNames(
  c("Moderate", "Severe", "Asymptomatic", "Mild")[rank(-class_overall_means)],
  as.character(1:K_FOCAL)
)

# ── Figure 3: Profile plots (hard vs. weighted)
fig3_data <- table5_long %>%
  mutate(
    Class_label    = paste0("Class ", Class, " (", class_role[as.character(Class)], ")"),
    Subscale_label = factor(Label, levels = unname(subscale_labels))
  ) %>%
  pivot_longer(c(Hard, Weighted), names_to = "Method", values_to = "Mean")

fig3 <- ggplot(fig3_data,
               aes(x = Subscale_label, y = Mean,
                   group = Method, color = Method, linetype = Method)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  facet_wrap(~ Class_label, ncol = 2) +
  scale_color_manual(
    values = c("Hard" = "#D62728", "Weighted" = "#1F77B4"),
    labels = c("Hard (modal) assignment", "Probability-weighted")
  ) +
  scale_linetype_manual(
    values = c("Hard" = "solid", "Weighted" = "dashed"),
    labels = c("Hard (modal) assignment", "Probability-weighted")
  ) +
  labs(
    title    = "Figure 3. SCL-90 Symptom Profiles: Hard Assignment vs. Probability-Weighted",
    subtitle = "Subscale means (1–5) by class and summarisation method",
    x = "SCL-90 Subscale", y = "Mean Score (1–5)",
    color = "Method", linetype = "Method"
  ) +
  theme_paper +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 9))

ggsave(file.path(OUT, "Figure3_ProfilePlots.pdf"), fig3, width = 11, height = 8, dpi = 300)
ggsave(file.path(OUT, "Figure3_ProfilePlots.png"), fig3, width = 11, height = 8, dpi = 300)
cat("Figure 3 saved.\n")

# ── Figure 4: Max posterior distribution
fig4 <- ggplot(dat_with_post, aes(x = max_post)) +
  geom_histogram(bins = 50, fill = "#4393C3", color = "white", alpha = 0.85) +
  geom_vline(xintercept = c(.70, .80),
             color = c("#D62728", "#FF7F0E"),
             linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = .695, y = Inf, label = ".70",
           vjust = 2, hjust = 1, color = "#D62728", size = 3.5) +
  annotate("text", x = .795, y = Inf, label = ".80",
           vjust = 2, hjust = 1, color = "#FF7F0E", size = 3.5) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, 1, .1)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Figure 4. Distribution of Maximum Posterior Class-Membership Probabilities",
    subtitle = paste0("N = ", scales::comma(nrow(dat_with_post)),
                      "  |  dashed lines = common certainty thresholds"),
    x = "Maximum Posterior Probability", y = "Frequency"
  ) +
  theme_paper

ggsave(file.path(OUT, "Figure4_MaxPost_Distribution.pdf"), fig4, width = 8, height = 5, dpi = 300)
ggsave(file.path(OUT, "Figure4_MaxPost_Distribution.png"), fig4, width = 8, height = 5, dpi = 300)
cat("Figure 4 saved.\n")

# ── Figure 5: Ambiguity burden by class
fig5_data <- table6 %>%
  select(Class, Pct_lt70, Pct_lt80, Pct_margin15, Is_severe) %>%
  pivot_longer(c(Pct_lt70, Pct_lt80, Pct_margin15),
               names_to = "Threshold", values_to = "Pct") %>%
  mutate(
    Class_label = ifelse(Is_severe,
                         paste0("Class ", Class, "\n(Severe)"),
                         paste0("Class ", Class)),
    Threshold_label = recode(Threshold,
                             Pct_lt70     = "max post < .70",
                             Pct_lt80     = "max post < .80",
                             Pct_margin15 = "top-2 margin < .15")
  )

fig5 <- ggplot(fig5_data,
               aes(x = Class_label, y = Pct, fill = Threshold_label)) +
  geom_col(position = position_dodge(0.8), width = 0.75) +
  scale_fill_manual(values = c("#D62728", "#FF7F0E", "#BCBD22")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 15)) +
  labs(
    title    = "Figure 5. Ambiguity Burden by Class",
    subtitle = "Percentage of class members with weak posterior dominance",
    x = "Class", y = "% Members Classified as Ambiguous",
    fill = "Ambiguity Criterion"
  ) +
  theme_paper

ggsave(file.path(OUT, "Figure5_AmbiguityByClass.pdf"), fig5, width = 8, height = 5, dpi = 300)
ggsave(file.path(OUT, "Figure5_AmbiguityByClass.png"), fig5, width = 8, height = 5, dpi = 300)
cat("Figure 5 saved.\n")


# ── STUDY 2: Simulation — 3×3×3 design, N=1000 fixed, 200 reps
# ── Parallel execution — expected time: 3-5 minutes

library(parallel)

worker_fn <- function(cond, n_rep = 200, n_ind = 9, K = 3) {
  library(mclust); library(MASS)
  
  N        <- cond$N
  d_sep    <- c(Low = 0.5, Medium = 1.5, High = 3.0)[cond$separation]
  props    <- list(
    Balanced = rep(1/K, K),
    Moderate = c(.60, .25, .15),
    Severe   = c(.80, .15, .05)
  )[[cond$balance]]
  r        <- c(High = .90, Moderate = .70, Low = .50)[cond$reliability]
  noise_sd <- sqrt((1 - r) / r)
  means_list <- lapply(1:K, function(k) rep((k-1) * d_sep, n_ind))
  
  # QC tracking
  n_converged <- 0
  n_failed    <- 0
  
  out <- matrix(NA_real_, nrow = n_rep, ncol = 6,
                dimnames = list(NULL,
                                c("prev_bias","over_label","amb_sev",
                                  "sep_infl","agreement","pdi")))
  
  for (ri in seq_len(n_rep)) {
    ns <- as.integer(round(N * props)); ns[K] <- N - sum(ns[-K])
    S  <- diag(noise_sd^2, n_ind)
    X  <- do.call(rbind, lapply(1:K, function(k)
      MASS::mvrnorm(ns[k], means_list[[k]], S)))[sample(N), ]
    
    fit <- tryCatch(
      mclust::Mclust(X, G = K, modelNames = "EEI", verbose = FALSE),
      error = function(e) NULL
    )
    if (is.null(fit)) { n_failed <- n_failed + 1; next }
    n_converged <- n_converged + 1
    
    posts <- fit$z
    hc    <- apply(posts, 1, which.max)
    mp    <- apply(posts, 1, max)
    
    # Identify severe class (highest mean on indicator 1)
    cm1 <- tapply(X[,1], hc, mean)
    es  <- as.integer(names(which.max(cm1)))
    hp  <- mean(hc == es)
    wp  <- mean(posts[, es])
    ihs <- (hc == es)
    
    # Between-class variance
    gm   <- mean(X[,1])
    bcvh <- sum(tapply(X[,1], hc,
                       function(x) length(x)/N * (mean(x) - gm)^2))
    bcvw <- sum(sapply(1:K, function(k)
      sum(posts[,k])/N *
        (weighted.mean(X[,1], posts[,k]) - gm)^2))
    
    # Profile Discrepancy Index (PDI) — NEW outcome
    pdi_val <- mean(sapply(1:K, function(k) {
      hard_m <- colMeans(X[hc == k, , drop = FALSE])
      wtd_m  <- apply(X, 2, function(col) weighted.mean(col, posts[,k]))
      mean(abs(hard_m - wtd_m))
    }))
    
    out[ri, ] <- c(
      prev_bias  = hp - props[K],
      over_label = hp - wp,
      amb_sev    = if (any(ihs)) mean(mp[ihs] < .70) else NA,
      sep_infl   = if (bcvw > 0) (bcvh/bcvw - 1) * 100 else 0,
      agreement  = mean(mp >= .70),
      pdi        = pdi_val
    )
  }
  
  data.frame(
    N = N, separation = cond$separation,
    balance = cond$balance, reliability = cond$reliability,
    n_planned   = n_rep,
    n_converged = n_converged,
    n_failed    = n_failed,
    prev_bias_M    = mean(out[,"prev_bias"],  na.rm=TRUE),
    prev_bias_SD   = sd(out[,"prev_bias"],    na.rm=TRUE),
    over_label_M   = mean(out[,"over_label"], na.rm=TRUE),
    over_label_SD  = sd(out[,"over_label"],   na.rm=TRUE),
    amb_severe_M   = mean(out[,"amb_sev"],    na.rm=TRUE),
    amb_severe_SD  = sd(out[,"amb_sev"],      na.rm=TRUE),
    sep_infl_M     = mean(out[,"sep_infl"],   na.rm=TRUE),
    sep_infl_SD    = sd(out[,"sep_infl"],     na.rm=TRUE),
    agreement_M    = mean(out[,"agreement"],  na.rm=TRUE),
    agreement_SD   = sd(out[,"agreement"],    na.rm=TRUE),
    pdi_M          = mean(out[,"pdi"],        na.rm=TRUE),
    pdi_SD         = sd(out[,"pdi"],          na.rm=TRUE)
  )
}

# ── 27-condition grid (N fixed at 1000)
sim_grid <- expand.grid(
  N           = 1000L,
  separation  = c("Low", "Medium", "High"),
  balance     = c("Balanced", "Moderate", "Severe"),
  reliability = c("High", "Moderate", "Low"),
  stringsAsFactors = FALSE
)

cond_list <- split(sim_grid, seq_len(nrow(sim_grid)))
cat("Conditions:", nrow(sim_grid), "| Reps:", 200,
    "| Total fits:", nrow(sim_grid) * 200, "\n")

N_CORES <- parallel::detectCores() - 1
cat("Running on", N_CORES, "cores — estimated time: 3-5 minutes\n")

cl <- parallel::makeCluster(N_CORES, type = "PSOCK")
parallel::clusterSetRNGStream(cl, 20260412)
parallel::clusterExport(cl, "worker_fn")

t0          <- proc.time()
sim_results <- parallel::parLapply(cl, cond_list, worker_fn, n_rep = 200)
parallel::stopCluster(cl)

cat(sprintf("Done in %.1f minutes.\n", (proc.time()-t0)["elapsed"]/60))

# ── Combine and save
sim_table <- do.call(rbind, sim_results)
sim_table$separation  <- factor(sim_table$separation,  levels=c("Low","Medium","High"))
sim_table$reliability <- factor(sim_table$reliability, levels=c("Low","Moderate","High"))
sim_table$balance     <- factor(sim_table$balance,     levels=c("Balanced","Moderate","Severe"))
sim_table[sapply(sim_table, is.numeric)] <-
  round(sim_table[sapply(sim_table, is.numeric)], 4)

# ── Simulation QC report
cat("\n=== SIMULATION QC ===\n")
cat("Total planned replications:", sum(sim_table$n_planned), "\n")
cat("Total converged:           ", sum(sim_table$n_converged), "\n")
cat("Total failed:              ", sum(sim_table$n_failed), "\n")
cat("Convergence rate:          ",
    round(sum(sim_table$n_converged)/sum(sim_table$n_planned)*100, 1), "%\n")

cat("\n=== TABLE 7: Simulation Results ===\n")
print(sim_table[, c("separation","balance","reliability",
                    "amb_severe_M","prev_bias_M","sep_infl_M",
                    "pdi_M","agreement_M")])

write.csv(sim_table, file.path(OUT, "Table7_SimulationResults.csv"), row.names = FALSE)
saveRDS(sim_table,   file.path(OUT, "simulation_results.rds"))
cat("Table 7 saved.\n")


# ── Figure 6: Simulation heatmap
fig6 <- ggplot(sim_table,
               aes(x = separation, y = reliability, fill = amb_severe_M)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.0f%%", amb_severe_M * 100)),
            size = 3.5, color = "white", fontface = "bold") +
  facet_wrap(~ balance, ncol = 3, labeller = label_both) +
  scale_fill_viridis_c(
    name   = "% Ambiguous\nSevere Cases",
    labels = scales::percent_format(accuracy = 1),
    option = "plasma", direction = -1
  ) +
  labs(
    title    = "Figure 6. Simulation: Ambiguous Severe-Class Labeling Under Hard Assignment",
    subtitle = "% of hard-assigned severe cases with max posterior < .70  |  N = 1,000",
    x = "Class Separation", y = "Indicator Reliability"
  ) +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        strip.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom")

ggsave(file.path(OUT, "Figure6_SimHeatmap.pdf"), fig6, width = 11, height = 5, dpi = 300)
ggsave(file.path(OUT, "Figure6_SimHeatmap.png"), fig6, width = 11, height = 5, dpi = 300)
cat("Figure 6 saved.\n")


# ── TABLE 8: Diagnostic framework with assignment adequacy zones
table8 <- tribble(
  ~Diagnostic,                    ~Low_concern,              ~Moderate_concern,           ~High_concern,
  "Overall ambiguity burden",     "< 10% with max_post < .70","10–30% with max_post < .70","> 30% with max_post < .70",
  "Severe-class ambiguity burden","< 10% of severe members", "10–25% of severe members",   "> 25% of severe members",
  "Prevalence inflation (PII)",   "< 2 percentage points",   "2–5 percentage points",       "> 5 percentage points",
  "Separation inflation (SII)",   "< 5% BCV increase",       "5–15% BCV increase",          "> 15% BCV increase",
  "Profile discrepancy (PDI)",    "< 0.02 scale units",      "0.02–0.05 scale units",        "> 0.05 scale units",
  "AvePP (all classes)",          "All classes ≥ .85",        "One or more .70–.84",         "One or more < .70"
)

cat("\n=== TABLE 8: Diagnostic Framework ===\n")
print(table8, n = Inf)
write.csv(table8, file.path(OUT, "Table8_DiagnosticFramework.csv"), row.names = FALSE)

# ── Apply framework to your empirical data
overall_amb70  <- mean(dat_with_post$ambig_70) * 100
sev_amb70      <- table3$Pct_lt70[table3$Class == SEVERE_CLASS]
prev_infl      <- abs(table4$PII[table4$Class == SEVERE_CLASS])
sep_infl_mean  <- mean(sep_inflation$Pct_inflation)
pdi_mean       <- mean(pdi_by_class$PDI)
min_avepp      <- min(table3$AvePP) / 100

rate_concern <- function(val, low, high) {
  if (val < low) "Low" else if (val <= high) "Moderate" else "High"
}

empirical_verdict <- data.frame(
  Diagnostic = c("Overall ambiguity burden (%<.70)",
                 "Severe-class ambiguity (%<.70)",
                 "Prevalence inflation (pp)",
                 "Separation inflation (%)",
                 "Profile discrepancy (PDI)",
                 "Minimum AvePP"),
  Observed   = round(c(overall_amb70, sev_amb70, prev_infl,
                       sep_infl_mean, pdi_mean, min_avepp), 3),
  Concern    = c(rate_concern(overall_amb70, 10, 30),
                 rate_concern(sev_amb70,     10, 25),
                 rate_concern(prev_infl,      2,  5),
                 rate_concern(sep_infl_mean,  5, 15),
                 rate_concern(pdi_mean,     0.02, 0.05),
                 rate_concern(1 - min_avepp, 0.15, 0.30))
)

cat("\n=== EMPIRICAL CONCERN RATING ===\n")
print(empirical_verdict)
write.csv(empirical_verdict,
          file.path(OUT, "EmpiricalConcernRating.csv"), row.names = FALSE)

# ── TABLE 9: Reporting checklist ─────────────────────────────────────────────
checklist <- data.frame(
  Item = c(
    "1. Report the number of latent profiles extracted",
    "2. Report BIC, entropy, and AvePP for chosen solution",
    "3. Report whether hard or weighted assignment was used",
    "4. If hard assignment: report max posterior probability mean by class",
    "5. If hard assignment: report % of ambiguous cases (max_post < 0.70)",
    "6. Report Prevalence Inflation Index (PII) per class",
    "7. Report Separation Inflation Index (SII)",
    "8. Report Profile Discrepancy Index (PDI)",
    "9. Report simulation condition most similar to empirical data",
    "10. Justify hard assignment based on simulation benchmarks"
  ),
  Recommended_Threshold = c(
    "—",
    "Entropy > 0.80; AvePP > 0.90",
    "State explicitly",
    "> 0.90 average",
    "< 10% of sample",
    "< 1 percentage point",
    "< 0.05",
    "< 0.10",
    "Match separation + balance condition",
    "Cite PDI/SII from simulation"
  ),
  stringsAsFactors = FALSE
)

# Safe print — no na.print argument
print(checklist)

# Optional: save to CSV
write.csv(checklist, "table9_reporting_checklist.csv", row.names = FALSE)
cat("Table 9 saved.\n")



# ── RESULTS INVENTORY ──────────────────────────────────────────────
cat("=== WHAT EXISTS IN YOUR ENVIRONMENT ===\n")

# Check key objects
objects_to_check <- c(
  "dat", "dat_with_post", "focal_model", "focal_data",
  "hard_class_vec", "lpa_fit", "profile_summary",
  "ambig_summary", "pii_table", "sii_val", "pdi_val",
  "avePP_table", "sim_results", "checklist"
)

for (obj in objects_to_check) {
  if (exists(obj)) {
    x <- get(obj)
    if (is.data.frame(x)) {
      cat(sprintf("  ✓ %-20s | data.frame | %d rows × %d cols\n", obj, nrow(x), ncol(x)))
    } else if (is.numeric(x)) {
      cat(sprintf("  ✓ %-20s | numeric    | length = %d | value = %s\n", obj, length(x), paste(round(head(x,3),4), collapse=", ")))
    } else {
      cat(sprintf("  ✓ %-20s | %s\n", obj, class(x)[1]))
    }
  } else {
    cat(sprintf("  ✗ %-20s | MISSING\n", obj))
  }
}

cat("\n=== KEY RESULTS SNAPSHOT ===\n")

# Profile means
if (exists("profile_summary")) {
  cat("\n-- Profile Summary (Table 3) --\n")
  print(profile_summary)
}

# Ambiguity
if (exists("ambig_summary")) {
  cat("\n-- Ambiguity Summary (Table 4) --\n")
  print(ambig_summary)
}

# PII
if (exists("pii_table")) {
  cat("\n-- Prevalence Inflation Index (Table 5) --\n")
  print(pii_table)
}

# SII
if (exists("sii_val")) {
  cat("\n-- Separation Inflation Index (SII):", round(sii_val, 4), "\n")
}

# PDI
if (exists("pdi_val")) {
  cat("-- Profile Discrepancy Index (PDI):", round(pdi_val, 4), "\n")
}

# AvePP
if (exists("avePP_table")) {
  cat("\n-- Average Posterior Probability (Table 6) --\n")
  print(avePP_table)
}

# Simulation
if (exists("sim_results")) {
  cat("\n-- Simulation Results: rows =", nrow(sim_results), "| conditions =",
      length(unique(paste(sim_results$separation, sim_results$balance, sim_results$reliability))), "\n")
  cat("   Columns:", paste(names(sim_results), collapse=", "), "\n")
}

# Saved files
cat("\n=== SAVED FILES ===\n")
saved_files <- list.files(pattern = "\\.(csv|png|pdf)$")
if (length(saved_files) == 0) {
  cat("  No CSV/PNG/PDF files found in working directory\n")
  cat("  Working directory:", getwd(), "\n")
} else {
  for (f in saved_files) cat("  ✓", f, "\n")
}





















