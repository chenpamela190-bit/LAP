## Project Title
Hard Assignment vs. Probability-Weighted Summarization in Latent Profile Analysis

## Purpose
This project examines whether **hard class assignment** (assigning each person to a single modal class) provides results similar to **probability-weighted summarization** (using posterior probabilities) in latent profile analysis (LPA).

The script implements a full analysis pipeline using **SCL-90** data, including an empirical benchmark analysis and a simulation study.

## Analysis Components

### 1. Empirical analysis
The empirical section of the script:

- loads raw SCL-90 item data
- performs quality control
- computes nine SCL-90 subscale scores
- fits LPA models with 2 to 5 classes
- selects a focal class solution
- compares hard-assigned and probability-weighted results

### 2. Simulation study
The simulation section evaluates assignment adequacy under a **3 × 3 × 3 design**, varying:

- class separation
- class balance
- indicator discrimination precision

For each condition, the script quantifies the extent to which hard assignment distorts prevalence estimates, class profiles, and ambiguity in the severe class.

## Required Input Data
The script expects an Excel dataset containing the following variables:

- `StudentsID`
- `GENDER`
- `AGE`
- `Q1` to `Q90`

These correspond to the 90 SCL-90 item responses and selected demographic variables.

## Preprocessing Steps
The script applies the following preprocessing steps:

1. loads the raw dataset
2. converts item responses to numeric
3. removes structurally invalid cases (for example, impossible age values)
4. computes nine SCL-90 subscale means
5. removes cases with missing subscale data
6. saves a quality-control summary

## Main Empirical Outputs
The script generates and saves:

- descriptive statistics for the nine SCL-90 subscales
- subscale correlation matrix
- LPA model-fit table for 2–5 classes
- posterior uncertainty metrics by class
- hard vs. weighted class prevalence comparison
- hard vs. weighted class profile means
- profile discrepancy index (PDI)
- separation inflation index (SII)
- empirical concern rating based on the diagnostic framework

## Main Simulation Outputs
The script generates and saves:

- condition-level simulation results
- convergence summary
- ambiguity burden in the severe class
- prevalence bias under hard assignment
- separation inflation under hard assignment
- profile discrepancy under hard assignment
- diagnostic framework table
- reporting checklist

## Figures Produced
The script produces the following figures:

- class profile plots: hard assignment vs. probability-weighted
- distribution of maximum posterior probabilities
- ambiguity burden by class
- simulation heatmap

## R Packages Used
The main packages required are:

- `readxl`
- `mclust`
- `tidyLPA`
- `ggplot2`
- `patchwork`
- `viridis`
- `kableExtra`
- `gt`
- `MASS`
- `mvtnorm`
- `dplyr`
- `tidyr`
- `tibble`
- `purrr`
- `scales`

The script attempts to install missing packages automatically.

## Reproducibility
The script sets a fixed random seed:

```r
set.seed(20260412)
