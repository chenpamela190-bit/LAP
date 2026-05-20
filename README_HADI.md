# HADI: Hard-Assignment Distortion Index

**Manuscript:** "When Is Hard Class Assignment Defensible? Introducing the Hard-Assignment Distortion Index (HADI) for Latent Profile and Latent Class Analysis"

---

## Repository Contents

```
/
├── code/
│   ├── hadi_functions.R                    # Core HADI calculation functions
│   ├── simulation_main.R                   # Monte Carlo simulation script
│   ├── empirical_illustration.R            # Empirical workflow (requires real data)
│   └── create_synthetic_scl90_demo_data.R  # Generates synthetic demonstration data
├── data/
│   └── synthetic_scl90_demo_data.csv       # SYNTHETIC demonstration data (see below)
├── outputs/
│   └── simulation_summary/                 # Simulation summary results
└── README.md
```

---

## About the Synthetic Demonstration Data

**File:** `data/synthetic_scl90_demo_data.csv`

> **IMPORTANT: This dataset is ARTIFICIAL and contains NO real participant records.**

This file is provided solely for:
- demonstrating the HADI workflow and code execution
- allowing readers to run the analysis scripts end-to-end
- supporting reproducibility transparency

### What it is

The synthetic dataset has the same variable structure as the empirical SCL-90-R
illustration reported in the manuscript:

| Variable | Description |
|----------|-------------|
| `ID` | Participant identifier (integer) |
| `Class` | Assigned latent class (1–4) |
| `SOM` | Somatisation subscale mean (scale: 1–5) |
| `OCD` | Obsessive-Compulsive subscale mean |
| `IS` | Interpersonal Sensitivity subscale mean |
| `DEP` | Depression subscale mean |
| `ANX` | Anxiety subscale mean |
| `HOS` | Hostility subscale mean |
| `PHOB` | Phobic Anxiety subscale mean |
| `PAR` | Paranoid Ideation subscale mean |
| `PSY` | Psychoticism subscale mean |

Synthetic class proportions approximate those of the empirical illustration:
- Class 1 (Minimal symptom): ~59.5%
- Class 2 (Mild): ~23.8%
- Class 3 (Moderate): ~11.8%
- Class 4 (High symptom): ~4.9%

### What it is NOT

- It does **not** reproduce or approximate the confidential institutional dataset.
- Running the empirical workflow on this synthetic data will **not** reproduce the
  results reported in the manuscript.
- It is not a substitute for the real data.

### The real empirical dataset

The empirical SCL-90-R dataset used in the manuscript consists of institutional
mental-health screening records from a university student population. This dataset
is **not publicly available** due to:
- participant confidentiality
- ethical restrictions
- institutional data-governance requirements

Requests for access should be directed to the relevant data-custodian institution
and will be subject to institutional approval and applicable data-protection
requirements.

---

## Reproducing the Synthetic Demonstration Data

```r
# Run from the repository root directory
source("code/create_synthetic_scl90_demo_data.R")
# Output: data/synthetic_scl90_demo_data.csv
```

---

## Reproducing the Simulation Results

```r
source("code/simulation_main.R")
```

All simulation outputs are also provided pre-computed in `outputs/simulation_summary/`.

---

## Code Availability

All code in this repository is publicly available at:
**(https://github.com/chenpamela190-bit/HADI)**

---

## Data Availability

| Resource | Status |
|----------|--------|
| Simulation code and functions | Publicly available (this repository) |
| Synthetic SCL-90-R demo data | Publicly available (this repository) |
| Empirical SCL-90-R dataset | **Not available** — confidential institutional records |

---


## Contact

For questions about the code or HADI framework: chenpamela190@gmail.com
