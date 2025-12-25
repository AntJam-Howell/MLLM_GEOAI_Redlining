# MLLM_GEOAI_Redlining
# MLLM_GeoAI_Redlining

## MLLMs, Street View and Urban Policy-Intelligence: Recovering the Sustainability Effects of Redlining

This repository contains the replication code and data for the paper:

> **"MLLMs, Street View and Urban Policy-Intelligence: Recovering the Sustainability Effects of Redlining"**  
> *npj Urban Sustainability*

---

## Overview

This study evaluates whether multimodal large language models (MLLMs) can derive neighborhood-level sustainability indicators from Google Street View (GSV) imagery and recover the legacy effects of historical redlining in the Phoenix metropolitan area. We compare MLLM-based inference (GPT-4o) against conventional semantic segmentation (ResNet-based) and authoritative benchmarks (ACS poverty rates, GEIE tree canopy coverage).

---

## Repository Structure

```
MLLM_GeoAI_Redlining/
└── MainScript/Dataset/
    ├── DL_LLM_Census_Analysis.R          # Main analysis script
    ├── DL_LLM_Census_Analysis.RData      # R workspace with processed data
    ├── LLM_Results_All_phx_gilbert_Structured_Update.csv   # MLLM inference results
    ├── merged_summary.csv                # Merged summary statistics
    └── README.md                         # This file
```

---

## File Descriptions

### Scripts

| File | Description |
|------|-------------|
| `DL_LLM_Census_Analysis.R` | Main R script for all analyses including: data preparation, prediction accuracy comparisons, OLS/ZIP-FE/SAR regression models, bootstrap inference, and figure/table generation |

### Data

| File | Description |
|------|-------------|
| `DL_LLM_Census_Analysis.RData` | R workspace containing processed spatial data, model objects, and intermediate results |
| `LLM_Results_All_phx_gilbert_Structured_Update.csv` | Image-level MLLM inference results from GPT-4o, including poverty indicators, tree canopy estimates, and housing classifications for Phoenix and Gilbert GSV panoramas |
| `merged_summary.csv` | CBG-level merged dataset combining MLLM predictions, semantic segmentation outputs, and authoritative benchmark data |

---

## Requirements

### R Packages

```r
# Core packages
library(tidyverse)
library(sf)
library(spdep)
library(spatialreg)

# Data retrieval
library(tidycensus)

# Tables and visualization
library(kableExtra)
library(ggplot2)
library(patchwork)

# Additional
library(boot)
library(stargazer)
```

### Software

- R (≥ 4.0)
- RStudio (recommended)

---

## Usage

1. Clone the repository:
   ```bash
   git clone https://github.com/[username]/MLLM_GeoAI_Redlining.git
   ```

2. Open `DL_LLM_Census_Analysis.R` in RStudio

3. Load the workspace:
   ```r
   load("MainScript/Dataset/DL_LLM_Census_Analysis.RData")
   ```

4. Run the analysis script to reproduce all figures and tables

---

## Key Analyses

The script performs the following analyses:

1. **Prediction Accuracy** — Correlations between MLLM/ResNet predictions and authoritative benchmarks (ACS poverty, GEIE canopy)

2. **Redlining Legacy Effects** — OLS, ZIP code fixed effects, and spatial autoregressive (SAR) models comparing measurement approaches

3. **Bootstrap Inference** — Nonparametric bootstrap for robust confidence intervals

4. **ACS Margin of Error Analysis** — Diagnostic analysis of measurement uncertainty by HOLC classification

---

## Data Sources

- **Google Street View** — Panoramic imagery sampled at 100m intervals within CBGs
- **American Community Survey (ACS)** — 5-year poverty estimates (2018–2022)
- **GEIE Tree Canopy** — Tree Equity Score canopy coverage data
- **HOLC Maps** — Historical redlining boundaries from the Mapping Inequality project

---

## Citation

If you use this code or data, please cite:

```bibtex
@article{howell2025mllm,
  title={MLLMs, Street View and Urban Policy-Intelligence: Recovering the Sustainability Effects of Redlining},
  author={Howell, Anthony and [co-authors]},
  journal={npj Urban Sustainability},
  year={2025}
}
```

---

## License

This project is licensed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/), consistent with npj Urban Sustainability's open access policy.

---

## Contact

For questions about the code or data, please open an issue or contact the corresponding author.