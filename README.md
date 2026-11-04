# Antecedents of Extrinsic IER: Analysis Scripts
## Helena Van der Jeught (Ghent University)

This repository contains all analysis scripts for the paper "Perceived Negative 
Emotions of the Target and the Regulator's Own Negative Emotions as Antecedents of
Extrinsic Interpersonal Emotion Regulation" by Van der Jeught, H., Versyp, O., 
Ceulemans, E., Kuppens, P., Kalokerinos, E., Verhees, M., Frérart, L., 
Begum Kalkan, R., & Sels, L.

The study examines perceived negative emotions and the regulator's own negative 
emotions as antecedents of extrinsic emotion regulation (IER) engagement, 
using both laboratory (VMR) and daily life (ESM) data from Belgian and Dutch couples.

---

## How to run
Scripts can be inspected without data. Raw data are not publicly available 
due to privacy regulations (AVG/GDPR). For data access requests, 
contact the corresponding author.

---

## Scripts
Run scripts in the following order:

| Script | Description |
|--------|-------------|
| `01a_inspection_exclusion.R` | Data inspection and exclusion of diagnosed couples |
| `01b_demographics.R` | Demographic descriptives |
| `02_descriptives.R` | Descriptive statistics and correlations of key variables |
| `03_preparation.R` | Data preparation: centering, lagging, filtering. **Must be run before scripts 04–07** |
| `04_within_analyses.R` | Within-level analyses (H1a, H1b). **Must be run before scripts 06 & 07** |
| `05_between_analyses.R` | Between-level analyses (H2a, H2b) — Truth and Bias model. **Must be run before scripts 06 & 07** |
| `06_sensitivity_analyses.R` | Sensitivity analyses |
| `07_supplementary_analyses.R` | Supplementary and exploratory analyses |

---

## Key decisions and deviations from preregistration
- 27 couples excluded based on diagnosis criteria (preregistration stated 21)
  - Discrepancy due to a miscalculation in the original preregistration
- Final samples: 72 couples (VMR) and 77 couples (ESM)

---

## Software
- R version 4.5.2 (2025-10-31)
- nlme (v3.1-168)
- tidyverse
- readxl
- psych
- ppcor
