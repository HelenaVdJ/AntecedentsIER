# ============================================================
# Antecedents IER — 02 Descriptives
# Helena Van der Jeught
# ============================================================

library(tidyverse)
library(readxl)
library(psych)
library(nlme)
# ============================================================
# Setup
# ============================================================

# Load background data and define excluded couples
bg <- read_excel("data/CCS_BGQuestionnaires_scored_and_SCID.xlsx", na = "NA")

excluded_couples <- bg %>%
  group_by(CoupleID) %>%
  summarise(
    exclude = any(Current_Depressive_Episode == 1 |
                    Bipolar == 1 |
                    Borderline == 1, na.rm = TRUE)
  ) %>%
  filter(exclude == TRUE) %>%
  pull(CoupleID)

# ============================================================
# VMR DATA
# ============================================================

# Load VMR data and filter to final subsample
vmr <- read_excel("data/CCS_VMR_preprocessed.xlsx")

vmr_neg <- vmr %>%
  filter(topic == "negative") %>%
  filter(!CoupleID %in% excluded_couples)
# 72 couples, 144 persons, 2304 rows

# Add partner number variable
vmr_remaining <- vmr_neg %>%
  mutate(partner_no = case_when(PpID < 700 ~ 1, PpID > 700 ~ 2))

# Person-mean center variables for within-person correlations
vmr_remaining <- vmr_remaining %>%
  group_by(PpID) %>%
  mutate(
    neg_aff_own_c = neg_aff_own - mean(neg_aff_own, na.rm = TRUE),
    neg_aff_partner_c = neg_aff_partner - mean(neg_aff_partner, na.rm = TRUE),
    extr_IER_c = extr_IER - mean(extr_IER, na.rm = TRUE)
  ) %>%
  ungroup()

# ============================================================
# Descriptive statistics VMR key variables
# ============================================================

vmr_remaining %>%
  select(neg_aff_own, neg_aff_partner, extr_IER) %>%
  summarise(across(everything(), list(
    mean = ~ mean(., na.rm = TRUE),
    sd = ~ sd(., na.rm = TRUE),
    min = ~ min(., na.rm = TRUE),
    max = ~ max(., na.rm = TRUE)
  ))) %>%
  pivot_longer(everything(),
               names_to = c("variable", "stat"),
               names_sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = stat, values_from = value)

# Descriptive statistics VMR key variables (72 couples, 144 persons)
# ---------------------------------------------------------------
# Variable               Mean    SD    Min   Max
# neg_aff_own            26.2   24.5     0   100  (own negative emotions)
# neg_aff_partner        30.2   25.0     0   100  (perceived negative emotions of partner)
# extr_IER               18.2   23.7     0   100  (IER engagement)
# ---------------------------------------------------------------

# ============================================================
# Restructure to pairwise format
# ============================================================

# Raw variables -- for between-person correlations
vmr_pair <- vmr_remaining %>%
  filter(partner_no == 1) %>%
  select(CoupleID, timepoint, neg_aff_own, neg_aff_partner, extr_IER) %>%
  rename_with(~ paste0(., "_p1"), -c(CoupleID, timepoint)) %>%
  left_join(
    vmr_remaining %>%
      filter(partner_no == 2) %>%
      select(CoupleID, timepoint, neg_aff_own, neg_aff_partner, extr_IER) %>%
      rename_with(~ paste0(., "_p2"), -c(CoupleID, timepoint)),
    by = c("CoupleID", "timepoint")
  )

dim(vmr_pair)  # 1152 rows (72 couples x 16 timepoints), 8 columns

# Centered variables -- for within-person correlations
vmr_pair_c <- vmr_remaining %>%
  filter(partner_no == 1) %>%
  select(CoupleID, timepoint, neg_aff_own_c, neg_aff_partner_c, extr_IER_c) %>%
  rename_with(~ paste0(., "_p1"), -c(CoupleID, timepoint)) %>%
  left_join(
    vmr_remaining %>%
      filter(partner_no == 2) %>%
      select(CoupleID, timepoint, neg_aff_own_c, neg_aff_partner_c, extr_IER_c) %>%
      rename_with(~ paste0(., "_p2"), -c(CoupleID, timepoint)),
    by = c("CoupleID", "timepoint")
  )

dim(vmr_pair_c)  # 1152 rows (72 couples x 16 timepoints), 8 columns

# ============================================================
# Within-person correlations VMR (pairwise format)
# ============================================================

vmr_pair_c %>%
  select(neg_aff_own_c_p1, neg_aff_partner_c_p1, extr_IER_c_p1,
         neg_aff_own_c_p2, neg_aff_partner_c_p2, extr_IER_c_p2) %>%
  corr.test(use = "complete") %>%
  print(short = FALSE)

# Within-person correlations VMR key variables (pairwise format, n = 1152)
# ---------------------------------------------------------------
# Partner 1 (p1) variables:
# neg_aff_own_c_p1     = own negative emotions (person-mean centered)
# neg_aff_partner_c_p1 = perceived negative emotions of partner (person-mean centered)
# extr_IER_c_p1        = IER engagement (person-mean centered)
# Partner 2 (p2) variables: same variables for partner 2
# ---------------------------------------------------------------
# Key within-person correlations:
# neg_aff_own_p1 -- neg_aff_partner_p1:  r = 0.42*** 
# neg_aff_partner_p1 -- extr_IER_p1:     r = 0.25*** 
# neg_aff_own_p1 -- extr_IER_p1:         r = 0.12*** 
# neg_aff_own_p2 -- neg_aff_partner_p2:  r = 0.42***
# neg_aff_partner_p2 -- extr_IER_p2:     r = 0.22***
# neg_aff_own_p2 -- extr_IER_p2:         r = 0.20***
# Cross-partner correlations:
# extr_IER_p1 -- extr_IER_p2:            r = 0.06 (ns) 
# * p < .05, ** p < .01, *** p < .001
# ---------------------------------------------------------------

# ============================================================
# Between-person correlations VMR (pairwise format)
# ============================================================

vmr_pair %>%
  group_by(CoupleID) %>%
  summarise(
    mean_neg_aff_own_p1 = mean(neg_aff_own_p1, na.rm = TRUE),
    mean_neg_aff_partner_p1 = mean(neg_aff_partner_p1, na.rm = TRUE),
    mean_extr_IER_p1 = mean(extr_IER_p1, na.rm = TRUE),
    mean_neg_aff_own_p2 = mean(neg_aff_own_p2, na.rm = TRUE),
    mean_neg_aff_partner_p2 = mean(neg_aff_partner_p2, na.rm = TRUE),
    mean_extr_IER_p2 = mean(extr_IER_p2, na.rm = TRUE)
  ) %>%
  select(-CoupleID) %>%
  corr.test(use = "complete") %>%
  print(short = FALSE)

# Between-person correlations VMR key variables (pairwise format, n = 72 couples)
# ---------------------------------------------------------------
# Partner 1 (p1) variables:
# neg_aff_own_p1     = own negative emotions
# neg_aff_partner_p1 = perceived negative emotions of partner
# extr_IER_p1        = IER engagement
# Partner 2 (p2) variables: same variables for partner 2
# ---------------------------------------------------------------
# Key between-person correlations:
# neg_aff_own_p1 -- neg_aff_partner_p1:   r = 0.60**
# neg_aff_partner_p1 -- extr_IER_p1:      r = 0.54**
# neg_aff_own_p1 -- extr_IER_p1:          r = 0.47**
# neg_aff_own_p2 -- neg_aff_partner_p2:   r = 0.78**
# neg_aff_partner_p2 -- extr_IER_p2:      r = 0.59**
# neg_aff_own_p2 -- extr_IER_p2:          r = 0.57**
# Cross-partner correlations:
# extr_IER_p1 -- extr_IER_p2:             r = 0.15 (ns)
# neg_aff_partner_p1 -- extr_IER_p2:      r = 0.22 (ns)
# ** p < .001
# ---------------------------------------------------------------

# Intraclass correlations (ICC) for key variables
icc_vmr_own <- lme(neg_aff_own ~ 1,
                   random = ~ 1 | CoupleID,
                   data = vmr_remaining,
                   na.action = na.omit)
VarCorr(icc_vmr_own)

icc_vmr_perc <- lme(neg_aff_partner ~ 1,
                    random = ~ 1 | CoupleID,
                    data = vmr_remaining,
                    na.action = na.omit)
VarCorr(icc_vmr_perc)

icc_vmr_ier <- lme(extr_IER ~ 1,
                   random = ~ 1 | CoupleID,
                   data = vmr_remaining,
                   na.action = na.omit)
VarCorr(icc_vmr_ier)

# VMR ICC calculations:
# Own NE:        226.59 / (226.59 + 379.16) = .37
# Perceived NE:  250.60 / (250.60 + 379.18) = .40
# IER engagement: 181.04 / (181.04 + 385.10) = .32

# VMR SDw and SDb
vmr_remaining %>%
  group_by(PpID) %>%
  mutate(
    neg_aff_own_pm = mean(neg_aff_own, na.rm = TRUE),
    neg_aff_partner_pm = mean(neg_aff_partner, na.rm = TRUE),
    extr_IER_pm = mean(extr_IER, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  summarise(
    # SDw = SD of within-person deviations
    SDw_own = sd(neg_aff_own - neg_aff_own_pm, na.rm = TRUE),
    SDw_perc = sd(neg_aff_partner - neg_aff_partner_pm, na.rm = TRUE),
    SDw_IER = sd(extr_IER - extr_IER_pm, na.rm = TRUE),
    # SDb = SD of person means
    SDb_own = sd(neg_aff_own_pm, na.rm = TRUE),
    SDb_perc = sd(neg_aff_partner_pm, na.rm = TRUE),
    SDb_IER = sd(extr_IER_pm, na.rm = TRUE)
  )

# ============================================================
# ESM DATA
# ============================================================

# Load ESM data and filter to final subsample
esm <- read_excel("data/CCS_ESMbeeps_individual.xlsx", na = "NA")

esm_remaining <- esm %>%
  filter(!CoupleID %in% excluded_couples)
# 77 couples, 154 persons

# Add partner number variable
esm_remaining <- esm_remaining %>%
  mutate(partner_no = case_when(PpID < 700 ~ 1, PpID > 700 ~ 2))

# Filter IER to >= 0 per OSF preregistration
esm_remaining <- esm_remaining %>%
  mutate(extrinsicIER = as.numeric(extrinsicIER)) %>%
  mutate(extrinsicIER = ifelse(extrinsicIER < 0, NA, extrinsicIER))

# Person-mean center variables for within-person correlations
esm_remaining <- esm_remaining %>%
  group_by(PpID) %>%
  mutate(
    NA_own_c = as.numeric(NA_own) - mean(as.numeric(NA_own), na.rm = TRUE),
    NA_partner_c = as.numeric(NA_partner) - mean(as.numeric(NA_partner), na.rm = TRUE),
    extrinsicIER_c = extrinsicIER - mean(extrinsicIER, na.rm = TRUE)
  ) %>%
  ungroup()

# ============================================================
# Descriptive statistics ESM key variables
# ============================================================

esm_remaining %>%
  summarise(
    mean_NA_own = mean(as.numeric(NA_own), na.rm = TRUE),
    sd_NA_own = sd(as.numeric(NA_own), na.rm = TRUE),
    min_NA_own = min(as.numeric(NA_own), na.rm = TRUE),
    max_NA_own = max(as.numeric(NA_own), na.rm = TRUE),
    mean_NA_partner = mean(as.numeric(NA_partner), na.rm = TRUE),
    sd_NA_partner = sd(as.numeric(NA_partner), na.rm = TRUE),
    min_NA_partner = min(as.numeric(NA_partner), na.rm = TRUE),
    max_NA_partner = max(as.numeric(NA_partner), na.rm = TRUE),
    mean_IER = mean(extrinsicIER, na.rm = TRUE),
    sd_IER = sd(extrinsicIER, na.rm = TRUE),
    min_IER = min(extrinsicIER, na.rm = TRUE),
    max_IER = max(extrinsicIER, na.rm = TRUE)
  )

# Descriptive statistics ESM key variables (77 couples, 154 persons)
# ---------------------------------------------------------------
# Variable               Mean    SD    Min   Max
# NA_own                 15.5   18.0     0   100  (own negative emotions)
# NA_partner             20.1   18.6     0    99  (perceived negative emotions of partner)
# extrinsicIER            5.5   12.5     0    50  (IER engagement, scores >= 0 only)
# ---------------------------------------------------------------

# ============================================================
# Additional descriptives for method section
# ============================================================

#Load Raw Data-set
esm_raw <- read_excel("data/CCS_ESMbeeps_individual.xlsx", na = "NA") %>%
  filter(!CoupleID %in% excluded_couples) %>%
  mutate(extrinsicIER = as.numeric(extrinsicIER))

# Note: time interval between beeps is computed in 03_Preparation.R,
# after lag exclusions are applied.

# Descriptives for IER scores below 0, 0, and above 0

esm_raw %>%
  filter(extrinsicIER < 0) %>%
  summarise(
    n   = n(),
    pct = round(n() / sum(!is.na(esm_raw$extrinsicIER)) * 100, 2),
    M   = mean(extrinsicIER),
    Mdn = median(extrinsicIER),
    SD  = sd(extrinsicIER),
    min = min(extrinsicIER),
    max = max(extrinsicIER)
  )
# n = 1676 (16.87%), M = -7.05, Mdn = -2, SD = 11.20, range = -50 to -1

esm_raw %>%
  filter(extrinsicIER == 0) %>%
  summarise(
    n   = n(),
    pct = round(n() / sum(!is.na(esm_raw$extrinsicIER)) * 100, 2)
  )
# n = 2749 (27.66%)

esm_raw %>%
  filter(extrinsicIER > 0) %>%
  summarise(
    n   = n(),
    pct = round(n() / sum(!is.na(esm_raw$extrinsicIER)) * 100, 2),
    M   = mean(extrinsicIER),
    Mdn = median(extrinsicIER),
    SD  = sd(extrinsicIER),
    min = min(extrinsicIER),
    max = max(extrinsicIER)
  )
# n = 5512 (55.47%), M = 12.00, Mdn = 9, SD = 11.70, range = 1 to 50

# CORRELATIONS
# ============================================================
# Restructure to pairwise format
# ============================================================

# Raw variables -- for between-person correlations
esm_pair <- esm_remaining %>%
  filter(partner_no == 1) %>%
  select(CoupleID, beepno, NA_own, NA_partner, extrinsicIER) %>%
  rename_with(~ paste0(., "_p1"), -c(CoupleID, beepno)) %>%
  left_join(
    esm_remaining %>%
      filter(partner_no == 2) %>%
      select(CoupleID, beepno, NA_own, NA_partner, extrinsicIER) %>%
      rename_with(~ paste0(., "_p2"), -c(CoupleID, beepno)),
    by = c("CoupleID", "beepno")
  )

dim(esm_pair)
head(esm_pair)

# Centered variables -- for within-person correlations
esm_pair_c <- esm_remaining %>%
  filter(partner_no == 1) %>%
  select(CoupleID, beepno, NA_own_c, NA_partner_c, extrinsicIER_c) %>%
  rename_with(~ paste0(., "_p1"), -c(CoupleID, beepno)) %>%
  left_join(
    esm_remaining %>%
      filter(partner_no == 2) %>%
      select(CoupleID, beepno, NA_own_c, NA_partner_c, extrinsicIER_c) %>%
      rename_with(~ paste0(., "_p2"), -c(CoupleID, beepno)),
    by = c("CoupleID", "beepno")
  )

dim(esm_pair_c)

# ============================================================
# Within-person correlations ESM (pairwise format)
# ============================================================

esm_pair_c %>%
  select(NA_own_c_p1, NA_partner_c_p1, extrinsicIER_c_p1,
         NA_own_c_p2, NA_partner_c_p2, extrinsicIER_c_p2) %>%
  corr.test(use = "complete") %>%
  print(short = FALSE)

# Within-person correlations ESM key variables (pairwise format, n = 3038)
# ---------------------------------------------------------------
# Partner 1 (p1) variables:
# NA_own_c_p1        = own negative emotions (person-mean centered)
# NA_partner_c_p1    = perceived negative emotions of partner (person-mean centered)
# extrinsicIER_c_p1  = IER engagement (person-mean centered)
# Partner 2 (p2) variables: same variables for partner 2
# ---------------------------------------------------------------
# Key within-person correlations:
# NA_own_p1 -- NA_partner_p1:       r =  0.39*** 
# NA_partner_p1 -- extrinsicIER_p1: r =  0.08*** 
# NA_own_p1 -- extrinsicIER_p1:     r = -0.06***
# NA_own_p2 -- NA_partner_p2:       r =  0.37***
# NA_partner_p2 -- extrinsicIER_p2: r =  0.12***
# NA_own_p2 -- extrinsicIER_p2:     r =  0.00 (ns)
# Cross-partner correlations:
# extrinsicIER_p1 -- extrinsicIER_p2: r = 0.14***
# NA_partner_p1 -- extrinsicIER_p2:   r = 0.01 (ns)
# * p < .05, ** p < .01, *** p < .001
# ---------------------------------------------------------------

# ============================================================
# Between-person correlations ESM (pairwise format)
# ============================================================

esm_pair %>%
  group_by(CoupleID) %>%
  summarise(
    mean_NA_own_p1 = mean(as.numeric(NA_own_p1), na.rm = TRUE),
    mean_NA_partner_p1 = mean(as.numeric(NA_partner_p1), na.rm = TRUE),
    mean_extrinsicIER_p1 = mean(extrinsicIER_p1, na.rm = TRUE),
    mean_NA_own_p2 = mean(as.numeric(NA_own_p2), na.rm = TRUE),
    mean_NA_partner_p2 = mean(as.numeric(NA_partner_p2), na.rm = TRUE),
    mean_extrinsicIER_p2 = mean(extrinsicIER_p2, na.rm = TRUE)
  ) %>%
  select(-CoupleID) %>%
  corr.test(use = "complete") %>%
  print(short = FALSE)

# Between-person correlations ESM key variables (pairwise format, n = 77 couples)
# ---------------------------------------------------------------
# Partner 1 (p1) variables:
# NA_own_p1        = own negative emotions
# NA_partner_p1    = perceived negative emotions of partner
# extrinsicIER_p1  = IER engagement
# Partner 2 (p2) variables: same variables for partner 2
# ---------------------------------------------------------------
# Key between-person correlations:
# NA_own_p1 -- NA_partner_p1:           r = 0.71**
# NA_partner_p1 -- extrinsicIER_p1:     r = 0.28* 
# NA_own_p1 -- extrinsicIER_p1:         r = 0.20 (ns)
# NA_own_p2 -- NA_partner_p2:           r = 0.80**
# NA_partner_p2 -- extrinsicIER_p2:     r = 0.33**
# NA_own_p2 -- extrinsicIER_p2:         r = 0.20 (ns)
# Cross-partner correlations:
# extrinsicIER_p1 -- extrinsicIER_p2:   r = 0.41**
# NA_partner_p1 -- extrinsicIER_p2:     r = 0.30*
# * p < .05, ** p < .01, *** p < .001
# ---------------------------------------------------------------

# ICC for ESM
icc_esm_own <- lme(as.numeric(NA_own) ~ 1,
                   random = ~ 1 | CoupleID,
                   data = esm_remaining,
                   na.action = na.omit)
VarCorr(icc_esm_own)

icc_esm_perc <- lme(as.numeric(NA_partner) ~ 1,
                    random = ~ 1 | CoupleID,
                    data = esm_remaining,
                    na.action = na.omit)
VarCorr(icc_esm_perc)

icc_esm_ier <- lme(extrinsicIER ~ 1,
                   random = ~ 1 | CoupleID,
                   data = esm_remaining,
                   na.action = na.omit)
VarCorr(icc_esm_ier)
# ESM ICC calculations:
# Own NE:         92.13 / (92.13 + 233.11) = .28
# Perceived NE:  109.49 / (109.49 + 234.61) = .32
# IER engagement: 26.30 / (26.30 + 134.16) = .16

# ESM SDw and SDb
esm_remaining %>%
  group_by(PpID) %>%
  mutate(
    NA_own_pm = mean(as.numeric(NA_own), na.rm = TRUE),
    NA_partner_pm = mean(as.numeric(NA_partner), na.rm = TRUE),
    extrinsicIER_pm = mean(extrinsicIER, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  summarise(
    SDw_own = sd(as.numeric(NA_own) - NA_own_pm, na.rm = TRUE),
    SDw_perc = sd(as.numeric(NA_partner) - NA_partner_pm, na.rm = TRUE),
    SDw_IER = sd(extrinsicIER - extrinsicIER_pm, na.rm = TRUE),
    SDb_own = sd(NA_own_pm, na.rm = TRUE),
    SDb_perc = sd(NA_partner_pm, na.rm = TRUE),
    SDb_IER = sd(extrinsicIER_pm, na.rm = TRUE)
  )
