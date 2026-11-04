# ============================================================
# Antecedents IER — 03 Data Preparation
# Helena Van der Jeught
# ============================================================

library(tidyverse)
library(readxl)
library(psych)
library(nlme)

# ============================================================
# Setup — define excluded couples
# ============================================================

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

vmr <- read_excel("data/CCS_VMR_preprocessed.xlsx")

# Filter to negative conversation and exclude diagnosed couples
vmr_neg <- vmr %>%
  filter(topic == "negative") %>%
  filter(!CoupleID %in% excluded_couples)
# Double Checking the prepared vmr data set
n_distinct(vmr_neg$PpID) # 144 participants
n_distinct(vmr_neg$CoupleID) # 72 couples
n_distinct(vmr_neg$timepoint) # 16 timepoints

# Key variables vmr_neg:
# neg_aff_own        = own negative emotions (regulator's self-report)
# neg_aff_partner    = perceived negative emotions of partner (regulator's perception)
# actual_neg_partner = target's actual negative emotions (partner's neg_aff_own) -- created below
# extr_IER           = IER engagement

# Create target's actual negative emotions
# Each person's neg_aff_own is linked to their partner's rows as actual_neg_partner
# This allows us to distinguish between what the regulator perceives (neg_aff_partner)
# and what the target actually reported feeling (actual_neg_partner)
# ID scheme: Person 1 = PpID < 700, Person 2 = PpID + 700
partner_actual <- vmr_neg %>%
  select(CoupleID, PpID, timepoint, neg_aff_own) %>%
  rename(actual_neg_partner = neg_aff_own)

vmr_neg <- vmr_neg %>%
  mutate(PartnerID = ifelse(PpID < 700, PpID + 700, PpID - 700)) %>%
  left_join(partner_actual %>% rename(PartnerID = PpID),
            by = c("CoupleID", "PartnerID", "timepoint"))

# Verify no missing values in key variables
vmr_neg %>%
  select(neg_aff_own, neg_aff_partner, actual_neg_partner, extr_IER) %>%
  summarise(across(everything(), ~ sum(is.na(.))))
# All should be 0

# Person-mean centering
vmr_neg <- vmr_neg %>%
  group_by(PpID) %>%
  mutate(
    # Within-level analyses (H1a, H1b)
    neg_aff_partner_cw = neg_aff_partner - mean(neg_aff_partner, na.rm = TRUE),
    neg_aff_own_cr = neg_aff_own - mean(neg_aff_own, na.rm = TRUE),
    # Between-level analyses (H2a, H2b) -- Truth and Bias model
    actual_neg_partner_ct = actual_neg_partner - mean(actual_neg_partner, na.rm = TRUE),
    neg_aff_partner_ct = neg_aff_partner - mean(actual_neg_partner, na.rm = TRUE)
  ) %>%
  ungroup()

# Verify centering
vmr_neg %>%
  group_by(PpID) %>%
  summarise(
    mean_own = mean(neg_aff_own_cr, na.rm = TRUE),
    mean_partner = mean(neg_aff_partner_cw, na.rm = TRUE)
  ) %>%
  summary()
# All means should be exactly 0

# Create lagged IER variable
vmr_neg <- vmr_neg %>%
  group_by(PpID) %>%
  arrange(PpID, timepoint) %>%
  mutate(IER_lag = lag(extr_IER)) %>%
  ungroup()

# Person-mean center lagged IER variable
vmr_neg <- vmr_neg %>%
  group_by(PpID) %>%
  mutate(IER_lag_c = IER_lag - mean(IER_lag, na.rm = TRUE)) %>%
  ungroup()

# Timepoint 1 should have NA for IER_lag
vmr_neg %>%
  filter(timepoint == 1) %>%
  summarise(n_NA = sum(is.na(IER_lag)),
            n_total = n())
# n_NA should equal n_total (all timepoint 1 rows have NA): check

# Spot check couple 3
vmr_neg %>%
  filter(CoupleID == 3) %>%
  select(PpID, timepoint, extr_IER, IER_lag) %>%
  head(10)
#check

# Save prepared VMR dataset
write.csv(vmr_neg, "data/vmr_prepared.csv", row.names = FALSE)
# 72 couples, 144 persons, 2304 rows

# Confirm saved VMR dataset
vmr_check <- read.csv("data/vmr_prepared.csv")
dim(vmr_check)
names(vmr_check)
# Saved VMR prepared dataset
# 2304 rows, 25 columns
# 72 couples, 144 persons
# New variables added: PartnerID, actual_neg_partner, neg_aff_partner_cw, neg_aff_own_cr,
# actual_neg_partner_ct, neg_aff_partner_ct, IER_lag

# ============================================================
# ESM DATA
# ============================================================

esm <- read_excel("data/CCS_ESMbeeps_individual.xlsx", na = "NA")

# Exclude diagnosed couples
esm_prep <- esm %>%
  filter(!CoupleID %in% excluded_couples)

# Check sample size
n_distinct(esm_prep$PpID)      # 154 persons
n_distinct(esm_prep$CoupleID)  # 77 couples
nrow(esm_prep) #13860 rows

# ESM key variables:
# NA_own             = own negative emotions (regulator's self-report)
# NA_partner         = perceived negative emotions of partner (regulator's perception)
# actual_neg_partner = target's actual negative emotions (partner's NA_own)
# extrinsicIER       = IER engagement
# Note: NA prefix stands for Negative Affect, not missing data

# ============================================================
# Filter IER to >= 0 (hedonic IER only, per OSF preregistration)
# ============================================================

# Check how many values were already missing before filtering
sum(is.na(as.numeric(esm_prep$extrinsicIER)))
# 3923 already missing before filteringn (unanswered beeps)

# Check distribution of non-missing values before filtering
esm_prep %>%
  mutate(extrinsicIER = as.numeric(extrinsicIER)) %>%
  filter(!is.na(extrinsicIER)) %>%
  summarise(
    n_negative = sum(extrinsicIER < 0),
    n_zero = sum(extrinsicIER == 0),
    n_positive = sum(extrinsicIER > 0),
    n_total = n(),
    pct_negative = round(n_negative / n_total * 100, 1),
    pct_zero = round(n_zero / n_total * 100, 1),
    pct_positive = round(n_positive / n_total * 100, 1)
  )
# Distribution before filtering (n = 9937 responded beeps):
# Negative (< 0): 1676 (16.9%) -- set to NA per OSF preregistration
# Zero:           2749 (27.7%)
# Positive (> 0): 5512 (55.5%)

# Set negative values to NA
esm_prep <- esm_prep %>%
  mutate(extrinsicIER = as.numeric(extrinsicIER)) %>%
  mutate(extrinsicIER = ifelse(extrinsicIER < 0, NA, extrinsicIER))
# 1676 negative values set to NA

# Confirm: no negative values remain
sum(esm_prep$extrinsicIER < 0, na.rm = TRUE)
# Should be 0: check

# Total NAs after filtering
sum(is.na(esm_prep$extrinsicIER))
# 5599 total NAs (3923 already missing + 1676 negative values set to NA)

# Create target's actual negative emotions
# Each person's neg_aff_own is linked to their partner's rows as actual_neg_partner
# This allows us to distinguish between what the regulator perceives (neg_aff_partner)
# and what the target actually reported feeling (actual_neg_partner)
# ID scheme: Person 1 = PpID < 700, Person 2 = PpID + 700

partner_actual_esm <- esm_prep %>%
  select(CoupleID, PpID, beepno, NA_own) %>%
  rename(actual_neg_partner = NA_own)

esm_prep <- esm_prep %>%
  mutate(PartnerID = ifelse(PpID < 700, PpID + 700, PpID - 700)) %>%
  left_join(partner_actual_esm %>% rename(PartnerID = PpID),
            by = c("CoupleID", "PartnerID", "beepno"))

# Spot check couple 3
esm_prep %>%
  filter(CoupleID == 3) %>%
  select(PpID, PartnerID, beepno, NA_own, NA_partner, actual_neg_partner) %>%
  head(10)
#confirmed: merge successful

# Check for missing values in key variables
esm_prep %>%
  select(NA_own, NA_partner, actual_neg_partner, extrinsicIER) %>%
  summarise(across(everything(), ~ sum(is.na(.))))
# NA_own: 2654 missing
# NA_partner: 2654 missing
# actual_neg_partner: 2654 missing
# extrinsicIER: 5599 missing (3923 already missing + 1676 negative values set to NA)
# Note: na = "NA" added to read_excel() call to correctly identify
# string "NA" values as missing
# Missing values will be excluded listwise by na.action = na.omit in multilevel models
# per OSF preregistration


# ============================================================
# Person-mean centering
# ============================================================

esm_prep <- esm_prep %>%
  group_by(PpID) %>%
  mutate(
    # Within-level analyses (H1a, H1b)
    NA_partner_cw = as.numeric(NA_partner) - mean(as.numeric(NA_partner), na.rm = TRUE),
    NA_own_cr = as.numeric(NA_own) - mean(as.numeric(NA_own), na.rm = TRUE),
    # Between-level analyses (H2a, H2b) -- Truth and Bias model
    actual_neg_partner_ct = as.numeric(actual_neg_partner) - mean(as.numeric(actual_neg_partner), na.rm = TRUE),
    NA_partner_ct = as.numeric(NA_partner) - mean(as.numeric(actual_neg_partner), na.rm = TRUE)
  ) %>%
  ungroup()

# Verify centering
esm_prep %>%
  group_by(PpID) %>%
  summarise(
    mean_own = mean(NA_own_cr, na.rm = TRUE),
    mean_partner = mean(NA_partner_cw, na.rm = TRUE)
  ) %>%
  summary()
# All means should be exactly 0 (tiny values e.g. -2.977e-15 are floating point rounding errors)


# ============================================================
# Create lagged IER variable
# Exclude first beep of each day (overnight lags) per OSF preregistration
# Exclude beeps following a missing observation per OSF preregistration
# ============================================================

# Create lagged IER variable
esm_prep <- esm_prep %>%
  mutate(date = as.Date(timeStampScheduled)) %>%
  group_by(PpID, date) %>%
  arrange(PpID, timeStampScheduled) %>%
  mutate(first_beep_of_day = row_number() == 1) %>%
  ungroup() %>%
  group_by(PpID) %>%
  arrange(PpID, timeStampScheduled) %>%
  mutate(
    IER_lag = lag(extrinsicIER),
    IER_lag = ifelse(first_beep_of_day, NA, IER_lag),
    prev_IER_missing = is.na(lag(extrinsicIER)) & !first_beep_of_day,
    IER_lag = ifelse(prev_IER_missing, NA, IER_lag)
  ) %>%
  ungroup()

# Person-mean center lagged IER variable
esm_prep <- esm_prep %>%
  group_by(PpID) %>%
  mutate(IER_lag_c = IER_lag - mean(IER_lag, na.rm = TRUE)) %>%
  ungroup()

# Check: first beep of each day should have NA for IER_lag
esm_prep %>%
  filter(first_beep_of_day == TRUE) %>%
  summarise(n_NA = sum(is.na(IER_lag)),
            n_total = n())
# n_NA should equal n_total: 2156

# Check additional beeps excluded due to missing previous observation
sum(esm_prep$prev_IER_missing, na.rm = TRUE) 
# 4813 additional beeps excluded due to missing previous observation
# These beeps span an indeterminate time interval per OSF preregistration

# Time interval between consecutive beeps (valid lags only)
lag_intervals <- esm_prep %>%
  filter(!first_beep_of_day, !prev_IER_missing) %>%
  group_by(PpID) %>%
  arrange(PpID, timeStampScheduled) %>%
  mutate(interval_min = as.numeric(difftime(timeStampScheduled,
                                            lag(timeStampScheduled),
                                            units = "mins"))) %>%
  ungroup()


lag_intervals %>%
  summarise(
    M_interval   = mean(interval_min, na.rm = TRUE),
    SD_interval  = sd(interval_min, na.rm = TRUE),
    min_interval = min(interval_min, na.rm = TRUE),
    max_interval = max(interval_min, na.rm = TRUE)
  )
# M_interval = 74.90, SD_interval = 6.25, min = 59.6, max = 94.1


# Note: this exclusion is already handled implicitly by na.action = na.omit
# in the multilevel models, since IER_lag is NA for these beeps
# Kept here for transparency and consistency with OSF preregistration
# ============================================================
# Save prepared ESM dataset
# ============================================================

write.csv(esm_prep, "data/esm_prepared.csv", row.names = FALSE)

# Confirm saved ESM dataset
esm_check <- read.csv("data/esm_prepared.csv")
dim(esm_check)
names(esm_check)
# Saved ESM prepared dataset
# 13860 rows, 62 columns
# 77 couples, 154 persons
# New variables added: PartnerID, actual_neg_partner, NA_partner_cw, NA_own_cr,
# actual_neg_partner_ct, NA_partner_ct, date, first_beep_of_day, IER_lag, prev_IER_missing
