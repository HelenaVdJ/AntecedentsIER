# ============================================================
# Antecedents IER — 01a Data Inspection
# Helena Van der Jeught
# ============================================================

library(tidyverse)
library(readxl)

# ============================================================
# Background Questionnaires
# ============================================================

bg <- read_excel("data/CCS_BGQuestionnaires_scored_and_SCID.xlsx")

dim(bg)                  # 208 participants, 417 variables
n_distinct(bg$CoupleID)  # 104 couples

# ============================================================
# Exclusion criteria
# ============================================================

# Exclude couples where at least one partner has a current diagnosis of:
# depressive episode, bipolar disorder, or borderline personality disorder,
# per OSF preregistration

excluded_couples <- bg %>%
  group_by(CoupleID) %>%
  summarise(
    exclude = any(Current_Depressive_Episode == 1 | 
                    Bipolar == 1 | 
                    Borderline == 1, na.rm = TRUE)
  ) %>%
  filter(exclude == TRUE) %>%
  pull(CoupleID)

length(excluded_couples)
# Result: 27 couples excluded
# Expected per OSF preregistration: 21 couples
# Discrepancy explained: the original count included low-compliance couples,
# which was not part of the preregistered exclusion criteria.

# Individual diagnosis counts
table(bg$Current_Depressive_Episode)  # 17 individuals with current depressive episode
table(bg$Bipolar)                     # 9 individuals with bipolar disorder
table(bg$Borderline)                  # 16 individuals with borderline pd

# Breakdown at couple level
bg %>%
  group_by(CoupleID) %>%
  summarise(
    n_partners_with_diagnosis = sum(Current_Depressive_Episode == 1 | 
                                      Bipolar == 1 | 
                                      Borderline == 1, na.rm = TRUE)
  ) %>%
  filter(n_partners_with_diagnosis > 0) %>%
  count(n_partners_with_diagnosis)
# 23 couples have 1 partner with a diagnosis
# 4 couples have 2 partners with a diagnosis

# Total individuals with a diagnosis across all couples
sum(bg$Current_Depressive_Episode == 1 | 
      bg$Bipolar == 1 | 
      bg$Borderline == 1, na.rm = TRUE)
# 31 individuals across 27 couples


# ============================================================
# VMR DATA
# ============================================================

vmr <- read_excel("data/CCS_VMR_preprocessed.xlsx")

dim(vmr)    # 6272 rows, 17 columns
names(vmr)  # variable names

n_distinct(vmr$PpID)      # 196 participants
n_distinct(vmr$CoupleID)  # 98 couples

# Which couples from background data are absent from VMR?
setdiff(unique(bg$CoupleID), unique(vmr$CoupleID))
# Couples 22, 57, 59, 84, 108, 119 absent from VMR data (104 - 6 = 98)

vmr %>%
  count(PpID) %>%
  summary()
# Each participant has exactly 32 rows (2 conversations x 16 assessments)

# Key variables:
# neg_aff_own     = own negative emotions
# neg_aff_partner = perceived negative emotions of partner
# extr_IER        = IER engagement
# timepoint       = assessment moment (1-16)
# topic           = conversation type (positive or negative)

# Check conversation types
table(vmr$topic)
# negative: 3136 rows, positive: 3136 rows

# Filter to negative conversation only
vmr_neg <- vmr %>%
  filter(topic == "negative")
# 3136 rows, 98 couples, 196 persons

# Of the 27 excluded couples, how many are present in VMR data?
intersect(excluded_couples, unique(vmr_neg$CoupleID))
length(intersect(excluded_couples, unique(vmr_neg$CoupleID)))
# Result: 26 of 27 excluded couples present in VMR data
setdiff(excluded_couples, unique(vmr_neg$CoupleID))
# Couple 59 is the excluded couple absent from VMR data

# VMR subsample after exclusion
vmr_remaining <- vmr_neg %>%
  filter(!CoupleID %in% excluded_couples)

n_distinct(vmr_remaining$CoupleID)  # 72 couples (98 - 26 = 72)
n_distinct(vmr_remaining$PpID)      # 144 persons

# ============================================================
# ESM DATA
# ============================================================

esm <- read_excel("data/CCS_ESMbeeps_individual.xlsx", na = "NA")

dim(esm)    # 18560 rows, 51 columns
names(esm)

n_distinct(esm$CoupleID)  # 103 couples (1 couple absent vs. 104 expected)
n_distinct(esm$PpID)      # 206 persons

# How many excluded couples are present in ESM data?
length(intersect(excluded_couples, unique(esm$CoupleID)))
# Result: 26 of 27 excluded couples present in ESM data
# Same couple absent from both VMR and ESM data

# ESM subsample after exclusion
esm_remaining <- esm %>%
  filter(!CoupleID %in% excluded_couples)

n_distinct(esm_remaining$CoupleID)  # 77 couples
n_distinct(esm_remaining$PpID)      # 154 persons
# 103 - 26 = 77 couples

# ============================================================
# Summary: which excluded couple is absent from both datasets?
# ============================================================

# One couple is absent from both VMR and ESM data.
# This couple meets exclusion criteria (diagnoses present for both partners).
# Of the 27 excluded couples, 26 are present in VMR and 26 in ESM.
