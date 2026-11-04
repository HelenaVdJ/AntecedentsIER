# ============================================================
# Antecedents IER — 01b Demographics
# Helena Van der Jeught
# ============================================================

library(tidyverse)
library(readxl)

# ============================================================
# Setup
# ============================================================

# Load background data
bg <- read_excel("data/CCS_BGQuestionnaires_scored_and_SCID.xlsx", na = "NA")

# Define excluded couples
excluded_couples <- bg %>%
  group_by(CoupleID) %>%
  summarise(
    exclude = any(Current_Depressive_Episode == 1 |
                    Bipolar == 1 |
                    Borderline == 1, na.rm = TRUE)
  ) %>%
  filter(exclude == TRUE) %>%
  pull(CoupleID)

# Load VMR and ESM data to get final subsamples
vmr <- read_excel("data/CCS_VMR_preprocessed.xlsx")
vmr_remaining <- vmr %>%
  filter(topic == "negative") %>%
  filter(!CoupleID %in% excluded_couples)

esm <- read_excel("data/CCS_ESMbeeps_individual.xlsx", na = "NA")
esm_remaining <- esm %>%
  filter(!CoupleID %in% excluded_couples)

# ============================================================
# Filter to final subsamples
# ============================================================

bg_lab <- bg %>%
  filter(!CoupleID %in% excluded_couples) %>%
  filter(PpID %in% unique(vmr_remaining$PpID))
# 144 persons, 72 couples

bg_esm <- bg %>%
  filter(!CoupleID %in% excluded_couples) %>%
  filter(CoupleID %in% unique(esm_remaining$CoupleID))
# 154 persons, 77 couples

# ============================================================
# Demographics — Lab sample
# ============================================================

# Age
bg_lab %>%
  summarise(
    mean_age = mean(as.numeric(AgeYEARS), na.rm = TRUE),
    sd_age = sd(as.numeric(AgeYEARS), na.rm = TRUE),
    min_age = min(as.numeric(AgeYEARS), na.rm = TRUE),
    max_age = max(as.numeric(AgeYEARS), na.rm = TRUE)
  )
# Mean age = 30.7 years (SD = 8.37, range = 19-56)


# Gender (1 = man, 2 = woman, 3 = other)
table(bg_lab$gender)
# 68 men, 75 women, 1 other
# Note: 68 + 75 + 1 = 144 persons -- correct

# Sexual orientation (1 = heterosexual, 2 = bisexual, 3 = homosexual, 4 = other)
table(bg_lab$sexOrientation)
# 119 heterosexual, 12 bisexual, 9 homosexual, 4 other

# Check same-sex couples
bg_lab %>%
  select(CoupleID, gender) %>%
  group_by(CoupleID) %>%
  summarise(
    n_men = sum(gender == 1, na.rm = TRUE),
    n_women = sum(gender == 2, na.rm = TRUE),
    n_other = sum(gender == 3, na.rm = TRUE)
  ) %>%
  mutate(same_sex = n_men == 2 | n_women == 2) %>%
  count(same_sex)
# Same-sex couples
# 7 same-sex couples (9.7%), 65 different-sex couples (90.3%)

# Nationality (4 = Belgian, 5 = Dutch, 6 = other)
table(bg_lab$nationality)
# 138 Belgian, 5 Dutch, 1 other

#Ethnicity (1 = Asian, 2 = Black or Afro-American, 3 = Spanish or Latino, 4 = Whit,; 5 = other)
table(bg_lab$etnicity)
# 3 Asian, 139 White, 2 other

# Relationship duration in months
bg_lab %>%
  summarise(
    mean_rel = mean(as.numeric(RelDurMonths), na.rm = TRUE),
    sd_rel = sd(as.numeric(RelDurMonths), na.rm = TRUE)
    )
# Mean relationship duration = 87.40 months (SD = 75.70), approximately 7.32 years (SD = 6.31)

 
# Married (1 = yes, 2 = no)
table(bg_lab$married, useNA = "always")
# 44 married (22 couples), 98 not married (49 couples), 2 missing (1 couple)

# Living together (1 = not living together, 2 = living together)
table(bg_lab$livingTogether, useNA = "always")
# 1 not living together, 143 living together

# Children (0 = no, 1 = yes)
table(bg_lab$childrenYN)
# 110 without children, 34 with children

# Education (1 = primary, 2 = secondary, 3 = bachelor, 4 = master, 5 = doctorate)
table(bg_lab$edu)
# 1 primary: 1, 2 secondary: 31, 3 bachelor: 53, 4 master: 58, 5 doctorate: 1

# Education -- higher education = bachelor (3) or master (4) or doctorate (5)
bg_lab %>%
  summarise(
    n_higher = sum(edu %in% c(3, 4, 5), na.rm = TRUE),
    n_total = sum(!is.na(edu)),
    pct_higher = round(n_higher / n_total * 100, 1)
  )
# ============================================================
# Demographics — ESM sample
# ============================================================

# Age
bg_esm %>%
  summarise(
    mean_age = mean(as.numeric(AgeYEARS), na.rm = TRUE),
    sd_age = sd(as.numeric(AgeYEARS), na.rm = TRUE),
    min_age = min(as.numeric(AgeYEARS), na.rm = TRUE),
    max_age = max(as.numeric(AgeYEARS), na.rm = TRUE))
# Mean age = 31.0 years (SD = 8.31, range = 19-56)

# warning message
# Check non-numeric values in AgeYEARS for ESM sample
bg_esm %>%
  filter(is.na(as.numeric(AgeYEARS))) %>%
  select(PpID, CoupleID, AgeYEARS)
# 1 missing value: PpID 703, couple 3

# Gender (1 = man, 2 = woman, 3 = other)
table(bg_esm$gender)
# 73 men, 80 women, 1 other

# Sexual orientation (1 = heterosexual, 2 = bisexual, 3 = homosexual, 4 = other)
table(bg_esm$sexOrientation)
# 129 heterosexual, 12 bisexual, 9 homosexual, 4 other

# Same-sex couples ESM
bg_esm %>%
  select(CoupleID, gender) %>%
  group_by(CoupleID) %>%
  summarise(
    n_men = sum(gender == 1, na.rm = TRUE),
    n_women = sum(gender == 2, na.rm = TRUE),
    n_other = sum(gender == 3, na.rm = TRUE)
  ) %>%
  mutate(same_sex = n_men == 2 | n_women == 2) %>%
  count(same_sex)
# Same-sex couples ESM
# 7 same-sex couples (9.1%), 70 different-sex couples (90.9%)

# Nationality (4 = Belgian, 5 = Dutch, 6 = other)
table(bg_esm$nationality)
# 144 Belgian, 7 Dutch, 3 other

#Ethnicity (1 = Asian, 2 = Black or Afro-American, 3 = Spanish or Latino, 4 = Whit,; 5 = other)
table(bg_esm$etnicity)
# 3 Asian, 149 White, 2 other

# Relationship duration in months
bg_esm %>%
  summarise(
    mean_rel_months = mean(as.numeric(RelDurMonths), na.rm = TRUE),
    sd_rel_months   = sd(as.numeric(RelDurMonths), na.rm = TRUE),
    mean_rel_years  = mean(as.numeric(RelDurMonths), na.rm = TRUE) / 12,
    sd_rel_years    = sd(as.numeric(RelDurMonths), na.rm = TRUE) / 12
  )

# Mean relationship duration = 86 months (SD = 74.3),
# approximately 7.20 years (SD = 6.2 years)


# Married (1 = yes, 2 = no)
table(bg_esm$married, useNA = "always")
# 47 married, 104 not married, 3 missing

# Living together (1 = not living together, 2 = living together)
table(bg_esm$livingTogether, useNA = "always")
# 1 not living together, 153 living together

# Children (0 = no, 1 = yes)
table(bg_esm$childrenYN)
# 118 without children, 36 with children

table(bg_esm$edu)
# primary: 1, secondary: 33, bachelor: 56, master: 62, doctorate: 2

# Education ESM -- higher education = bachelor (3) or master (4) or doctorate (5)
bg_esm %>%
  summarise(
    n_higher = sum(edu %in% c(3, 4, 5), na.rm = TRUE),
    n_total = sum(!is.na(edu)),
    pct_higher = round(n_higher / n_total * 100, 1)
  )

# ESM compliance per person
esm_remaining %>%
  group_by(PpID) %>%
  summarise(compliance = first(as.numeric(compliance))) %>%
  summarise(
    mean_compliance = round(mean(compliance, na.rm = TRUE) * 100, 1),
    sd_compliance = round(sd(compliance, na.rm = TRUE) * 100, 1),
    min_compliance = round(min(compliance, na.rm = TRUE) * 100, 1),
    max_compliance = round(max(compliance, na.rm = TRUE) * 100, 1)
  )
# ESM compliance per person
# Mean = 80.9% (SD = 14.2%, range = 21.1% - 98.9%)