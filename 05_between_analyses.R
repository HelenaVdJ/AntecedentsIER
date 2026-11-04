# ============================================================
# Antecedents IER — 05 Between-level Analyses (H2a, H2b)
# Helena Van der Jeught
# ============================================================

library(tidyverse)
library(nlme)

# Load prepared datasets
vmr <- read.csv("data/vmr_prepared.csv")
esm <- read.csv("data/esm_prepared.csv", na.strings = "NA")

# ============================================================
# VMR — Truth and Bias model (Step 1)
# ============================================================

# Step 1: Decompose perceived negative emotions into truth and bias components
# Level 1: perceived NE regressed on actual NE and own NE
# Random intercept (b0ij) = directional bias
# Random effect on actual NE (b1ij) = tracking accuracy
# Random effect on own NE (b2ij) = projection bias
# Partner-specific random effects estimated using pdBlocked
# per OSF preregistration (West & Kenny, 2011)
# Code suggested by fourth author (Otto Versyp)

# Create partner number variable
vmr <- vmr %>%
  mutate(partner_no = case_when(PpID < 700 ~ 1, PpID > 700 ~ 2))
vmr$partner_no <- as.factor(vmr$partner_no)

model_TB_vmr_fixed <- lme(neg_aff_partner_ct ~ actual_neg_partner_ct + neg_aff_own_cr,
                          random = list(CoupleID = pdBlocked(list(
                            pdDiag(~ partner_no - 1),
                            pdDiag(~ partner_no:actual_neg_partner_ct - 1),
                            pdDiag(~ partner_no:neg_aff_own_cr - 1)))),
                          correlation = corCompSymm(form = ~ 1 | CoupleID/timepoint),
                          weights = varIdent(form = ~ 1 | partner_no),
                          data = vmr,
                          na.action = na.omit,
                          method = "ML")

summary(model_TB_vmr_fixed)

# Results VMR Truth and Bias model:
# --------------------------------------------------------
# Fixed effects (average across all dyads):
# (Intercept)           B = 4.000, t(2230) =  2.76, p = .006
#   --> on average, regulators overestimate partner's negative emotions by 4.00 points
# actual_neg_partner_ct B = 0.196, t(2230) =  8.79, p < .001
#   --> average truth force: perceived NE tracks actual NE
# neg_aff_own_cr        B = 0.395, t(2230) = 15.10, p < .001
#   --> average projection: own NE colors perception of partner's NE

# Random effects (partner-specific, SD):
# b0ij -- directional bias:   SD = 17.10 (partner 1), 16.99 (partner 2)
# b1ij -- tracking accuracy:  SD =  0.18 (partner 1),  0.06 (partner 2)
# b2ij -- projection bias:    SD =  0.22 (partner 1),  0.13 (partner 2)

# Rho = -0.030 (negligible dyadic non-independence at Level 1)
# Number of observations: 2304, Number of groups: 72 couples
# --------------------------------------------------------

# Extract person-level random effects
re_vmr_fixed <- ranef(model_TB_vmr_fixed) %>%
  rownames_to_column("CoupleID")

# Restructure random effects to get one row per person
re_vmr_person <- re_vmr_fixed %>%
  mutate(CoupleID = as.numeric(CoupleID)) %>%
  left_join(vmr %>% 
              filter(partner_no == 1) %>% 
              select(CoupleID, PpID) %>% 
              distinct(), 
            by = "CoupleID") %>%
  rename(PpID_1 = PpID) %>%
  left_join(vmr %>% 
              filter(partner_no == 2) %>% 
              select(CoupleID, PpID) %>% 
              distinct(), 
            by = "CoupleID") %>%
  rename(PpID_2 = PpID)

# Create long format with one row per person
re_vmr_long <- bind_rows(
  re_vmr_person %>%
    transmute(CoupleID, PpID = PpID_1,
              b0ij = partner_no1,
              b1ij = `partner_no1:actual_neg_partner_ct`,
              b2ij = `partner_no1:neg_aff_own_cr`),
  re_vmr_person %>%
    transmute(CoupleID, PpID = PpID_2,
              b0ij = partner_no2,
              b1ij = `partner_no2:actual_neg_partner_ct`,
              b2ij = `partner_no2:neg_aff_own_cr`)
) %>%
  arrange(CoupleID, PpID)

head(re_vmr_long)
# Confirmed: one row per person with person-specific b0ij, b1ij, b2ij
# Example couple 3:
# Person 3:   b0ij =  12.89 (chronic overestimation)
# Person 703: b0ij = -28.24 (chronic underestimation)
# 144 persons, 72 couples
nrow(re_vmr_long)  # 144
n_distinct(re_vmr_long$CoupleID)  # 72

# ============================================================
# VMR: Between-person regression (Step 2)
# ============================================================

# Calculate mean IER and mean actual negative emotions per person
# And merge with random effects
vmr_between <- vmr %>%
  group_by(CoupleID, PpID) %>%
  summarise(
    mean_IER = mean(extr_IER, na.rm = TRUE),
    mean_actual_neg = mean(actual_neg_partner, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(mean_actual_neg_gmc = mean_actual_neg - mean(mean_actual_neg, na.rm = TRUE)) %>%
  left_join(re_vmr_long, by = c("CoupleID", "PpID"))

head(vmr_between)

# All Level-2 predictors will be grand-mean centered
vmr_between <- vmr_between %>%
  mutate(
    b0ij_gmc = b0ij - mean(b0ij, na.rm = TRUE),
    b1ij_gmc = b1ij - mean(b1ij, na.rm = TRUE),
    b2ij_gmc = b2ij - mean(b2ij, na.rm = TRUE)
  )
# H2a: directional bias (b0ij) predicts mean IER
model_H2a_vmr <- lme(mean_IER ~ b0ij_gmc + mean_actual_neg_gmc,
                     random = ~ 1 | CoupleID,
                     correlation = corCompSymm(form = ~ 1 | CoupleID),
                     data = vmr_between,
                     na.action = na.omit,
                     method = "ML")

summary(model_H2a_vmr)

# Results VMR H2a (directional bias predicts mean IER):
# --------------------------------------------------------
# Fixed effects:
# (Intercept)              B = 18.241, t(71) = 14.31, p < .001
# b0ij_gmc (directional b) B =  0.559, t(70) =  6.67, p < .001  --> significant (H2a supported)
# mean_actual_neg_gmc      B =  0.536, t(70) =  6.96, p < .001
# --------------------------------------------------------
# Rho = -0.100 (small negative correlation between partners' residuals)
# Conclusion: regulators who chronically overestimate partner's negative emotions
# show higher average IER engagement
# Number of observations: 144, Number of groups: 72 couples
# --------------------------------------------------------

intervals(model_H2a_vmr, which = "fixed")
# Fixed effects:
#                         lower    est.    upper
# (Intercept)            15.725  18.241  20.757
# b0ij_gmc                0.394   0.559   0.725
# mean_actual_neg_gmc     0.384   0.536   0.688

# H2b: projection bias (b2ij) predicts mean IER
model_H2b_vmr <- lme(mean_IER ~ b2ij_gmc + mean_actual_neg_gmc,
                     random = ~ 1 | CoupleID,
                     correlation = corCompSymm(form = ~ 1 | CoupleID),
                     data = vmr_between,
                     na.action = na.omit,
                     method = "ML")

summary(model_H2b_vmr)

# Results VMR H2b (projection bias predicts mean IER):
# --------------------------------------------------------
# Fixed effects:
# (Intercept)              B = 18.241, t(71) = 14.21, p < .001
# b2ij_gmc (projection bias, gmc)   B = 12.274, t(70) =  0.94, p = .352  --> H2b not supported
# mean_actual_neg_gmc      B =  0.406, t(70) =  5.40, p < .001
# --------------------------------------------------------
# Rho = -0.290
# Conclusion: projection bias does not significantly predict mean IER engagement
# Note: H2b was exploratory with no directional prediction per OSF preregistration
# Number of observations: 144, Number of groups: 72 couples

intervals(model_H2b_vmr, which = "fixed")
# Fixed effects:
#                         lower    est.    upper
# (Intercept)            15.708  18.241  20.774
# b2ij_gmc              -13.602  12.274  38.150  --> CI includes 0, not significant
# mean_actual_neg_gmc     0.257   0.406   0.554


# ============================================================
# ESM - Truth and Bias model (Step 1)
# ============================================================

# Create partner number variable
esm <- esm %>%
  mutate(partner_no = case_when(PpID < 700 ~ 1, PpID > 700 ~ 2))
esm$partner_no <- as.factor(esm$partner_no)

# Step 1: Decompose perceived negative emotions into truth and bias components
# Partner-specific random effects estimated using pdBlocked
# per OSF preregistration (West & Kenny, 2011)
# Suggested by fourth author (Otto Versyp)

model_TB_esm_fixed <- lme(NA_partner_ct ~ actual_neg_partner_ct + NA_own_cr,
                          random = list(CoupleID = pdBlocked(list(
                            pdDiag(~ partner_no - 1),
                            pdDiag(~ partner_no:actual_neg_partner_ct - 1),
                            pdDiag(~ partner_no:NA_own_cr - 1)))),
                          correlation = corCompSymm(form = ~ 1 | CoupleID/beepno),
                          weights = varIdent(form = ~ 1 | partner_no),
                          data = esm,
                          na.action = na.omit,
                          method = "ML")

summary(model_TB_esm_fixed)

# Results ESM Truth and Bias model:
# --------------------------------------------------------
# Fixed effects (average across all dyads):
# (Intercept)           B = 4.697, t(9797) =  5.25, p < .001
#   --> on average, regulators overestimate partner's negative emotions by 4.70 points
# actual_neg_partner_ct B = 0.291, t(9797) = 14.73, p < .001
#   --> average truth force: perceived NE tracks actual NE
# NA_own_cr             B = 0.298, t(9797) = 16.58, p < .001
#   --> average projection: own NE colors perception of partner's NE

# Random effects (partner-specific, SD):
# b0ij -- directional bias:   SD = 11.45 (partner 1), 10.57 (partner 2)
# b1ij -- tracking accuracy:  SD =  0.18 (partner 1),  0.22 (partner 2)
# b2ij -- projection bias:    SD =  0.18 (partner 1),  0.17 (partner 2)

# Rho = 0.033 (negligible dyadic non-independence at Level 1)
# Number of observations: 9876, Number of groups: 77 couples
# --------------------------------------------------------

# Extract person-level random effects
re_esm_fixed <- ranef(model_TB_esm_fixed) %>%
  rownames_to_column("CoupleID")

# Restructure to one row per person
re_esm_person <- re_esm_fixed %>%
  mutate(CoupleID = as.numeric(CoupleID)) %>%
  left_join(esm %>%
              filter(partner_no == 1) %>%
              select(CoupleID, PpID) %>%
              distinct(),
            by = "CoupleID") %>%
  rename(PpID_1 = PpID) %>%
  left_join(esm %>%
              filter(partner_no == 2) %>%
              select(CoupleID, PpID) %>%
              distinct(),
            by = "CoupleID") %>%
  rename(PpID_2 = PpID)

re_esm_long <- bind_rows(
  re_esm_person %>%
    transmute(CoupleID, PpID = PpID_1,
              b0ij = partner_no1,
              b1ij = `partner_no1:actual_neg_partner_ct`,
              b2ij = `partner_no1:NA_own_cr`),
  re_esm_person %>%
    transmute(CoupleID, PpID = PpID_2,
              b0ij = partner_no2,
              b1ij = `partner_no2:actual_neg_partner_ct`,
              b2ij = `partner_no2:NA_own_cr`)
) %>%
  arrange(CoupleID, PpID)

head(re_esm_long)
nrow(re_esm_long)# 154
n_distinct(re_esm_long$CoupleID) # 77

# ============================================================
# ESM — Step 2: Between-person regressions (H2a, H2b)
# ============================================================


# Calculate mean IER and mean actual negative emotions per person
esm_between <- esm %>%
  group_by(CoupleID, PpID) %>%
  summarise(
    mean_IER = mean(extrinsicIER, na.rm = TRUE),
    mean_actual_neg = mean(as.numeric(actual_neg_partner), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(mean_actual_neg_gmc = mean_actual_neg - mean(mean_actual_neg, na.rm = TRUE)) %>%
  left_join(re_esm_long, by = c("CoupleID", "PpID"))

# Grand-mean center b0ij and b2ij per OSF preregistration
esm_between <- esm_between %>%
  mutate(
    b0ij_gmc = b0ij - mean(b0ij, na.rm = TRUE),
    b1ij_gmc = b1ij - mean(b1ij, na.rm = TRUE),
    b2ij_gmc = b2ij - mean(b2ij, na.rm = TRUE)
  )

# H2a: directional bias (b0ij) predicts mean IER
model_H2a_esm <- lme(mean_IER ~ b0ij_gmc + mean_actual_neg_gmc,
                     random = ~ 1 | CoupleID,
                     correlation = corCompSymm(form = ~ 1 | CoupleID),
                     data = esm_between,
                     na.action = na.omit,
                     method = "ML")

summary(model_H2a_esm)

# Results ESM H2a (directional bias predicts mean IER):
# --------------------------------------------------------
# Fixed effects:
# (Intercept)              B = 8.157, t(76) = 13.86, p < .001
# b0ij_gmc (directional bias, gmc)  B = 0.135, t(75) =  2.97, p = .004  --> significant (H2a supported)
# mean_actual_neg_gmc      B = 0.179, t(75) =  3.34, p = .001

# Rho = -0.002 (negligible dyadic non-independence at Level 1)

# Conclusion: regulators who chronically overestimate partner's negative emotions
# show higher average IER engagement in daily life
# Number of observations: 154, Number of groups: 77 couples
# --------------------------------------------------------

intervals(model_H2a_esm, which = "fixed")
# Fixed effects:
#                         lower    est.    upper
# (Intercept)             6.996   8.157   9.319
# b0ij_gmc                0.046   0.135   0.225
# mean_actual_neg_gmc     0.073   0.179   0.285

# H2b: projection bias (b2ij) predicts mean IER
model_H2b_esm <- lme(mean_IER ~ b2ij_gmc + mean_actual_neg_gmc,
                     random = ~ 1 | CoupleID,
                     correlation = corCompSymm(form = ~ 1 | CoupleID),
                     data = esm_between,
                     na.action = na.omit,
                     method = "ML")

summary(model_H2b_esm)

# Results ESM H2b (projection bias predicts mean IER):
# --------------------------------------------------------
# Fixed effects:
# (Intercept)              B = 8.157, t(76) = 13.38, p < .001
# b2ij_gmc (projection bias, gmc)   B = 1.540, t(75) =  0.43, p = .666  --> H2b not supported
# mean_actual_neg_gmc      B = 0.098, t(75) =  2.02, p = .047

# Rho = -0.002 (negligible dyadic non-independence at Level 1)

# Conclusion: projection bias does not significantly predict mean IER engagement
# Note: H2b was exploratory with no directional prediction per OSF preregistration
# Number of observations: 154, Number of groups: 77 couples
# --------------------------------------------------------

intervals(model_H2b_esm, which = "fixed")
# Fixed effects:
#                         lower    est.    upper
# (Intercept)             6.955   8.157   9.359
# b2ij_gmc               -5.470   1.540   8.551  --> CI includes 0, not significant
# mean_actual_neg_gmc     0.003   0.098   0.194
