# ============================================================
# Supplementary analyses
# Helena Van der Jeught
# Preregistered and unpreregistered exploratory/supplementary analyses
# ============================================================
library(tidyverse)
library(readxl)
library(psych)
library(nlme)

# ============================================================
# Exploratory analysis/Supplementary analysis 1: tracking accuracy (b1ij) predicts mean IER
# per OSF preregistration
# ============================================================
# VMR
# ============================================================

# Recreate vmr_between
# Rerun Truth and Bias model for VMR
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

re_vmr_fixed <- ranef(model_TB_vmr_fixed) %>%
  rownames_to_column("CoupleID")

re_vmr_person <- re_vmr_fixed %>%
  mutate(CoupleID = as.numeric(CoupleID)) %>%
  left_join(vmr %>% filter(partner_no == 1) %>% select(CoupleID, PpID) %>% distinct(), by = "CoupleID") %>%
  rename(PpID_1 = PpID) %>%
  left_join(vmr %>% filter(partner_no == 2) %>% select(CoupleID, PpID) %>% distinct(), by = "CoupleID") %>%
  rename(PpID_2 = PpID)

re_vmr_long <- bind_rows(
  re_vmr_person %>%
    transmute(CoupleID, PpID = PpID_1, b0ij = partner_no1,
              b1ij = `partner_no1:actual_neg_partner_ct`,
              b2ij = `partner_no1:neg_aff_own_cr`),
  re_vmr_person %>%
    transmute(CoupleID, PpID = PpID_2, b0ij = partner_no2,
              b1ij = `partner_no2:actual_neg_partner_ct`,
              b2ij = `partner_no2:neg_aff_own_cr`)
) %>%
  arrange(CoupleID, PpID)

vmr_between <- vmr %>%
  group_by(CoupleID, PpID) %>%
  summarise(
    mean_IER = mean(extr_IER, na.rm = TRUE),
    mean_actual_neg = mean(actual_neg_partner, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(mean_actual_neg_gmc = mean_actual_neg - mean(mean_actual_neg, na.rm = TRUE)) %>%
  left_join(re_vmr_long, by = c("CoupleID", "PpID"))

# Grand-mean center b1ij per OSF preregistration
vmr_between <- vmr_between %>%
  mutate(b1ij_gmc = b1ij - mean(b1ij, na.rm = TRUE))

# VMR 
model_b1_vmr <- lme(mean_IER ~ b1ij_gmc + mean_actual_neg_gmc,
                    random = ~ 1 | CoupleID,
                    data = vmr_between,
                    na.action = na.omit,
                    method = "ML")

summary(model_b1_vmr)
# Results VMR exploratory (tracking accuracy predicts mean IER):
# --------------------------------------------------------
# Fixed effects:
# (Intercept)              B = 18.241, t(71) = 12.58, p < .001
# b1ij_gmc (tracking accuracy, gmc) B = 5.604, t(70) =  0.27, p = .787  --> not significant
# mean_actual_neg_gmc      B =  0.317, t(70) =  4.00, p < .001
# --------------------------------------------------------
# Conclusion: tracking accuracy does not predict mean IER engagement in VMR data
# Findings interpreted with caution given exploratory nature
# Number of observations: 144, Number of groups: 72 couples

intervals(model_b1_vmr, which = "fixed")
# Fixed effects:
#                         lower    est.    upper
# (Intercept)            15.380  18.241  21.102
# b1ij_gmc              -35.148   5.604  46.356  --> CI includes 0, not significant
# mean_actual_neg_gmc     0.161   0.317   0.474

# ============================================================
# ESM
# ============================================================
# Recreate esm_between
esm <- esm %>%
  mutate(partner_no = case_when(PpID < 700 ~ 1, PpID > 700 ~ 2))
esm$partner_no <- as.factor(esm$partner_no)

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

re_esm_fixed <- ranef(model_TB_esm_fixed) %>%
  rownames_to_column("CoupleID")

re_esm_person <- re_esm_fixed %>%
  mutate(CoupleID = as.numeric(CoupleID)) %>%
  left_join(esm %>% filter(partner_no == 1) %>% select(CoupleID, PpID) %>% distinct(), by = "CoupleID") %>%
  rename(PpID_1 = PpID) %>%
  left_join(esm %>% filter(partner_no == 2) %>% select(CoupleID, PpID) %>% distinct(), by = "CoupleID") %>%
  rename(PpID_2 = PpID)

re_esm_long <- bind_rows(
  re_esm_person %>%
    transmute(CoupleID, PpID = PpID_1, b0ij = partner_no1,
              b1ij = `partner_no1:actual_neg_partner_ct`,
              b2ij = `partner_no1:NA_own_cr`),
  re_esm_person %>%
    transmute(CoupleID, PpID = PpID_2, b0ij = partner_no2,
              b1ij = `partner_no2:actual_neg_partner_ct`,
              b2ij = `partner_no2:NA_own_cr`)
) %>%
  arrange(CoupleID, PpID)

esm_between <- esm %>%
  group_by(CoupleID, PpID) %>%
  summarise(
    mean_IER = mean(extrinsicIER, na.rm = TRUE),
    mean_actual_neg = mean(as.numeric(actual_neg_partner), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(mean_actual_neg_gmc = mean_actual_neg - mean(mean_actual_neg, na.rm = TRUE)) %>%
  left_join(re_esm_long, by = c("CoupleID", "PpID"))

# Grand-mean center b1ij per OSF preregistration
esm_between <- esm_between %>%
  mutate(b1ij_gmc = b1ij - mean(b1ij, na.rm = TRUE))

# ESM 
model_b1_esm <- lme(mean_IER ~ b1ij_gmc + mean_actual_neg_gmc,
                    random = ~ 1 | CoupleID,
                    data = esm_between,
                    na.action = na.omit,
                    method = "ML")

summary(model_b1_esm)
# Results ESM exploratory (tracking accuracy predicts mean IER):
# --------------------------------------------------------
# Fixed effects:
# (Intercept)              B = 8.157, t(76) = 13.45, p < .001
# b1ij_gmc (tracking accuracy, gmc) B = 7.256, t(75) =  2.39, p = .019  --> significant
# mean_actual_neg_gmc      B = 0.119, t(75) =  2.47, p = .016
# --------------------------------------------------------
# Conclusion: regulators who more accurately track their partner's actual
# negative emotions show higher mean IER engagement in daily life
# Effect not found in VMR data
# Findings interpreted with caution given exploratory nature
# Number of observations: 154, Number of groups: 77 couples

intervals(model_b1_esm, which = "fixed")
# Fixed effects:
#                         lower    est.    upper
# (Intercept)             6.961   8.157   9.354
# b1ij_gmc                1.263   7.256  13.249  --> CI excludes 0, significant
# mean_actual_neg_gmc     0.024   0.119   0.213

# ============================================================
# Supplementary Analysis 2: Combined Model of Directional
# and Projection Bias
# Suggested by Peter Kuppens and Eva Ceulemans
# Both predictors entered simultaneously (not preregistered)
# ============================================================

# FIRST RUN 03_Preparation.R and 06_between_analyses.R

# --- Laboratory data ---

model_H2ab_vmr <- lme(mean_IER ~ b0ij_gmc + b2ij_gmc + mean_actual_neg_gmc,
                      random = ~ 1 | CoupleID,
                      correlation = corCompSymm(form = ~ 1 | CoupleID),
                      data = vmr_between,
                      na.action = na.omit,
                      method = "ML")

summary(model_H2ab_vmr)
intervals(model_H2ab_vmr, which = "fixed")

# Results VMR combined model (72 couples, 144 persons):
# Fixed effects:
# (Intercept)          B = 18.24, SE =  1.28, t(71) = 14.24, p < .001
# b0ij_gmc             B =  0.56, SE =  0.09, t(69) =  6.59, p < .001
# b2ij_gmc             B = -3.71, SE = 12.12, t(69) = -0.31, p = .760
# mean_actual_neg_gmc  B =  0.53, SE =  0.08, t(69) =  6.91, p < .001
#
# 95% CIs:
# (Intercept)         [15.72, 20.76]
# b0ij_gmc            [ 0.40,  0.73]
# b2ij_gmc            [-27.55, 20.13]
# mean_actual_neg_gmc [ 0.38,  0.69]

# Correlation between bias scores in laboratory data
cor_robust(vmr_between$b0ij_gmc, vmr_between$b2ij_gmc, vmr_between$CoupleID)
# r = .23, 95% CI [.04, .42], p = .021, n = 144

# --- ESM data ---

model_H2ab_esm <- lme(mean_IER ~ b0ij_gmc + b2ij_gmc + mean_actual_neg_gmc,
                      random = ~ 1 | CoupleID,
                      correlation = corCompSymm(form = ~ 1 | CoupleID),
                      data = esm_between,
                      na.action = na.omit,
                      method = "ML")

summary(model_H2ab_esm)
intervals(model_H2ab_esm, which = "fixed")

# Results ESM combined model (77 couples, 154 persons):
# Fixed effects:
# (Intercept)          B =  8.16, SE =  0.59, t(76) = 13.78, p < .001
# b0ij_gmc             B =  0.14, SE =  0.05, t(74) =  2.97, p = .004
# b2ij_gmc             B =  1.72, SE =  3.47, t(74) =  0.50, p = .622
# mean_actual_neg_gmc  B =  0.18, SE =  0.05, t(74) =  3.27, p = .002
#
# 95% CIs:
# (Intercept)         [ 6.99,  9.32]
# b0ij_gmc            [ 0.05,  0.23]
# b2ij_gmc            [-5.11,  8.55]
# mean_actual_neg_gmc [ 0.07,  0.28]

# Correlation between bias scores in ESM data
cor_robust(esm_between$b0ij_gmc, esm_between$b2ij_gmc, esm_between$CoupleID)
# r = -.03, 95% CI [-.20, .15], p = .766, n = 154
