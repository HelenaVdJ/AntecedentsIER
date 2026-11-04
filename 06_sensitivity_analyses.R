# ============================================================
# Antecedents IER — 06 Sensitivity Analyses
# Helena Van der Jeught
# ============================================================

library(tidyverse)
library (readxl)
library(nlme)

# ============================================================
# Setup
# ============================================================
# Load prepared datasets
vmr <- read.csv("data/vmr_prepared.csv")
esm <- read.csv("data/esm_prepared.csv", na.strings = "NA")

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

# Add partner number variable
vmr <- vmr %>%
  mutate(partner_no = case_when(PpID < 700 ~ 1, PpID > 700 ~ 2))
vmr$partner_no <- as.factor(vmr$partner_no)

esm <- esm %>%
  mutate(partner_no = case_when(PpID < 700 ~ 1, PpID > 700 ~ 2))
esm$partner_no <- as.factor(esm$partner_no)


# ============================================================
# Sensitivity analyses: adding target's actual negative emotions
# as covariate to primary within-level models
# per OSF preregistration
# ============================================================

# VMR sensitivity model
model_within_vmr_sensitivity <- lme(extr_IER ~ neg_aff_partner_cw + neg_aff_own_cr + IER_lag_c + actual_neg_partner_ct,
                                    random = list(CoupleID = pdDiag(~ 1 + neg_aff_partner_cw + neg_aff_own_cr + IER_lag_c + actual_neg_partner_ct)),
                                    correlation = corCompSymm(form = ~ 1 | CoupleID/timepoint),
                                    data = vmr,
                                    na.action = na.omit,
                                    method = "ML")

summary(model_within_vmr_sensitivity)

# Results VMR sensitivity model (adding target's actual negative emotions as covariate):
# --------------------------------------------------------
# Fixed effects:
# (Intercept)                               B =  18.306, t(2048) = 11.35, p < .001
# neg_aff_partner_cw (perceived NE)         B =   0.167, t(2048) =  4.20, p < .001  --> significant (H1a supported)
# neg_aff_own_cr (own NE)                   B =   0.085, t(2048) =  2.26, p = .024  --> significant (H1b supported)
# IER_lag (prior IER)                       B =   0.165, t(2048) =  5.41, p < .001
# actual_neg_partner_ct (actual NE)         B =   0.023, t(2048) =  0.82, p = .411  --> not significant
# --------------------------------------------------------
# Rho = -0.396
# Conclusion: effects of perceived and own negative emotions on IER engagement
# hold above and beyond target's actual negative emotions
# Number of observations: 2160, Number of groups: 72 couples
# --------------------------------------------------------

intervals(model_within_vmr_sensitivity, which = "fixed")
# Fixed effects:
#                           lower    est.    upper
# (Intercept)              15.147  18.306  21.466
# neg_aff_partner_cw        0.089   0.167   0.245
# neg_aff_own_cr            0.011   0.085   0.158
# IER_lag_c                 0.105   0.165   0.224
# actual_neg_partner_ct    -0.032   0.023   0.079  --> CI includes 0, not significant

# ESM sensitivity model
model_within_esm_sensitivity <- lme(extrinsicIER ~ NA_partner_cw + NA_own_cr + IER_lag_c + actual_neg_partner_ct,
                                    random = list(CoupleID = pdDiag(~ 1 + NA_partner_cw + NA_own_cr + IER_lag_c + actual_neg_partner_ct)),
                                    correlation = corCompSymm(form = ~ 1 | CoupleID/beepno),
                                    data = esm,
                                    na.action = na.omit,
                                    method = "ML")

summary(model_within_esm_sensitivity)

# Results ESM sensitivity model (adding target's actual negative emotions as covariate):
# --------------------------------------------------------
# Fixed effects:
# (Intercept)                               B =  8.100, t(4485) = 13.38, p < .001
# NA_partner_cw (perceived NE)              B =  0.070, t(4485) =  4.13, p < .001  --> significant (H1a supported)
# NA_own_cr (own NE)                        B = -0.048, t(4485) = -2.67, p = .008  --> significant (H1b supported)
# IER_lag (prior IER)                       B =  0.174, t(4485) = 6.88, p < .001
# actual_neg_partner_ct (actual NE)         B =  0.033, t(4485) =  2.29, p = .022  --> significant
# --------------------------------------------------------
# Rho = 0.019
# Conclusion: effects of perceived and own negative emotions on IER engagement hold above and beyond
# target's actual negative emotions in ESM data
# Unlike VMR data, target's actual negative emotions also significantly predict IER
# Number of observations: 4566, Number of groups: 77 couples

intervals(model_within_esm_sensitivity, which = "fixed")
# Fixed effects:
#                           lower    est.    upper
# (Intercept)               6.914   8.100   9.286
# NA_partner_cw             0.037   0.070   0.104
# NA_own_cr                -0.083  -0.048  -0.013
# IER_lag_c                 0.125   0.174   0.224
# actual_neg_partner_ct     0.005   0.033   0.061  --> CI excludes 0, significant