# ============================================================
# Antecedents IER — 04 Within-level Analyses (H1a, H1b)
# Helena Van der Jeught
# ============================================================

library(tidyverse)
library(nlme)

# Load prepared datasets
vmr <- read.csv("data/vmr_prepared.csv")
esm <- read.csv("data/esm_prepared.csv", na.strings = "NA")

# ============================================================
# VMR — Within-level model (H1a, H1b)
# ============================================================

# Primary within-level model
# IER at t+1 regressed on perceived NE, own NE, and prior IER at time t
# All Level 1 predictors are person-mean centered
# per OSF preregistration

model_within_vmr <- lme(extr_IER ~ neg_aff_partner_cw + neg_aff_own_cr + IER_lag_c,
                        random = list(CoupleID = pdDiag(~ 1 + neg_aff_partner_cw + neg_aff_own_cr + IER_lag_c)),
                        correlation = corCompSymm(form = ~ 1 | CoupleID/timepoint),
                        data = vmr,
                        na.action = na.omit,
                        method = "ML")

summary(model_within_vmr)

# Results VMR within-level model (72 couples, 144 persons):
# --------------------------------------------------------
# Fixed effects:
# (Intercept)                          B = 18.306, t(2085) = 11.35, p < .001
# neg_aff_partner_cw (perceived NE)    B = 0.173,  t(2085) = 4.41,  p < .001  --> H1a supported
# neg_aff_own_cr (own NE)              B = 0.093,  t(2085) = 2.61,  p = .009  --> H1b supported
# IER_lag_c (prior IER)                  B = 0.167,  t(2085) = 5.49,  p < .001

# Random effects:
# Rho = -0.395 (notable negative dyadic non-independence at Level 1)

# Model converged after removing correlations between random effects
# per OSF preregistration convergence protocol (Barr, 2013)

# Number of observations: 2160
# Number of groups: 72 couples, 144 persons

# Extract and display confidence intervals for reporting
# VMR within-level model
intervals(model_within_vmr, which = "fixed")
# Fixed effects:
#                         lower        est.      upper
# (Intercept)             15.145       18.306    21.466
# neg_aff_partner_cw      0.096        0.173     0.250  --> H1a
# neg_aff_own_cr          0.023        0.093     0.163  --> H1b
# IER_lag_c               0.107        0.167     0.226

# ============================================================
# ESM — Within-level model (H1a, H1b)
# ============================================================

# Primary within-level model
# IER at t+1 regressed on perceived NE, own NE, and prior IER at time t
# All Level 1 predictors are person-mean centered
# per OSF preregistration

model_within_esm <- lme(extrinsicIER ~ NA_partner_cw + NA_own_cr + IER_lag_c,
                        random = list(CoupleID = pdDiag(~ 1 + NA_partner_cw + NA_own_cr + IER_lag_c)),
                        correlation = corCompSymm(form = ~ 1 | CoupleID/beepno),
                        data = esm,
                        na.action = na.omit,
                        method = "ML")

summary(model_within_esm)

# Results ESM within-level model (77 couples, 154 persons):
# --------------------------------------------------------
# Fixed effects:
# (Intercept)                          B =  8.012, t(4806) = 13.42, p < .001
# NA_partner_cw (perceived NE)         B =  0.075, t(4806) =  4.75, p < .001  --> significant (H1a supported)
# NA_own_cr (own NE)                   B = -0.050, t(4806) = -2.85, p = .004  --> significant (H1b supported; negative direction)
# IER_lag_c (prior IER)                  B =  0.176, t(4806) =  6.91, p < .001

# Random effects:
# Rho = 0.015 (negligible positive dyadic non-independence at Level 1)

# Model converged after removing correlations between random effects
# per OSF preregistration convergence protocol (Barr, 2013)

# Number of observations: 4886
# Number of groups: 77 couples, 154 persons
# --------------------------------------------------------
# Note: own negative emotions negatively predict IER in ESM data
# suggesting depletion of regulatory resources in daily life
# contrasts with positive direction in VMR data (B = 0.093)


# ESM within-level model confidence intervals
intervals(model_within_esm, which = "fixed")
# Fixed effects:
#                     lower    est.    upper
# (Intercept)         6.842   8.012   9.182
# NA_partner_cw       0.044   0.075   0.106  --> H1a
# NA_own_cr          -0.084  -0.050  -0.016  --> H1b
# IER_lag_c           0.126   0.176   0.225