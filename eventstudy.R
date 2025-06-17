library(ggplot2)
library(fixest)

# from hosp_data, assign treatment and control group  
# treatment group when first penalized == 2012
hosp_data <- hosp_data %>%
  mutate(treatment = ifelse(first_penalty == "2012", 1, 0)) 

# control group when hospitals are either never penalized or first penalized after 2016 
hosp_data <- hosp_data %>%
  mutate(control = ifelse(first_penalty == "none" | first_penalty > 2016, 1, 0))

# filter out hospitals that are not in treatment or control group
hosp_data <- hosp_data %>% filter(treatment == 1 | control == 1)

# filter relevant years
hosp_data <- hosp_data %>% filter(YEAR <= 2016 & YEAR > 2008) 

# Create a treatment-time interaction variable for DiD
hosp_data <- hosp_data %>%
  mutate(post = ifelse(YEAR >= 2012, 1, 0),  
         did = treatment * post)

#see sums script for counts by group

# FTERN 
# check for parallel trends
hosp_data <- hosp_data %>%
  mutate(event_time = YEAR - 2012)

event_ftern <- feols(
  FTERN ~ i(event_time, treatment, ref = -1) | MCRNUM + YEAR,
  data = hosp_data,
  cluster = ~MCRNUM
  )
summary(event_ftern)
iplot(event_ftern,
      xlab = "Event Time",
      main = "Event Study: Full Time Equivalent RN and 2012 Penalties")

ggsave("plots/event_ftern.png")

# differenece in difference regression 
# TWFE
did_ftern <- feols(
  FTERN ~ did + post + treatment | MCRNUM + YEAR, 
  data = hosp_data
)
summary(did_ftern)

# FTELPN 
# check for parallel trends
event_lpn <- feols(
  FTELPN ~ i(event_time, treatment, ref = -1) | MCRNUM + YEAR,
  data = hosp_data,
  cluster = ~MCRNUM
)
summary(event_lpn)

iplot(event_lpn,
      xlab = "Event Time",
      main = "Event Study: Full Time Equivalent LPNs and 2012 Penalties")

ggsave("plots/event_ftelpn.png")

# diff in diff regression
# TWFE 
did_ftelpn <- feols(
  FTELPN ~ did + post + treatment | MCRNUM + YEAR,  
  data = hosp_data
)
summary(did_ftelpn)


# Honest DID --------------------------------------------#
## Installation
# Install remotes package if not installed
install.packages("remotes")

# Turn off warning-error-conversion, because the tiniest warning stops installation
Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "true")

# install from github
remotes::install_github("asheshrambachan/HonestDiD")

#Install here, dplyr, did, haven, ggplot2, fixest packages from CRAN if not yet installed
install.packages(c("here", "dplyr", "did", "haven", "ggplot2", "fixest"))
library(here)
library(dplyr)
library(did)
library(haven)
library(ggplot2)
library(fixest)
library(HonestDiD)

twfe_results <- fixest::feols(FTERN ~ i(YEAR, treatment, ref = 2011) | MCRNUM + YEAR,
                        cluster = "MCRNUM",
                        data = hosp_data)
summary(twfe_results)

#save results from original did 
betahat <- summary(twfe_results)$coefficients #save the coefficients
sigma <- summary(twfe_results)$cov.scaled #save the covariance matrix
names(betahat)


delta_rm_results <-
HonestDiD::createSensitivityResults_relativeMagnitudes(
                                    betahat = betahat, #coefficients
                                    sigma = sigma, #covariance matrix
                                    numPrePeriods = 2, #num. of pre-treatment coefs
                                    numPostPeriods = 5, #num. of post-treatment coefs
                                    Mbarvec = seq(0.5,2,by=0.5) #values of Mbar
                                    )

delta_rm_results

# original results
originalResults <- HonestDiD::constructOriginalCS(betahat = betahat,
                                                  sigma = sigma,
                                                  numPrePeriods = 2,
                                                  numPostPeriods = 5)


# plot the sensitivity results
sensplot_ftern<- HonestDiD::createSensitivityPlot_relativeMagnitudes(delta_rm_results, originalResults)

sensplot_ftern <- sensplot_ftern +
  labs(
    title = "Sensitivity Analysis for FTERN",
    subtitle = "Honest DiD: Relative Magnitude Method",
    x = "Mbar",
    y = "CI for Treatment Effect"
  )
print(sensplot_ftern)
ggsave("plots/sensplot_ftern.png", plot = sensplot_ftern, width = 8, height = 6)

# honest DiD for FTELPN --------------------------------------------#
twfe_ftelpn <- fixest::feols(FTELPN ~ i(YEAR, treatment, ref = 2011) | MCRNUM + YEAR,
                        cluster = "MCRNUM",
                        data = hosp_data)
summary(twfe_ftelpn)

#save results from original did 
betahat1 <- summary(twfe_ftelpn)$coefficients #save the coefficients
sigma1 <- summary(twfe_ftelpn)$cov.scaled #save the covariance matrix
names(betahat)


delta_rm_ftelpn <-
HonestDiD::createSensitivityResults_relativeMagnitudes(
                                    betahat = betahat1, #coefficients
                                    sigma = sigma1, #covariance matrix
                                    numPrePeriods = 2, #num. of pre-treatment coefs
                                    numPostPeriods = 5, #num. of post-treatment coefs
                                    Mbarvec = seq(0.5,2,by=0.5) #values of Mbar
                                    )

delta_rm_ftelpn

# original results
originalResults <- HonestDiD::constructOriginalCS(betahat = betahat1,
                                                  sigma = sigma1,
                                                  numPrePeriods = 2,
                                                  numPostPeriods = 5)

# plot the sensitivity results
sensplot_ftelpn<- HonestDiD::createSensitivityPlot_relativeMagnitudes(delta_rm_ftelpn, originalResults)

sensplot_ftelpn <- sensplot_ftelpn +
  labs(
    title = "Sensitivity Analysis for FTELPN",
    subtitle = "Honest DiD: Relative Magnitude Method",
    x = "Mbar",
    y = "CI for Treatment Effect"
  )
print(sensplot_ftelpn)
