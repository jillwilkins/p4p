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

# honest for FTERN --------------------------------------------#
#save results from original did 
betahat <- summary(event_ftern)$coefficients #save the coefficients
sigma <- summary(event_ftern)$cov.scaled #save the covariance matrix
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
#save results from original did 
betahat1 <- summary(event_ftelpn)$coefficients #save the coefficients
sigma1 <- summary(event_ftelpn)$cov.scaled #save the covariance matrix


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
ggsave("plots/sensplot_ftelpn.png", plot = sensplot_ftelpn, width = 8, height = 6)

# honest DiD for FTEMD --------------------------------------------#
#save results from original did 
betahat_md <- summary(event_ftemd)$coefficients #save the coefficients
sigma_md <- summary(event_ftemd)$cov.scaled #save the covariance matrix

delta_rm_md <-
HonestDiD::createSensitivityResults_relativeMagnitudes(
                                    betahat = betahat_md, #coefficients
                                    sigma = sigma_md, #covariance matrix
                                    numPrePeriods = 2, #num. of pre-treatment coefs
                                    numPostPeriods = 5, #num. of post-treatment coefs
                                    Mbarvec = seq(0.5,2,by=0.5) #values of Mbar
                                    )

delta_rm_md

# original results
og_md <- HonestDiD::constructOriginalCS(betahat = betahat_md,
                                                  sigma = sigma_md,
                                                  numPrePeriods = 2,
                                                  numPostPeriods = 5)

# plot the sensitivity results
sensplot_ftemd<- HonestDiD::createSensitivityPlot_relativeMagnitudes(delta_rm_md, og_md)

sensplot_ftemd <- sensplot_ftemd +
  labs(
    title = "Sensitivity Analysis for FTEMD",
    subtitle = "Honest DiD: Relative Magnitude Method",
    x = "Mbar",
    y = "CI for Treatment Effect"
  )
print(sensplot_ftemd)
ggsave("plots/sensplot_ftelmd.png", plot = sensplot_ftemd, width = 8, height = 6)
