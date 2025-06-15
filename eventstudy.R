library(ggplot2)
library(fixest)

#Now that i have hosp_data, lets check out parallel trends 
# treatment group when first penalized == 2012
hosp_data <- hosp_data %>%
  mutate(treatment = ifelse(first_penalty == "2012", 1, 0)) 

# control group when hospitals are either never penalized or first penalized after 2016 
hosp_data <- hosp_data %>%
  mutate(control = ifelse(first_penalty == "none" | first_penalty > 2016, 1, 0))

# filter 
hosp_data <- hosp_data %>% filter(YEAR <= 2016 & YEAR > 2008)
hosp_data <- hosp_data %>%
  filter(treatment == 1 | control == 1)

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

# Honest DID 

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

#save results from original did 
betahat <- summary(event_ftern)$coefficients #save the coefficients
# Pull the event-time coefficients only (drops intercept, etc.)
betahat <- coef(event_ftern)
betahat <- betahat[grepl("^event_time::", names(betahat))]
names(betahat)

sigma <- summary(event_ftern)$cov.scaled #save the covariance matrix
sigma <- vcov(event_ftern, cluster = "MCRNUM")
sigma <- sigma[names(betahat), names(betahat)]

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

# ---------------------------------------------------------------# 
# lets see if the number of beds changes over time
# Step 1: Drop hospitals with all missing bed values
hosp_beds_clean <- hosp_data %>%
  group_by(MCRNUM) %>%
  filter(!all(is.na(beds))) %>%  # keep hospitals with at least one non-missing bed value
  ungroup()

# Filter out hospitals with absurdly large bed counts
hosp_beds_clean <- hosp_beds_clean %>%
  filter(beds <= 100000)  # You can adjust this threshold as needed


# Step 2: Compute min and max beds per hospital
bed_change_summary <- hosp_beds_clean %>%
  group_by(MCRNUM, treatment) %>%
  summarise(
    min_beds = min(beds, na.rm = TRUE),
    max_beds = max(beds, na.rm = TRUE),
    bed_change = max_beds - min_beds,
    .groups = "drop"
  )
View(bed_change_summary)

#check outliers!!! 
hosp_data %>%
  filter(MCRNUM == "312018") %>%
  select(YEAR, MCRNUM, FTERN, FTELPN, beds, hrrp_payment) %>%
  arrange(YEAR)

# Step 3: Average bed change by treatment group
bed_change_by_group <- bed_change_summary %>%
  mutate(group = ifelse(treatment == 1, "Treated", "Control")) %>%
  group_by(group) %>%
  summarise(
    avg_bed_change = mean(bed_change, na.rm = TRUE),
    median_bed_change = median(bed_change, na.rm = TRUE),
    sd_bed_change = sd(bed_change, na.rm = TRUE),
    n = n()
  )

bed_change_by_group

