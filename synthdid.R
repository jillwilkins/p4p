library(dplyr)
library(tidyr)
library(synthdid)

# 1. Summarize (if needed) and remove duplicates
sdid_data <- hosp_filter %>%
  filter(!is.na(MCRNUM), treatment %in% c(0, 1)) %>%
  select(MCRNUM, YEAR, treatment, FTERN, FTEMD, FTELPN) %>%
  drop_na(FTERN) %>%
  drop_na(FTEMD) %>%
  drop_na(FTELPN) %>%
  group_by(MCRNUM, YEAR, treatment) %>%
  summarise(FTERN = mean(FTERN), FTEMD = mean(FTEMD), FTELPN = mean(FTELPN), .groups = "drop")

# build sdid modle and function
estimate_sdid <- function(data, outcome_var, pre_year = 2012) {
  # Pivot data to wide format for the given outcome
  wide_data <- data %>%
    select(MCRNUM, treatment, YEAR, all_of(outcome_var)) %>%
    pivot_wider(names_from = YEAR, values_from = all_of(outcome_var)) %>%
    arrange(treatment)

  # Convert to matrix
  Y <- wide_data %>%
    select(-MCRNUM, -treatment) %>%
    as.matrix()

  # Drop any rows with NA (units with missing years)
  complete_rows <- complete.cases(Y)
  Y <- Y[complete_rows, ]
  wide_data <- wide_data[complete_rows, ]

  # Define control units (N0) and pre-treatment periods (T0)
  N0 <- sum(wide_data$treatment == 0)
  T0 <- sum(as.numeric(colnames(Y)) < pre_year)

  # Estimate treatment effect
  synthdid::synthdid_estimate(Y, N0 = N0, T0 = T0)
}

# sdid results 
tau_ftern <- estimate_sdid(sdid_data, "FTERN")
tau_ftemd <- estimate_sdid(sdid_data, "FTEMD")
tau_ftelpn <- estimate_sdid(sdid_data, "FTELPN")


print(tau_ftern)
print(tau_ftemd)
print(tau_ftelpn)

# plot results 
synthdid_plot(tau_ftern) + ggtitle("Synthetic DiD: FTERN")
synthdid_plot(tau_ftemd) + ggtitle("Synthetic DiD: FTEMD")
synthdid_plot(tau_ftelpn) + ggtitle("Synthetic DiD: FTELPN")

