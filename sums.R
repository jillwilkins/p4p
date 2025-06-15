library(dplyr)
library(knitr)

# Counts by group
obs_main <- hosp_data %>%
  filter(treatment %in% c(0, 1)) %>%
  group_by(treatment) %>%
  summarise(
    total_obs = n(),
    unique_hospitals = n_distinct(MCRNUM),
    .groups = "drop"
  )

# Counts for other/NA treatment groups
obs_other <- hosp_data %>%
  filter(!treatment %in% c(0, 1) | is.na(treatment)) %>%
  summarise(
    treatment = NA_real_,
    total_obs = n(),
    unique_hospitals = n_distinct(MCRNUM)
  )

# Combine both
summary_all <- bind_rows(obs_main, obs_other)

# Rename treatment labels
summary_all <- summary_all %>%
  mutate(treatment_label = case_when(
    treatment == 0 ~ "Control (0)",
    treatment == 1 ~ "Treatment (1)",
    is.na(treatment) ~ "Total (Other/NA)"
  )) %>%
  select(treatment_label, total_obs, unique_hospitals)

# table
kable(summary_all,
      caption = "Hospital Counts by Treatment Group (Including Other/NA)",
      col.names = c("Group", "Total Observations", "Unique Hospitals"))

