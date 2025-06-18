library(dplyr)
library(fixest)

# 1. Define treatment indicator: 1 if first penalized in 2012 or 2013, else 0
hosp_data <- hosp_data %>%
  mutate(treatment = ifelse(first_penalty %in% c(2012, 2013), 1, 0))

# 2. Create event_time: years relative to first penalty for treated units; NA for controls
hosp_data <- hosp_data %>%
  mutate(event_time = ifelse(treatment == 1, YEAR - first_penalty, NA_integer_))

# 3. Optionally filter data to include only treated units and controls with known event_time
# (You can include never-treated units if you want, but event_time will be NA for them)
analysis_data <- hosp_data %>%
  filter(treatment == 1 | is.na(event_time))  # Keep treated + never treated


event_study <- feols(
  FTERN ~ i(event_time, treatment, ref = -1) | MCRNUM + YEAR,
  data = analysis_data
)

summary(event_study)
iplot(event_study, main = "Event Study: Effect of First Penalty in 2012/2013", xlab = "Years since penalty")
