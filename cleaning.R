# goal is to clean outliers from data
hosp_data_clean <- hosp_data %>%
  filter(!is.na(beds), beds >= 10)

q <- quantile(hosp_data$FTERN, probs = c(0.01, 0.99), na.rm = TRUE)
q_beds <- quantile(hosp_data$beds, probs = c(0.01, 0.99), na.rm = TRUE)

hosp_data_clean <- hosp_data_clean %>%
  filter(FTERN >= q[1], FTERN <= q[2],
         beds >= q_beds[1], beds <= q_beds[2])

hosp_data_clean %>%
  summarise(
    num_beds_zero = sum(beds <= 0, na.rm = TRUE),
    num_ftern_zero = sum(FTERN <= 0, na.rm = TRUE),
    num_missing_beds = sum(is.na(beds)),
    num_missing_ftern = sum(is.na(FTERN))
  )

# get rid of outliers (hospital level) 
# identidy hospitals with extreme values
outlier_hospitals <- hosp_data_clean %>%
  group_by(MCRNUM) %>%
  summarise(
    max_beds = max(beds, na.rm = TRUE)) %>%
  filter(max_beds > 3000) %>%
  pull(MCRNUM)
# remove all observations for those hospitals
hosp_data_clean <- hosp_data_clean %>%
  filter(!MCRNUM %in% outlier_hospitals)

hosp_data_clean %>%
  summarise(
    min_ftern = min(FTERN, na.rm = TRUE),
    q25_ftern = quantile(FTERN, 0.25, na.rm = TRUE),
    median_ftern = median(FTERN, na.rm = TRUE),
    mean_ftern = mean(FTERN, na.rm = TRUE),
    q75_ftern = quantile(FTERN, 0.75, na.rm = TRUE),
    max_ftern = max(FTERN, na.rm = TRUE)
  )

# see what years hospitals were first penalized
hosp_data_clean %>%
  group_by(first_penalty) %>%
  summarise(num_hospitals = n_distinct(MCRNUM)) %>%
  arrange(desc(num_hospitals))


selected_vars <- hosp_data %>%
  select(MCRNUM, FTERN, FTELPN, FTRNTF, PTRNTF, beds, hrrp_payment, PAYTOT, 
         first_penalty, treatment, YEAR, event_time, event_adj)

View(selected_vars)
