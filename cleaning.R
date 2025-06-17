library(ggplot2)

ggplot(hosp_data, aes(x = FTERN)) +
  geom_histogram(bins = 100) +
  scale_x_continuous(limits = c(0, 1000)) +
  theme_minimal()

ggplot(hosp_data, aes(x = beds, y = FTERN)) +
  geom_point(alpha = 0.3) +
  scale_y_log10() +
  scale_x_log10() +
  theme_minimal()

hosp_data %>%
  summarise(
    min_ftern = min(FTERN, na.rm = TRUE),
    q25_ftern = quantile(FTERN, 0.25, na.rm = TRUE),
    median_ftern = median(FTERN, na.rm = TRUE),
    mean_ftern = mean(FTERN, na.rm = TRUE),
    q75_ftern = quantile(FTERN, 0.75, na.rm = TRUE),
    max_ftern = max(FTERN, na.rm = TRUE)
  )

q <- quantile(hosp_data$FTERN, probs = c(0.01, 0.99), na.rm = TRUE)

hosp_data_clean <- hosp_data %>%
  filter(FTERN >= q[1], FTERN <= q[2])

hosp_data_clean %>%
  summarise(
    num_beds_zero = sum(beds <= 0, na.rm = TRUE),
    num_ftern_zero = sum(FTERN <= 0, na.rm = TRUE),
    num_missing_beds = sum(is.na(beds)),
    num_missing_ftern = sum(is.na(FTERN))
  )

hosp_data_clean <- hosp_data %>%
  filter(!is.na(beds))

ggplot(hosp_data_clean, aes(x = beds, y = FTERN)) +
  geom_point(alpha = 0.3) +
  theme_minimal()

hosp_data %>%
  filter(FTERN > 6000) %>%
  select(beds, MCRNUM, YEAR, FTERN)

hosp_data <- hosp_data_clean %>%
  filter(FTERN <= 2000, beds <= 5000, beds >= 5)

hosp_data %>%
  mutate(threshold_group = case_when(
    FTERN > 5000 ~ ">5000",
    FTERN > 4000 ~ ">4000",
    FTERN > 3000 ~ ">3000",
    FTERN > 2000 ~ ">2000",
    FTERN > 1000 ~ ">1000",
    TRUE ~ "≤1000"
  )) %>%
  group_by(treatment, threshold_group) %>%
  summarise(num_hospitals = n_distinct(MCRNUM), .groups = "drop") %>%
  arrange(treatment, desc(threshold_group))



