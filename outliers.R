# this script is used to analyze outliers
library(ggplot2)

#histogram of ftern
hist_ftern <- ggplot(hosp_data, aes(x = FTERN)) +
  geom_histogram(bins = 100) +
  scale_x_continuous(limits = c(0, 1000)) +
  theme_minimal()
ggsave("plots/histogram_ftern.png")

# full skew histogram
ggplot(hosp_data, aes(x = FTERN)) +
  geom_histogram(bins = 100) +
  scale_x_continuous(limits = c(0, 2000)) +
  theme_minimal()
ggsave("plots/hist_skew_ftern.png")

# colored hist 
ggplot(hosp_data, aes(x = FTERN, fill = factor(treatment))) +
  geom_histogram(bins = 100, position = "identity", alpha = 0.5) +
  scale_x_continuous(limits = c(0, 2000)) +
  scale_fill_manual(values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
                    name = "Group",
                    labels = c("Control", "Treatment")) +
  theme_minimal() +
  labs(x = "Number of FTERN", y = "Count",
       title = "Distribution of FTERN by Treatment Group")

ggsave("plots/hist_group_ftern.png")

# hospital level colored hist 
# Step 1: Aggregate to the hospital level
hosp_level <- hosp_data %>%
  group_by(MCRNUM, treatment) %>%
  summarise(avg_FTERN = mean(FTERN, na.rm = TRUE), .groups = "drop")

# Step 2: Plot the histogram
ggplot(hosp_level, aes(x = avg_FTERN, fill = factor(treatment))) +
  geom_histogram(bins = 50, position = "identity", alpha = 0.5) +
  scale_x_continuous(limits = c(0, 1000)) +
  scale_fill_manual(values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
                    name = "Group",
                    labels = c("Control", "Treatment")) +
  theme_minimal() +
  labs(x = "Average FTERN per Hospital",
       y = "Number of Hospitals",
       title = "Distribution of Hospital-Level FTERN by Group")

hosp_data %>%
  summarise(
    min_ftern = min(FTERN, na.rm = TRUE),
    q25_ftern = quantile(FTERN, 0.25, na.rm = TRUE),
    median_ftern = median(FTERN, na.rm = TRUE),
    mean_ftern = mean(FTERN, na.rm = TRUE),
    q75_ftern = quantile(FTERN, 0.75, na.rm = TRUE),
    max_ftern = max(FTERN, na.rm = TRUE)
  )

# check for outliers 
hosp_data_clean %>%
  mutate(threshold_group = case_when(
    FTERN > 5000 ~ ">5000",
    FTERN > 4000 ~ ">4000",
    FTERN > 3000 ~ ">3000",
    FTERN > 2000 ~ ">2000",
    FTERN > 1000 ~ ">1000",
    TRUE ~ "≤1000"
  )) %>%
  group_by(threshold_group) %>%
  summarise(num_hospitals = n_distinct(MCRNUM), .groups = "drop") %>%
  arrange(desc(threshold_group))

ggplot(hosp_data_clean, aes(x = beds, y = FTERN)) +
  geom_point(alpha = 0.3) +
  theme_minimal()