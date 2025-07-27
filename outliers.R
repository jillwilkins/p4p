# this script is used to analyze outliers
library(ggplot2)

#colored hist for FULL bed FTERN
hist_full <- ggplot(hosp_2012, aes(x = FTERN, fill = factor(treatment))) +
  geom_histogram(bins = 100, position = "identity", alpha = 0.5) +
  scale_x_continuous(limits = c(0, 2000)) +
  scale_fill_manual(values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
                    name = "Group",
                    labels = c("Control", "Treatment")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  labs(x = "Number of FTERN", y = "Count",
       title = "Distribution of FTERN by Treatment Group")


ggsave("plots/hist_group_ftern_full.png" , plot = hist_full)

# colored hist FTERN 
hist <- ggplot(hosp_filter, aes(x = FTERN, fill = factor(treatment))) +
  geom_histogram(bins = 100, position = "identity", alpha = 0.5) +
  scale_x_continuous(limits = c(0, 2000)) +
  scale_fill_manual(values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
                    name = "Group",
                    labels = c("Control", "Treatment")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  labs(x = "Number of FTERN", y = "Count",
       title = "Distribution of FTERN by Treatment Group")
print(hist)

ggsave("plots/hist_group_ftern_filter.png" , plot = hist)

# colored hist FTEMD full bed 
hist_full_md <- ggplot(hosp_2012, aes(x = FTEMD, fill = factor(treatment))) +
  geom_histogram(bins = 100, position = "identity", alpha = 0.5) +
  scale_x_continuous(limits = c(0, 2000)) +
  coord_cartesian(ylim = c(0, 1000)) +
  scale_fill_manual(values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
                    name = "Group",
                    labels = c("Control", "Treatment")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  labs(x = "Number of FTEMD", y = "Count",
       title = "Distribution of FTEMD by Treatment Group")


ggsave("plots/hist_group_ftemd_full.png" , plot = hist_full_md)

# colored hist FTEMD
hist_md <- ggplot(hosp_filter, aes(x = FTEMD, fill = factor(treatment))) +
  geom_histogram(bins = 100, position = "identity", alpha = 0.5) +
  scale_x_continuous(limits = c(0, 2000)) +
  coord_cartesian(ylim = c(0, 1000)) +
  scale_fill_manual(values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
                    name = "Group",
                    labels = c("Control", "Treatment")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  labs(x = "Number of FTEMD", y = "Count",
       title = "Distribution of FTEMD by Treatment Group")

ggsave("plots/hist_group_ftemd_filter.png" , plot = hist_md)

# hospital level colored hist 
# Step 1: Aggregate to the hospital level
hosp_level <- hosp_filter %>%
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


# count the number of hospital/year pairs that have FTEMD = 0
zero_ftemd_count <- hosp_2012 %>%
  filter(FTEMD == 0, ADMTOT >= 25, BDTOT >= 30, BDTOT <= 2000) %>%
  group_by(MCRNUM, YEAR) %>%
  summarise(count = n(), .groups = "drop") %>%
  summarise(total_zero_ftemd = sum(count))
print(zero_ftemd_count)

hosp_2012 %>% # filter() %>%
  group_by(treatment, YEAR) %>%
  summarise(
    min_var = min(BDTOT, na.rm = TRUE),
    q25_var = quantile(BDTOT, 0.25, na.rm = TRUE),
    median_var = median(BDTOT, na.rm = TRUE),
    mean_var = mean(BDTOT, na.rm = TRUE),
    q75_var = quantile(BDTOT, 0.75, na.rm = TRUE),
    max_var = max(BDTOT, na.rm = TRUE)
  )

# Count the number of hospitals with FTEMD > 2000
hosp_filter %>%
  filter(FTEMD > 2000) %>%
  summarise(num_hospitals = n_distinct(MCRNUM))

# how many hospitals in each range?
hosp_2012 %>%
  mutate(threshold_group = case_when(
    ADMTOT > 10000 ~ ">10000",
    ADMTOT > 6000 ~ ">6000",
    ADMTOT > 3500 ~ ">3500",
    ADMTOT > 2000 ~ ">2000",
    ADMTOT > 1000 ~ ">1000",
    ADMTOT > 50 ~ ">50",
    TRUE ~ "≤50"
  )) %>%
  group_by(threshold_group) %>%
  summarise(num_hospitals = n_distinct(MCRNUM), .groups = "drop") %>%
  arrange(desc(threshold_group))

ggplot(hosp_data_clean, aes(x = beds, y = FTERN)) +
  geom_point(alpha = 0.3) +
  theme_minimal()