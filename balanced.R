# I want to analyze stats and trends for various bed groups 
# Helper function to get hospitals that ALWAYS meet the condition
hosp_meeting_condition <- function(data, condition_expr) {
  data %>%
    group_by(MCRNUM) %>%
    summarise(always_true = all({{ condition_expr }}), .groups = "drop") %>%
    filter(always_true) %>%
    pull(MCRNUM)
}

# Hospital IDs by condition over ALL years
hosp_all_ids   <- unique(hosp_2012$MCRNUM)
hosp_30_ids    <- hosp_meeting_condition(hosp_2012, BDTOT >= 30)
hosp_2000_ids  <- hosp_meeting_condition(hosp_2012, BDTOT >= 30 & BDTOT <= 2000)
hosp_1000_ids  <- hosp_meeting_condition(hosp_2012, BDTOT >= 30 & BDTOT <= 1000)

# Now filter the full dataset based on those hospital lists
bed_all <- hosp_2012 %>%
  filter(MCRNUM %in% hosp_all_ids) %>%
  mutate(bed_group = "Full Sample")

bed_30 <- hosp_2012 %>%
  filter(MCRNUM %in% hosp_30_ids) %>%
  mutate(bed_group = "BDTOT >= 30")

bed_2000 <- hosp_2012 %>%
  filter(MCRNUM %in% hosp_2000_ids) %>%
  mutate(bed_group = "30 <= BDTOT <= 2000")

bed_1000 <- hosp_2012 %>%
  filter(MCRNUM %in% hosp_1000_ids) %>%
  mutate(bed_group = "30 <= BDTOT <= 1000")

# Combine all into one
hosp_data_combined <- bind_rows(bed_all, bed_30, bed_2000, bed_1000)
# --------------------------------------------------------#

# Summary Statistics

# COUNTS
# Create a helper function to count unique hospitals by treatment group
count_by_group <- function(data, ids, group_name) {
  data %>%
    filter(MCRNUM %in% ids) %>%
    distinct(MCRNUM, treatment) %>%
    count(treatment, name = "num_hospitals") %>%
    mutate(group = group_name)
}

# Run the counts for each group
count_all   <- count_by_group(hosp_2012, hosp_all_ids,   "Full Sample")
count_30    <- count_by_group(hosp_2012, hosp_30_ids,    "BDTOT >= 30")
count_2000  <- count_by_group(hosp_2012, hosp_2000_ids,  "30 <= BDTOT <= 2000")
count_1000  <- count_by_group(hosp_2012, hosp_1000_ids,  "30 <= BDTOT <= 1000")

# Combine all into one summary table
summary_counts <- bind_rows(count_all, count_30, count_2000, count_1000) %>%
  mutate(treatment = ifelse(treatment == 1, "Treatment", "Control")) %>%
  pivot_wider(names_from = treatment, values_from = num_hospitals)

# View the summary table
print(summary_counts)
kable(summary_counts, format = "latex", booktabs = TRUE,
      caption = "Number of Hospitals per Group",
      align = "lcc")

# SUMMARY TABLE 
# Define the summary function
# Step 1: Define the summary function
summarize_group_stats <- function(data) {
  sum_vars <- c("BDTOT", "FTERN", "FTELPN", "FTEMD", "FTERES", "ADMTOT", "tot_operating_exp", "net_pat_rev")

  summary_long <- data %>%
    group_by(treatment) %>%
    summarise(across(all_of(sum_vars),
                     list(mean = ~mean(.x, na.rm = TRUE),
                          sd = ~sd(.x, na.rm = TRUE)),
                     .names = "{.col}_{.fn}"),
              .groups = "drop") %>%
    pivot_longer(-treatment, names_to = c("variable", "stat"), names_pattern = "(.+)_([^_]+)") %>%
    pivot_wider(names_from = stat, values_from = value, values_fn = mean) %>%
    mutate(
      mean = as.numeric(mean),
      sd = as.numeric(sd),
      stat_string = sprintf("%.2f (%.2f)", mean, sd)
    ) %>%
    select(treatment, variable, stat_string) %>%
    pivot_wider(names_from = treatment, values_from = stat_string, names_prefix = "treatment_")

  colnames(summary_long) <- c("Variable", "Control", "Treatment")

  summary_long$Variable <- recode(summary_long$Variable,
                                  BDTOT = "Beds",
                                  FTERN = "FTE RNs",
                                  FTELPN = "FTE LPNs",
                                  FTEMD = "FTE MDs",
                                  FTERES = "FTE Residents",
                                  ADMTOT = "Total Admissions",
                                  tot_operating_exp = "Operating Expenses",
                                  net_pat_rev = "Patient Revenue")

  return(summary_long)
}

summary_all    <- summarize_group_stats(bed_all)
summary_30     <- summarize_group_stats(bed_30)
summary_2000   <- summarize_group_stats(bed_2000)
summary_1000   <- summarize_group_stats(bed_1000)

# Print LaTeX-ready tables
kable(summary_all, format = "latex", booktabs = TRUE,
      caption = "Summary Statistics for Full Sample",
      align = "lcc")

kable(summary_30, format = "latex", booktabs = TRUE,
      caption = "Summary Statistics for Hospitals with BDTOT >= 30",
      align = "lcc")

kable(summary_2000, format = "latex", booktabs = TRUE,
      caption = "Summary Statistics for Hospitals with 30 <= BDTOT <= 2000",
      align = "lcc")

kable(summary_1000, format = "latex", booktabs = TRUE,
      caption = "Summary Statistics for Hospitals with 30 <= BDTOT <= 1000",
      align = "lcc")


# --------------------------------------------------------#
# Plot demeaned FTERN trends
# demean for ftern  
demean_ftern <- hosp_data_combined %>%
  group_by(bed_group, treatment, YEAR) %>%
  summarise(mean_ftern = mean(FTERN, na.rm = TRUE), .groups = "drop") %>%
  group_by(bed_group, treatment) %>%
  mutate(demeaned_ftern = mean_ftern - first(mean_ftern))

quad_ftern <- ggplot(demean_ftern, aes(x = YEAR, y = demeaned_ftern, color = factor(treatment))) +
  geom_line(size = 1) +
  geom_point() +
  scale_color_manual(values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
                     name = "Group", labels = c("Control", "Treatment")) +
  facet_wrap(~ bed_group, ncol = 2) +
  theme_minimal() +
  labs(
    title = "Demeaned FTERN Over Time by Bed Group",
    x = "Year", y = "Demeaned FTERN"
  )

print(quad_ftern)
ggsave("plots/quad_ftern.png", plot = quad_ftern, width = 10, height = 10, bg = "white")

# Plot FTEMD trends (not demeaned)
mean_ftemd <- hosp_data_combined %>% #filter(FTEMD > 0) %>%
  group_by(bed_group, treatment, YEAR) %>%
  summarise(mean_ftemd = mean(FTEMD, na.rm = TRUE), .groups = "drop")

quad_ftemd <- ggplot(mean_ftemd, aes(x = YEAR, y = mean_ftemd, color = factor(treatment))) +
  geom_line(size = 1) +
  geom_point() +
  scale_color_manual(values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
                     name = "Group", labels = c("Control", "Treatment")) +
  facet_wrap(~ bed_group, ncol = 2) +
  theme_minimal() +
  labs(
    title = "FTEMD Trends Over Time by Bed Group",
    x = "Year", y = "Avg FTEMD"
  )

print(quad_ftemd)
ggsave("plots/quad_ftemd.png", plot = quad_ftemd, width = 10, height = 10, bg = "white")
