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
