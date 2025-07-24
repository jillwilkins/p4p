# ---------------------------------------------------------------# 
# calculate percentage change in beds per hospital 
library(dplyr)
library(tidyr)

# Step 1: Filter for 2011 and 2016, then reshape
dup_hospitals <- hosp_2012 %>%
  count(MCRNUM, YEAR) %>%
  filter(n > 1) %>%
  distinct(MCRNUM)

dup_hospitals

library(dplyr)
library(tidyr)

bed_change <- hosp_2012 %>%
  filter(YEAR %in% c(2011, 2016)) %>%
  group_by(MCRNUM, YEAR) %>%
  summarise(BDTOT = mean(BDTOT, na.rm = TRUE), .groups = "drop") %>%  # collapse multiple rows per year
  pivot_wider(
    names_from = YEAR,
    values_from = BDTOT,
    names_prefix = "beds_"
  ) %>%
  mutate(bed_pct_change_11_16 = (beds_2016 - beds_2011) / beds_2011 * 100)

# join back to main dataset 
hosp_2012 <- hosp_2012 %>%
  left_join(bed_change %>% select(MCRNUM, bed_pct_change_11_16), by = "MCRNUM")

# summary of percentage change in beds
summary(bed_change$bed_pct_change_11_16)

bed_pct_sum <- bed_change %>%
  summarise(
    mean_pct_change   = mean(bed_pct_change_11_16, na.rm = TRUE),
    median_pct_change = median(bed_pct_change_11_16, na.rm = TRUE),
    q25_pct_change    = quantile(bed_pct_change_11_16, 0.25, na.rm = TRUE),
    q75_pct_change    = quantile(bed_pct_change_11_16, 0.75, na.rm = TRUE),
    q90_pct_change    = quantile(bed_pct_change_11_16, 0.90, na.rm = TRUE),
    q95_pct_change    = quantile(bed_pct_change_11_16, 0.95, na.rm = TRUE),
    q99_pct_change    = quantile(bed_pct_change_11_16, 0.99, na.rm = TRUE),
    min_pct_change    = min(bed_pct_change_11_16, na.rm = TRUE),
    max_pct_change    = max(bed_pct_change_11_16, na.rm = TRUE),
    n_missing         = sum(is.na(bed_pct_change_11_16)),
    n_hospitals       = n()
  )
View(bed_pct_sum)
# based on this, i am going to drop the 99% percentile of pct change for my bed matching 


