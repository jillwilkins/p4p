# general for all matching
# find estimates by matching 
library(dplyr)
install.packages("MatchIt")
library(MatchIt)
# You may need devtools first
install.packages("devtools")
devtools::install_github("grantmcdermott/fixestExtra")
library(fixestExtra)

# Calculate the 99th percentile cutoff for beds (levels and pct change)
bed_cutoff <- quantile(hosp_2012$BDTOT, 0.99, na.rm = TRUE)
bed_pct_cutoff <- quantile(hosp_2012$bed_pct_change_11_16, 0.99, na.rm = TRUE)

# average BDTOT per hospital years 2008 - 2011 
hosp_2012 <- hosp_2012 %>%
  group_by(MCRNUM) %>%
  mutate(bed_avg = mean(BDTOT[YEAR >= 2008 & YEAR <= 2011], na.rm = TRUE)) %>%
  ungroup()

# Print the cutoffs for reference
cat("Bed level 99th percentile cutoff:", bed_cutoff, "\n")
cat("Bed percentage change 99th percentile cutoff:", bed_pct_cutoff, "%\n")

# Filter out hospitals that have ANY year with BDTOT < 30 or ADMTOT < 25
# identify hospitals that meet criteria for ALL years
valid_hospitals <- hosp_2012 %>%
  group_by(MCRNUM) %>%
  summarise(
    min_BDTOT = min(BDTOT, na.rm = TRUE),
    min_ADMTOT = min(ADMTOT, na.rm = TRUE),
    meets_criteria = min_BDTOT >= 30 & min_ADMTOT >= 25,
    .groups = "drop"
  ) %>%
  filter(meets_criteria) %>%
  pull(MCRNUM)

cat("Original number of hospitals:", length(unique(hosp_2012$MCRNUM)), "\n")
cat("Hospitals meeting criteria for all years:", length(valid_hospitals), "\n")

# filter the main dataset to keep only valid hospitals
hosp_2012 <- hosp_2012 %>%
  filter(MCRNUM %in% valid_hospitals & YEAR >= 2008)

cat("Observations after filtering:", nrow(hosp_2012), "\n\n")
cat("Number of hospitals after all filters:", nrow(match), "\n")  

summary(hosp_2012$bed_avg)

# match using nearest neighbor with caliper
match_near <- matchit(
  treatment ~ bed_avg + ADMTOT,
  data = hosp_2012 %>% filter(!is.na(bed_avg)),
  method = "nearest",
  distance = "logit",
  caliper = 0.1
)

matched_df <- match.data(match_near)
matched_mcrnums <- matched_df$MCRNUM

panel_near <- hosp_2012 %>%
  filter(MCRNUM %in% matched_df$MCRNUM) %>%
  left_join(
    matched_df %>% 
      select(MCRNUM, weights) %>% 
      distinct(MCRNUM, .keep_all = TRUE),
    by = "MCRNUM"
  ) %>%
  mutate(event_time = YEAR - 2012)

pscores <- match_near$distance
summary(pscores)

# event study on matched data
#FTEMND
esmatch_ftemd <- feols(
  FTEMD ~ i(event_time, treatment, ref = -1) | MCRNUM + YEAR,
  data = panel_near,
  cluster = ~MCRNUM,
)

summary(esmatch_ftemd)


png("FTEMD_Event_Study_Matched.png", width = 800, height = 800)
iplot(esmatch_ftemd,
      xlab = "Event Time",
      main = "Event Study: FTEMD and 2012 Penalties (matched nearest)")
dev.off()

did <- feols(
  FTEMD ~ did + post + treatment | MCRNUM + YEAR, 
  data = panel_near) 
summary(did)

# FTERN 
esmatch_ftern <- feols(
  FTERN ~ i(event_time, treatment, ref = -1) | MCRNUM + YEAR,
  data = panel_near,
  cluster = ~MCRNUM,
)
summary(esmatch_ftern)

png("FTERN_Event_Study_Matched.png", width = 800, height = 800)
iplot(esmatch_ftern,
      xlab = "Event Time",
      main = "Event Study: FTERN and 2012 Penalties (matched nearest)")
dev.off()

did <- feols(
  FTERN ~ did + post + treatment | MCRNUM + YEAR, 
  data = panel_near) 
summary(did)

# FTRNTF
esmatch_ftrntf <- feols(
  FTRNTF ~ i(event_time, treatment, ref = -1) | MCRNUM + YEAR,
  data = panel_near,
  cluster = ~MCRNUM,
)
summary(esmatch_ftrntf)
png("FTRNTF_Event_Study_Matched.png", width = 800, height = 800)
iplot(esmatch_ftrntf,
      xlab = "Event Time",
      main = "Event Study: FTRNTF and 2012 Penalties (matched nearest)")
dev.off()

# FTELPN 
esmatch_ftelpn <- feols(
  FTELPN ~ i(event_time, treatment, ref = -1) | MCRNUM + YEAR,
  data = panel_near,
  cluster = ~MCRNUM,
)
summary(esmatch_ftelpn)

png("FTELPN_Event_Study_Matched.png", width = 800, height = 800)
iplot(esmatch_ftelpn,
      xlab = "Event Time",
      main = "Event Study: FTELPN and 2012 Penalties (matched nearest)")
dev.off()

did <- feols(
  FTELPN ~ did + post + treatment | MCRNUM + YEAR, 
  data = panel_near) 
summary(did)

# FTERES
esmatch_fteres <- feols(
  FTERES ~ i(event_time, treatment, ref = -1) | MCRNUM + YEAR,
  data = panel_near,
  cluster = ~MCRNUM,
)
summary(esmatch_fteres)
png("FTERES_Event_Study_Matched.png", width = 800, height = 800)
iplot(esmatch_fteres,
      xlab = "Event Time",
      main = "Event Study: FTERES and 2012 Penalties (matched nearest)")
dev.off()

# 1. Hospital counts by treatment group
cat("\n=== HOSPITAL COUNTS ===\n")
hospital_counts <- matched_df %>%
  group_by(treatment) %>%
  summarise(n_hospitals = n(), .groups = "drop") %>%
  mutate(group = ifelse(treatment == 1, "Treatment", "Control"))

print(hospital_counts)

# 2. Summary statistics for key variables
cat("\n=== SUMMARY STATISTICS FOR MATCHED SAMPLE ===\n")

# Select key variables for summary
key_vars <- c("bed_avg", "BDTOT", "ADMTOT", "FTEMD", "FTERN", "FTELPN", "FTERES", "tot_operating_exp", "net_pat_rev")

# Calculate means and standard deviations by treatment group
summary_stats <- matched_df %>%
  group_by(treatment) %>%
  summarise(
    across(all_of(key_vars),
           list(mean = ~mean(.x, na.rm = TRUE),
                sd = ~sd(.x, na.rm = TRUE)),
           .names = "{.col}_{.fn}"),
    .groups = "drop"
  ) %>%
  mutate(group = ifelse(treatment == 1, "Treatment", "Control"))

print(summary_stats)

# 3. Create a readable table for each variable
cat("\n=== VARIABLE-BY-VARIABLE COMPARISON ===\n")
for(var in key_vars) {
  control_mean <- summary_stats %>% filter(treatment == 0) %>% pull(paste0(var, "_mean"))
  control_sd <- summary_stats %>% filter(treatment == 0) %>% pull(paste0(var, "_sd"))
  treat_mean <- summary_stats %>% filter(treatment == 1) %>% pull(paste0(var, "_mean"))
  treat_sd <- summary_stats %>% filter(treatment == 1) %>% pull(paste0(var, "_sd"))
  
  cat(sprintf("%s:\n", var))
  cat(sprintf("  Control:   Mean = %.2f, SD = %.2f\n", control_mean, control_sd))
  cat(sprintf("  Treatment: Mean = %.2f, SD = %.2f\n", treat_mean, treat_sd))
  cat("\n")
}

# 4. Check balance quality (standardized differences)
cat("=== BALANCE CHECK (Standardized Differences) ===\n")
balance_check <- data.frame()

for(var in key_vars) {
  control_vals <- matched_df %>% filter(treatment == 0) %>% pull(!!sym(var))
  treat_vals <- matched_df %>% filter(treatment == 1) %>% pull(!!sym(var))
  
  mean_diff <- mean(treat_vals, na.rm = TRUE) - mean(control_vals, na.rm = TRUE)
  pooled_sd <- sqrt((var(treat_vals, na.rm = TRUE) + var(control_vals, na.rm = TRUE)) / 2)
  std_diff <- mean_diff / pooled_sd
  
  balance_check <- rbind(balance_check, 
                        data.frame(Variable = var, 
                                 Std_Diff = round(std_diff, 3)))
}

print(balance_check)
cat("Note: Standardized differences < 0.1 indicate good balance\n\n")

# 5. Save summary to CSV
write.csv(summary_stats, "matched_summary_stats.csv", row.names = FALSE)
write.csv(balance_check, "matched_balance_check.csv", row.names = FALSE)
cat("Summary files saved: matched_summary_stats.csv and matched_balance_check.csv\n\n")


