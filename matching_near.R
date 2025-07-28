# general for all matching
# find estimates by matching 
library(dplyr)
install.packages("MatchIt")
library(MatchIt)

# Calculate the 99th percentile cutoff for beds (levels and pct change)
bed_cutoff <- quantile(hosp_2012$BDTOT, 0.99, na.rm = TRUE)
bed_90 <- quantile(hosp_2012$BDTOT, 0.90, na.rm = TRUE)
cat("90th percentile info for BDTOT:", bed_90, "\n")
bed_pct_cutoff <- quantile(hosp_2012$bed_pct_change_11_16, 0.99, na.rm = TRUE)
bed_pct_90 <- quantile(hosp_2012$bed_pct_change_11_16, 0.90, na.rm = TRUE)
cat("90th percentile info for bed percentage change:", bed_pct_90, "%\n")

# average BDTOT and ADMTOT per hospital years 2008 - 2011 
hosp_2012 <- hosp_2012 %>%
  group_by(MCRNUM) %>%
  mutate(bed_preavg = mean(BDTOT[YEAR >= 2008 & YEAR <= 2011], na.rm = TRUE)) %>%
  ungroup()

hosp_2012 <- hosp_2012 %>%
  group_by(MCRNUM) %>%
  mutate(adm_preavg = mean(ADMTOT[YEAR >= 2008 & YEAR <= 2011], na.rm = TRUE)) %>%
  ungroup()

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
  filter(MCRNUM %in% valid_hospitals)

cat("Observations after filtering:", nrow(hosp_2012), "\n\n")
cat("Number of hospitals after all filters:", length(unique(hosp_2012$MCRNUM)), "\n")

summary(hosp_2012$bed_preavg)

# match using nearest neighbor with caliper
match_near <- matchit(
  treatment ~ bed_preavg, #+ adm_preavg,
  data = hosp_2012 %>% filter(!is.na(bed_preavg)), #& !is.na(adm_preavg)),
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

# number of unique hospitals in each treatment group
unique_hospitals <- panel_near %>%
  group_by(treatment) %>%
  summarise(n_hospitals = n_distinct(MCRNUM), .groups = "drop")
print(unique_hospitals)

# average FTEMD and FTERN per group in the matched sample
average_outcomes <- panel_positive_ftemd %>%
  group_by(treatment) %>%
  summarise(
    avg_FTEMD = mean(FTEMD, na.rm = TRUE),
    avg_FTERN = mean(FTERN, na.rm = TRUE),
    avg_FTELPN = mean(FTELPN, na.rm = TRUE),
    avg_FTERES = mean(FTERES, na.rm = TRUE),
    .groups = "drop"
  )
  print(average_outcomes)

# manually place incorrect zeros that happen not in 2012 
panel_near_fixed <- panel_near %>%
  mutate(FTERN = if_else(MCRNUM == 310016 & YEAR == 2008, 303, FTERN),
         FTERN = if_else(MCRNUM == 100173 & YEAR == 2010, 559, FTERN),
         FTERN = if_else(MCRNUM == 260070 & YEAR == 2009, 70, FTERN),
        FTERN = if_else(MCRNUM == 050511 & YEAR == 2010, 264.5, FTERN))

# questionable entries 
panel_near_fixed <- panel_near_fixed %>%
  filter(MCRNUM != 050228 & MCRNUM != 400013)
  
# investigate hospitals that have a FTEMD of 0 at any time
zero_ftemd_hosp <- panel_near %>%
  filter(FTEMD == 0 & FTMDTF == 0 & PTMDTF == 0) %>%
  select(MCRNUM, YEAR, treatment, FTEMD, FTMDTF, PTMDTF, BDTOT) %>%
  distinct()
View(zero_ftemd_hosp)
n_distinct(zero_ftemd_hosp$MCRNUM)

panel_positive_ftemd <- panel_near %>%
  group_by(MCRNUM) %>%
  filter(all(FTEMD > 0)) %>%
  ungroup()


# event study on matched data
#FTEMD
esmatch_ftemd <- feols(
  FTEMD ~ i(event_time, treatment, ref = -1) | MCRNUM + YEAR,
  data = panel_positive_ftemd,
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
  data = panel_near_fixed ,
  cluster = ~MCRNUM,
)
summary(esmatch_ftern)

png("q_FTERN_Event_Study_Matched.png", width = 800, height = 800)
iplot(esmatch_ftern,
      xlab = "Event Time",
      main = "Event Study: ?? FTERN and 2012 Penalties (matched nearest)")
dev.off()

did <- feols(
  FTERN ~ did + post + treatment | MCRNUM + YEAR, 
  data = panel_near_fixed) 
summary(did)

# FTRNTF
esmatch_ftrntf <- feols(
  FTRNTF ~ i(event_time, treatment, ref = -1) | MCRNUM + YEAR,
  data = panel_near_fixed,
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

# RNSCH 
esmatch_rnsch <- feols(
  RNSCH ~ i(event_time, treatment, ref = -1) | MCRNUM + YEAR,
  data = panel_near,
  cluster = ~MCRNUM,
)
summary(esmatch_rnsch)

# FTEPHRN 
esmatch_ftephrn <- feols(
  FTEPHRN ~ i(event_time, treatment, ref = -1) | MCRNUM + YEAR,
  data = panel_near,
  cluster = ~MCRNUM,
)
summary(esmatch_ftephrn)

# TPCAR 
esmatch_tpcar <- feols(
  TPCAR ~ i(event_time, treatment, ref = -1) | MCRNUM + YEAR,
  data = panel_near,
  cluster = ~MCRNUM,
)
summary(esmatch_tpcar)
png("TPCAR_Event_Study_Matched.png", width = 800, height = 800)
iplot(esmatch_tpcar,
      xlab = "Event Time",
      main = "Event Study: TPCAR and 2012 Penalties (matched nearest)")
dev.off()

# Investigate 2008 
# summary stats matched data for 2008 
sum_2008 <- panel_near %>%
  filter(YEAR == 2008) %>%
  group_by(treatment) %>%
  summarise(
    n_hospitals = n_distinct(MCRNUM),
    avg_bed_preavg = mean(bed_preavg, na.rm = TRUE),
    avg_BDTOT = mean(BDTOT, na.rm = TRUE),
    avg_ADMTOT = mean(ADMTOT, na.rm = TRUE),
    avg_FTEMD = mean(FTEMD, na.rm = TRUE),
    avg_FTERN = mean(FTERN, na.rm = TRUE),
    min_FTERN = min(FTERN, na.rm = TRUE),
    max_FTERN = max(FTERN, na.rm = TRUE),
    avg_FTELPN = mean(FTELPN, na.rm = TRUE),
    avg_FTERES = mean(FTERES, na.rm = TRUE),
    .groups = "drop"
  )
View(sum_2008)
View(panel_near %>% filter(YEAR == 2008) %>% select(MCRNUM, treatment, bed_preavg, BDTOT, ADMTOT, FTEMD, FTERN, FTELPN, FTERES))

# investigate hospitals that have a FTERN of 0 at any time 
zero_ftern_hospitals <- panel_near %>%
  filter(FTERN == 0) %>%
  select(MCRNUM, YEAR, treatment, FTERN) %>%
  distinct()
View(zero_ftern_hospitals)
n_distinct(zero_ftern_hospitals$MCRNUM)

zero_ftemd_hosp <- panel_near %>%
  filter(FTEMD == 0 & FTMDTF == 0 & PTMDTF == 0) %>%
  select(MCRNUM, YEAR, treatment, FTEMD, FTMDTF, PTMDTF, BDTOT) %>%
  distinct()
View(zero_ftemd_hosp)
n_distinct(zero_ftemd_hosp$MCRNUM)

View(panel_near %>% filter(MCRNUM %in% zero_ftern_hospitals$MCRNUM) %>% select(MCRNUM, YEAR, treatment, FTERN, FTEMD, FTELPN, FTERES))

# manually place incorrect zeros that happen not in 2012 
panel_near_fixed <- panel_near %>%
  mutate(FTERN = if_else(MCRNUM == 310016 & YEAR == 2008, 303, FTERN),
         FTERN = if_else(MCRNUM == 100173 & YEAR == 2010, 559, FTERN),
         FTERN = if_else(MCRNUM == 260070 & YEAR == 2009, 70, FTERN),
        FTERN = if_else(MCRNUM == 050511 & YEAR == 2010, 264.5, FTERN))

# questionable entries 
panel_near_fixed <- panel_near_fixed %>%
  filter(MCRNUM != 050228 & MCRNUM != 400013)
  






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


