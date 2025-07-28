library(fixest)

# from hosp_data, assign treatment and control group  
# treatment group when first penalized == 2012

hosp_2012 <- hosp_data %>%
  mutate(treatment = ifelse(first_penalty == "2012", 1, 0)) 

# control group when hospitals are either never penalized or first penalized after 2016 
hosp_2012 <- hosp_2012 %>%
  mutate(control = ifelse(first_penalty == "none" | first_penalty > 2016, 1, 0))

# filter out hospitals that are not in treatment or control group
hosp_2012 <- hosp_2012 %>% filter(treatment == 1 | control == 1)

# filter relevant years
hosp_2012 <- hosp_2012 %>% filter(YEAR <= 2016 & YEAR >= 2008) 

# Create a treatment-time interaction variable for DiD
hosp_2012 <- hosp_2012 %>%
  mutate(post = ifelse(YEAR >= 2012, 1, 0),  
         did = treatment * post)

#summary statistics for treatment and control groups
sum_vars <- c("BDTOT", "FTERN", "FTELPN", "ADMTOT", "tot_operating_exp", "net_pat_rev")

# mean and standard deviation for each variable by treatment group
summary_stats <- hosp_2012 %>%
  group_by(treatment) %>%
  summarise(across(all_of(sum_vars),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd = ~sd(.x, na.rm = TRUE)),
                   .names = "{.col}_{.fn}"))

print(summary_stats)

#-----------------------------------------------------------#
# i will create a balanced panel for analysis as well 
# BALANCING 
# Identify hospitals that ever meet the inclusion criteria
#included_hospitals <- hosp_2012 %>%
#  filter(BDTOT >= 30 & BDTOT <= 2000 & ADMTOT >= 25 & FTEMD > 0) %>%
#  pull(MCRNUM) %>%
#  unique()
# Keep all rows for those hospitals
#hosp_filter <- hosp_2012 %>%
#  filter(MCRNUM %in% included_hospitals)

# bed_2000 is defined in balanced.R 
hosp_filter <- bed_2000 

# FTERN 
# check for parallel trends
hosp_2012 <- hosp_2012 %>%
  mutate(event_time = YEAR - 2012)

hosp_filter <- hosp_filter %>%
  mutate(event_time = YEAR - 2012)

event_tccar <- feols(
  TCCAR ~ i(event_time, treatment, ref = -1) | MCRNUM + YEAR,
  data = hosp_filter,  #%>% filter(BDTOT >= 30),
  cluster = ~MCRNUM
)
summary(event_tccar)
iplot(event_tccar,
      xlab = "Event Time",
      main = "Event Study: tccar and 2012 Penalties (Beds 30-2000)"
)

# FTERN per bed
hosp_filter <- hosp_filter %>%
  mutate(FTERN_per_bed = FTERN / BDTOT, 
         FTEMD_per_bed = FTEMD / BDTOT,
         FTELPN_per_bed = FTELPN / BDTOT,
         FTERES_per_bed = FTERES / BDTOT)

event_ftern_per_bed <- feols(
  FTERN_per_bed ~ i(event_time, treatment, ref = -1) | MCRNUM + YEAR,
  data = hosp_filter,
  cluster = ~MCRNUM
)
summary(event_ftern_per_bed)
iplot(event_ftern_per_bed,
      xlab = "Event Time",
      main = "Event Study: FTERN per Bed and 2012 Penalties (beds 30-2000)"
)

did_ftern_per_bed <- feols(
  FTERN_per_bed ~ did + post + treatment | MCRNUM + YEAR, 
  data = hosp_filter
)
summary(did_ftern_per_bed)

did_ftern_full <- feols(
  FTERN ~ did + post + treatment | MCRNUM + YEAR, 
  data = hosp_2012
)
did_ftern_mid <- feols(
  FTERN ~ did + post + treatment | MCRNUM + YEAR, 
  data = hosp_2012 %>% filter(BDTOT >= 30)
)
summary(did_ftern_mid)

did_ftern_2 <- feols(
  FTERN ~ did + post + treatment | MCRNUM + YEAR, 
  data = hosp_2012 %>% filter(BDTOT >= 30 & BDTOT <= 2000)
)
summary(did_ftern_2)
did_ftern <- feols(
  FTERN ~ did + post + treatment | MCRNUM + YEAR, 
  data = hosp_2012 %>% filter(BDTOT >= 30 & BDTOT <= 1000)
)
summary(did_ftern)


# BEDS
event_beds <- feols(
  BDTOT ~ i(event_time, treatment, ref = -1) | MCRNUM + YEAR,
  data = hosp_2012  %>% filter(BDTOT >= 30 & BDTOT <= 2000),
  cluster = ~MCRNUM
)
summary(event_beds)
iplot(event_beds,
      xlab = "Event Time",
      main = "Event Study: Total Beds and 2012 Penalties (beds 30-2000)"
)

did_beds <- feols(
  BDTOT ~ did + post + treatment | MCRNUM + YEAR, 
  data = hosp_2012
)
summary(did_beds)

# ADMISSIONS (ADMTOT)
event_admtot <- feols(
  ADMTOT ~ i(event_time, treatment, ref = -1) | MCRNUM + YEAR,
  data = hosp_2012  %>% filter(BDTOT >= 30 & BDTOT <= 2000),
  cluster = ~MCRNUM
)
summary(event_admtot)
iplot(event_admtot,
      xlab = "Event Time",
      main = "Event Study: Total Admissions and 2012 Penalties (beds 30-2000)"
)

did_admtot <- feols(
  ADMTOT ~ did + post + treatment | MCRNUM + YEAR, 
  data = hosp_2012 %>% filter(BDTOT >= 30 & BDTOT <= 2000)
)
summary(did_admtot)
# PAYTOT 
event_paytot <- feols(
  PAYTOT ~ i(event_time, treatment, ref = -1) | MCRNUM + YEAR + BDTOT,
  data = hosp_2012  %>% filter(BDTOT >= 30 & BDTOT <= 2000),
  cluster = ~MCRNUM
)
summary(event_paytot)

iplot(event_paytot,
      xlab = "Event Time",
      main = "Event Study: Total Patient Revenue and 2012 Penalties (beds 30-2000)"
)

did_paytot <- feols(
  PAYTOT ~ did + post + treatment | MCRNUM + YEAR + BDTOT, 
  data = hosp_2012 %>% filter(BDTOT >= 30 & BDTOT <= 2000)
)
summary(did_paytot)

# PHYSICIAN (FTEMD)
event_ftemd_full <- feols(
  FTEMD ~ i(event_time, treatment, ref = -1) | MCRNUM + YEAR,
  data = hosp_2012  %>% filter(BDTOT >= 30 & BDTOT <= 2000 & ADMTOT >= 25 & FTEMD > 0),
  cluster = ~MCRNUM
)
summary(event_ftemd_full)
iplot(event_ftemd_full,
      xlab = "Event Time",
      main = "Event Study: Full Time Equivalent MDs and 2012 Penalties (large)"
)
# Run model balanced panel
event_ftemd <- feols(
  FTEMD ~ i(event_time, treatment, ref = -1) | MCRNUM + YEAR,
  data = bed_2000,
  cluster = ~MCRNUM
)
summary(event_ftemd)
iplot(event_ftemd,
      xlab = "Event Time",
      main = "Event Study: Full Time Equivalent MDs and 2012 Penalties (beds 30-2000)"
)
did_ftemd <- feols(
  FTEMD ~ did + post + treatment | MCRNUM + YEAR, 
  data = hosp_filter)
summary(did_ftemd)

# run on 30 - 1000 beds
event_ftemd_1 <- feols(
  FTEMD ~ i(event_time, treatment, ref = -1) | MCRNUM + YEAR,
  data = bed_1000,
  cluster = ~MCRNUM
)
summary(event_ftemd_1)

# RESIDENTS/INTERNS (FTERES)
event_fteres <- feols(
  FTERES ~ i(event_time, treatment, ref = -1) | MCRNUM + YEAR,
  data = hosp_filter  %>% filter(BDTOT >= 30 & BDTOT <= 2000),
  cluster = ~MCRNUM
)
summary(event_fteres)
iplot(event_fteres,
      xlab = "Event Time",
      main = "Event Study: Full Time Equivalent Residents/Interns and 2012 Penalties (beds 30-2000)"
)

did_fteres <- feols(
  FTERES ~ did + post + treatment | MCRNUM + YEAR, 
  data = hosp_filter
)
summary(did_fteres)

event_fteres_1 <- feols(
  FTERES ~ i(event_time, treatment, ref = -1) | MCRNUM + YEAR,
  data = bed_1000,
  cluster = ~MCRNUM
)
summary(event_fteres_1)

# FTELPN 
# check for parallel trends
event_ftelpn <- feols(
  FTELPN ~ i(event_time, treatment, ref = -1) | MCRNUM + YEAR,
  data = hosp_filter, #%>% filter(BDTOT >= 30 & BDTOT <= 2000),
  cluster = ~MCRNUM
)
summary(event_ftelpn)

iplot(event_ftelpn,
      xlab = "Event Time",
      main = "Event Study: Full Time Equivalent LPNs and 2012 Penalties (beds 30-2000)")

did_ftelpn <- feols(
  FTELPN ~ did + post + treatment | MCRNUM + YEAR, 
  data = hosp_filter # %>% filter(BDTOT >= 30 & BDTOT <= 2000)
)
summary(did_ftelpn)

# TREATMENT (how muuch does x variable predict treatment)
did_treatment <- feols(
  treatment ~ tot_operating_exp | MCRNUM + YEAR, 
  data = hosp_2012 %>% filter(BDTOT >= 30 & BDTOT <= 2000)
)
summary(did_treatment)

# Tables for LaTeX
# Step 1: Extract coefficients and standard errors
es_table_ftern_bed <- broom::tidy(event_ftern_per_bed, conf.int = FALSE) %>%
  filter(grepl("event_time::", term)) %>%
  mutate(
    term = gsub("event_time::", "", term),
    estimate_se = sprintf("%.2f (%.2f)", estimate, std.error)
  ) %>%
  select(term, estimate_se) %>%
  rename(`Event Time` = term, `Estimate (SE)` = estimate_se)

# Step 2: Create LaTeX table
kable(es_table_ftern_bed, format = "latex", booktabs = TRUE,
      caption = "Event Study Estimates: FTERN per Bed",
      align = "lc")
