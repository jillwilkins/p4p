library(readr)
library(dplyr)
library(tidyr)

# read aha and hcris raw data --------------------------------------------------------------------
aha <- read_csv("/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/input/AHAdata_20052023.csv")
hcris <- read.delim("/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/output/HCRIS_data.txt", stringsAsFactors = FALSE)

# select relevant variable from aha 

#change prover_number to be a character 
hcris <- hcris %>%
  mutate(provider_number = as.character(provider_number))

#merge with hcris penalty data
hosp_full <- aha %>%
  left_join(hcris %>% select(provider_number, year, beds, hrrp_payment, hvbp_payment, tot_operating_exp, net_pat_rev), 
            by = c("YEAR" = "year", "MCRNUM" = "provider_number"))

# create year a hospital was first penalized hrrp 
hosp_full <- hosp_full %>%
    group_by(MCRNUM) %>%
    mutate(first_penalty = case_when(
      all(is.na(hrrp_payment)) ~ "none",  # case where all values are NA
      any(hrrp_payment > 0, na.rm = TRUE) ~ as.character(
        ifelse(sum(hrrp_payment > 0, na.rm = TRUE) > 0, 
               min(YEAR[hrrp_payment > 0], na.rm = TRUE), 
               NA)
      ),
      TRUE ~ "none"  # if no penalty ever occurred, set "none"
    )) %>%
    ungroup()

#see what years hospitals were first penalized
hosp_full %>%
  group_by(first_penalty) %>%
  summarise(num_obs = n()) %>%
  arrange(desc(num_obs))  

hosp_full %>%
  group_by(first_penalty) %>%
  summarise(num_hospitals = n_distinct(MCRNUM)) %>%
  arrange(desc(num_hospitals))

# take out hospital types not relevant for HRRP 
# only keep SERV = 10 
hosp_data <- hosp_full %>% filter(SERV == 10)

# investigate sample 
hosp_data %>%
  summarise(
    min_ftern = min(FTERN, na.rm = TRUE),
    q25_ftern = quantile(FTERN, 0.25, na.rm = TRUE),
    median_ftern = median(FTERN, na.rm = TRUE),
    mean_ftern = mean(FTERN, na.rm = TRUE),
    q75_ftern = quantile(FTERN, 0.75, na.rm = TRUE),
    max_ftern = max(FTERN, na.rm = TRUE)
  )

