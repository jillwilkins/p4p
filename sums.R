library(dplyr)
library(knitr)
library(tidyr)

#Counts by group
obs_main <- hosp_2012 %>%
  filter(treatment %in% c(0, 1), BDTOT >= 300 & BDTOT <= 1500) %>%
  group_by(treatment) %>%
  summarise(
    total_obs = n(),
    unique_hospitals = n_distinct(MCRNUM),
    .groups = "drop"
  )


print(obs_main)
#-----------------------------------------------------------#
# Define variables to summarize
sum_vars <- c("BDTOT", "FTERN", "FTELPN", "FTEMD", "FTERES","ADMTOT", "tot_operating_exp", "net_pat_rev")

# Step 2: Compute mean & sd, reshape and clean
summary_long <- hosp_2012 %>% filter(BDTOT >= 30 & BDTOT <= 1000) %>%
  group_by(treatment) %>%
  summarise(across(all_of(sum_vars),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd = ~sd(.x, na.rm = TRUE)),
                   .names = "{.col}_{.fn}"),
            .groups = "drop") %>%
  pivot_longer(-treatment, names_to = c("variable", "stat"), names_pattern = "(.+)_([^_]+)") %>%
  pivot_wider(names_from = stat, values_from = value, values_fn = mean) %>%  # resolve duplicates
  mutate(
    mean = as.numeric(mean),
    sd = as.numeric(sd),
    stat_string = sprintf("%.2f (%.2f)", mean, sd)
  ) %>%
  select(treatment, variable, stat_string) %>%
  pivot_wider(names_from = treatment,
              values_from = stat_string,
              names_prefix = "treatment_")

# Step 3: Rename columns
colnames(summary_long) <- c("Variable", "Control", "Treatment")

# Step 4: Make variable names nicer
summary_long$Variable <- recode(summary_long$Variable,
                                BDTOT = "Beds",
                                FTERN = "FTE RNs",
                                FTELPN = "FTE LPNs",
                                FTEMD = "FTE MDs",
                                "FTERES" = "FTE Residents",
                                ADMTOT = "Total Admissions",
                                tot_operating_exp = "Operating Expenses",
                                tot_pat_rev = "Patient Revenue")

# Step 5: Print LaTeX-ready table
kable(summary_long, format = "latex", booktabs = TRUE,
      caption = "Summary Statistics by Treatment Status",
      align = "lcc")

# -----------------------------------------------------------#
# TWFE 
# Extract coefficient and standard error for a key variable (say, x1)
extract_est <- function(model, var) {
  est <- coef(model)[[var]]
  se <- sqrt(diag(vcov(model)))[[var]]
  pval <- summary(model)$coeftable[var, "Pr(>|t|)"]

stars <- if (pval < 0.001) {"***"} else if (pval < 0.01) {"**"} else if (pval < 0.05) {"*"} else if (pval < 0.1) {"."} else {" "}
sprintf("%.3f%s\n(%.3f)", est, stars, se)
}

# Build table
vars_of_interest <- c("did")  # variables you care about

reg_table <- tibble(
  Variable = vars_of_interest,
  `Model 1` = sapply(vars_of_interest, function(v) extract_est(did_ftern_full, v)),
  `Model 2` = sapply(vars_of_interest, function(v) extract_est(did_ftern_mid, v)), 
  `Model 3` = sapply(vars_of_interest, function(v) extract_est(did_ftern_2, v)),
  `Model 4` = sapply(vars_of_interest, function(v) extract_est(did_ftern, v))
)

# Output to LaTeX
kable(reg_table, format = "latex", booktabs = TRUE, align = "lcc",
      caption = "TWFE Estimates", escape = FALSE)

# -----------------------------------------------------------#
# Means and SD for FTERN per bed 
ftern_bed_sum <- hosp_filter %>% 
  group_by(treatment) %>%
  summarise(
    mean_ftern_bed = mean(ftern_beds, na.rm = TRUE),
    sd_ftern_bed = sd(ftern_beds, na.rm = TRUE),
    .groups = "drop"    
  )
ftern_bed_sum 
# -----------------------------------------------------------#
# plots 
# FTERN by year and treatment group
hosp_2012 <- hosp_2012 %>%
  mutate(ftern_beds = FTERN / BDTOT) 

ftern_trend <- hosp_2012 %>%
  group_by(YEAR, treatment) %>%
  summarise(mean_FTERN = mean(ftern_beds, na.rm = TRUE), .groups = "drop")

# Plot
library(ggplot2)
trend_ftern <- ggplot(ftern_trend, aes(x = YEAR, y = mean_FTERN, color = factor(treatment))) +
  geom_line(size = 1.3) +
  geom_point(size = 3) +
  labs(
    x = "Year",
    y = "Mean FTE RNs per Bed",
    color = "Treatment Group",
    title = "Mean FTE RNs per Bed Over Time by Treatment Group (beds >= 30)",
  ) +
  scale_color_manual(
    values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
    labels = c("Control", "Treated")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave("plots/trend_ftern.png", plot = trend_ftern)

#----------------------------------------------------------------------#
# BDTOT over time and group 
bed_trend <- hosp_2012 %>%
  filter(BDTOT >= 30 & BDTOT <= 2000) %>%
  group_by(YEAR, treatment) %>%
  summarise(mean_bed = mean(BDTOT, na.rm = TRUE), .groups = "drop")

# Plot 
library(ggplot2)
trend_bed <- ggplot(bed_trend, aes(x = YEAR, y = mean_bed, color = factor(treatment))) +
  geom_line(size = 1.3) +
  geom_point(size = 3) +
  labs(
    x = "Year",
    y = "Mean Beds",
    color = "Treatment Group",
    title = "Mean Beds Over Time by Treatment Group (beds 30 - 2000)",
  ) +
  scale_color_manual(
    values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
    labels = c("Control", "Treated")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )
trend_bed
ggsave("plots/trend_bed.png", plot = trend_bed)

# ----------------------------------------------------------------------#
# demeaned trends 


# Demean FTERN within each group
trend_data <- hosp_2012 %>% filter(BDTOT >= 30 & BDTOT <= 1000) %>%
  group_by(treatment, YEAR) %>%
  summarise(mean_ftern = mean(FTERN, na.rm = TRUE), .groups = "drop") %>%
  group_by(treatment) %>%
  mutate(demeaned_ftern = mean_ftern - first(mean_ftern))  # start both at 0

# Plot
ggplot(trend_data, aes(x = YEAR, y = demeaned_ftern, color = factor(treatment))) +
  geom_line(size = 1.2) +
  geom_point() +
  scale_color_manual(values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
                     name = "Group", labels = c("Control", "Treatment")) +
  theme_minimal() +
  labs(title = "Change in FTERN Over Time (Demeaned)",
       x = "Year", y = "Demeaned Average FTERN")

