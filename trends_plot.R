# quad plot bed group 
hosp_data_grouped <- hosp_2012 %>%
  mutate(bed_group = case_when(
    TRUE ~ "Full Sample",
    BDTOT >= 30 ~ "BDTOT >= 30",
    BDTOT >= 30 & BDTOT <= 2000 ~ "30 <= BDTOT <= 2000",
    BDTOT >= 30 & BDTOT <= 1000 ~ "30 <= BDTOT <= 1000"
  ))

# Create subsets
bed_all <- hosp_2012 %>%
  mutate(bed_group = "Full Sample")

bed_30 <- hosp_2012 %>%
  filter(BDTOT >= 30) %>%
  mutate(bed_group = "BDTOT >= 30")

bed_2000 <- hosp_2012 %>%
  filter(BDTOT >= 30, BDTOT <= 2000) %>%
  mutate(bed_group = "30 <= BDTOT <= 2000")

bed_1000 <- hosp_2012 %>%
  filter(BDTOT >= 30, BDTOT <= 1000) %>%
  mutate(bed_group = "30 <= BDTOT <= 1000")

# Combine
hosp_data_combined <- bind_rows(bed_all, bed_30, bed_2000, bed_1000)

# demean 
trend_ftern <- hosp_data_combined %>%
  group_by(bed_group, treatment, YEAR) %>%
  summarise(mean_ftern = mean(FTERN, na.rm = TRUE), .groups = "drop") %>%
  group_by(bed_group, treatment) %>%
  mutate(demeaned_ftern = mean_ftern - first(mean_ftern))

# plot for FTERN
quad_ftern <- ggplot(trend_ftern, aes(x = YEAR, y = demeaned_ftern, color = factor(treatment))) +
  geom_line(size = 1) +
  geom_point() +
  scale_color_manual(values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
                     name = "Group", labels = c("Control", "Treatment")) +
  facet_wrap(~ bed_group, ncol = 2) +
  theme_minimal() +
  labs(
    title = "Demeaned FTERN Trends Over Time by Bed Group",
    x = "Year", y = "Demeaned FTERN"
  )

ggsave("plots/quad_ftern.png", plot = quad_ftern, width = 10, height = 10, bg = "white")

# plot for FTEMD
trend_ftemd <- hosp_data_combined %>% filter(FTEMD > 0) %>%
  group_by(bed_group, treatment, YEAR) %>%
  summarise(mean_ftemd = mean(FTEMD, na.rm = TRUE), .groups = "drop") %>%
  group_by(bed_group, treatment) %>%
  mutate(demeaned_ftemd = mean_ftemd - first(mean_ftemd))

quad_ftemd <- ggplot(trend_ftemd, aes(x = YEAR, y = demeaned_ftemd, color = factor(treatment))) +
  geom_line(size = 1) +
  geom_point() +
  scale_color_manual(values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
                     name = "Group", labels = c("Control", "Treatment")) +
  facet_wrap(~ bed_group, ncol = 2) +
  theme_minimal() +
  labs(
    title = "Demeaned FTEMD Trends Over Time by Bed Group",
    x = "Year", y = "Demeaned FTEMD"
  )
  
print(quad_ftemd)
ggsave("plots/quad_ftemd.png", plot = quad_ftemd, width = 10, height = 10, bg = "white")




# PER BED 
# get averages 
plot_data <- hosp_2012 %>% filter (BDTOT >= 30 & BDTOT <= 2000 & FTEMD > 0) %>%
  group_by(treatment, YEAR) %>%
  summarise(avg_rn_bed = mean(FTERN / BDTOT, na.rm = TRUE), 
            avg_md_bed = mean(FTEMD / BDTOT, na.rm = TRUE),
            avg_lpn_bed = mean(FTELPN / BDTOT, na.rm = TRUE),
            avg_res_bed = mean(FTERES / BDTOT, na.rm = TRUE),
            .groups = "drop")

# ftern per bed
trend_ftern_bed <- ggplot(plot_data, aes(x = YEAR, y = avg_rn_bed, color = factor(treatment))) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_minimal(base_size = 16) +  # increase overall font size
  labs(
    title = "FTERN per Bed by Group",
    y = "FTERN per Bed",
    x = "Year",
    color = "Group"
  ) +
  scale_color_manual(
    values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
    labels = c("Control", "Treatment")
  ) 

ggsave("plots/trend_ftern_bed.png", plot = trend_ftern_bed, width = 8, height = 8, bg = "white")

# ftemd per bed
trend_ftemd_bed <- ggplot(plot_data, aes(x = YEAR, y = avg_md_bed, color = factor(treatment))) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_minimal(base_size = 16) +  # increase overall font size
  labs(
    title = "FTEMD per Bed by Group",
    y = "FTEMD per Bed",
    x = "Year",
    color = "Group"
  ) +
  scale_color_manual(
    values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
    labels = c("Control", "Treatment")
  ) 

ggsave("plots/trend_ftemd_bed.png", plot = trend_ftemd_bed, width = 8, height = 8, bg = "white")

