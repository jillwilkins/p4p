# demean 
trend_ftern <- hosp_data_combined %>%
  group_by(bed_group, treatment, YEAR) %>%
  summarise(mean_ftern = mean(FTERN, na.rm = TRUE), .groups = "drop") %>%
  group_by(bed_group, treatment) %>%
  mutate(demeaned_ftern = mean_ftern - first(mean_ftern))

# PER BED 
# get averages 
plot_data <- bed_2000 %>% #filter (BDTOT >= 30 & BDTOT <= 2000 & FTEMD > 0) %>%
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

print(trend_ftern_bed)
ggsave("plots/trend_ftern_bed.png", plot = trend_ftern_bed, width = 8, height = 8, bg = "white")

# ftern by group
trend_ftern_group <- ggplot(bed_2000, aes(x = YEAR, y = FTERN, color = factor(treatment))) +
  geom_line(stat = "summary", fun = "mean", size = 1.2) +
  geom_point(stat = "summary", fun = "mean", size = 2) +
  theme_minimal(base_size = 16) +  # increase overall font size
  labs(
    title = "FTERN by Group",
    y = "FTERN",
    x = "Year",
    color = "Group"
  ) +
  scale_color_manual(
    values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
    labels = c("Control", "Treatment")
  )
print(trend_ftern_group)
ggsave("plots/trend_ftern_group.png", plot = trend_ftern_group, width = 8, height = 8, bg = "white")

#ftemd by group 
trend_ftemd_group <- ggplot(bed_2000, aes(x = YEAR, y = FTEMD, color = factor(treatment))) +
  geom_line(stat = "summary", fun = "mean", size = 1.2) +
  geom_point(stat = "summary", fun = "mean", size = 2) +
  theme_minimal(base_size = 16) +  # increase overall font size
  labs(
    title = "FTEMD by Group",
    y = "FTEMD",
    x = "Year",
    color = "Group"
  ) +
  scale_color_manual(
    values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
    labels = c("Control", "Treatment")
  )
print(trend_ftemd_group)
ggsave("plots/trend_ftemd_group.png", plot = trend_ftemd_group, width = 8, height = 8, bg = "white")

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
print(trend_ftemd_bed)
ggsave("plots/trend_ftemd_bed.png", plot = trend_ftemd_bed, width = 8, height = 8, bg = "white")

# ftelpn by group 
trend_ftelpn_group <- ggplot(bed_2000, aes(x = YEAR, y = FTELPN, color = factor(treatment))) +
  geom_line(stat = "summary", fun = "mean", size = 1.2) +
  geom_point(stat = "summary", fun = "mean", size = 2) +
  theme_minimal(base_size = 16) +  # increase overall font size
  labs(
    title = "FTELPN by Group",
    y = "FTELPN",
    x = "Year",
    color = "Group"
  ) +
  scale_color_manual(
    values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
    labels = c("Control", "Treatment")
  )
print(trend_ftelpn_group)
ggsave("plots/trend_ftelpn_group.png", plot = trend_ftelpn_group, width = 8, height = 8, bg = "white")

# fteres by group 
trend_fteres_group <- ggplot(bed_2000, aes(x = YEAR, y = FTERES, color = factor(treatment))) +
  geom_line(stat = "summary", fun = "mean", size = 1.2) +
  geom_point(stat = "summary", fun = "mean", size = 2) +
  theme_minimal(base_size = 16) +  # increase overall font size
  labs(
    title = "FTERES by Group",
    y = "FTERES",
    x = "Year",
    color = "Group"
  ) +
  scale_color_manual(
    values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
    labels = c("Control", "Treatment")
  )
print(trend_fteres_group)
ggsave("plots/trend_fteres_group.png", plot = trend_fteres_group, width = 8, height = 8, bg = "white")
