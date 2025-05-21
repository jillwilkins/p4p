#plot FTERN per bed over time by penalty status
ggplot(hosp_data, aes(x = YEAR, y = ftern_bed , color = factor(treatment))) +
  geom_line(stat = "summary", fun = mean, linewidth = 1) +
  labs(x = "year", y = "FTE RN per bed", color = "Treatment Status",
       title = "Full Time Equivalent RNs Over Time ") +
  theme_minimal() + 
  coord_fixed(ratio = 10)

ggsave("plots/ftern_per_bed_plot.png")