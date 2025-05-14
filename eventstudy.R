library(ggplot2)

#Now that i have hosp_data, lets check out parallel trends 
# treatment group when first penalized == 2012
hosp_data <- hosp_data %>%
  mutate(treatment = ifelse(first_penalty == "2012", 1, 0)) 

# control group when hospitals are either never penalized or first penalized after 2016 
hosp_data <- hosp_data %>%
  mutate(control = ifelse(first_penalty == "none" | first_penalty > 2016, 1, 0))

# drop when after 2016 
hosp_data <- hosp_data %>%
  filter(YEAR <= 2016)

#descriptives
#plot for full time equiv per bed over time by penalty status
ftern_mean <- hosp_data %>%
  group_by(YEAR, treatment) %>%
  summarise(ftern_bed = mean(FTERN/ beds, na.rm = TRUE)) %>%
  ungroup()

ggplot(ftern_mean, aes(x = YEAR, y = ftern_bed , color = factor(treatment))) +
  geom_line(stat = "summary", fun = mean, size = 1) +
  labs(x = "year", y = "FTERN per bed", color = "Penalized in 2012",
       title = "Full Time Equivalent RNs per Bed ) +
  theme_minimal()   


