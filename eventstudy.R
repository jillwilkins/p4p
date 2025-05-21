library(ggplot2)
library(fixest)

#Now that i have hosp_data, lets check out parallel trends 
# treatment group when first penalized == 2012
hosp_data <- hosp_data %>%
  mutate(treatment = ifelse(first_penalty == "2012", 1, 0)) 

# control group when hospitals are either never penalized or first penalized after 2016 
hosp_data <- hosp_data %>%
  mutate(control = ifelse(first_penalty == "none" | first_penalty > 2016, 1, 0))

# filter 
hosp_data <- hosp_data %>% filter(YEAR <= 2016 & YEAR > 2008)
hosp_data <- hosp_data %>%
  filter(treatment == 1 | control == 1)

# Create a treatment-time interaction variable for DiD
hosp_data <- hosp_data %>%
  mutate(post = ifelse(YEAR >= 2012, 1, 0),  
         did = treatment * post)

#descriptives
#count if control or treatment group
hosp_data %>%
  group_by(treatment) %>%
  summarise(num_hospitals = n()) %>%
  arrange(desc(num_hospitals))

  hosp_data %>%
  group_by(treatment) %>%
  summarise(num_hospitals = n_distinct(MCRNUM)) %>%
  arrange(desc(num_hospitals))


# FTERN
# check for parallel trends
hosp_data <- hosp_data %>%
  mutate(event_time = YEAR - 2012)

event_ftern <- feols(
  FTERN ~ i(event_time, treatment, ref = -1) | MCRNUM + YEAR,
  data = hosp_data,
  cluster = ~MCRNUM
  )
summary(event_ftern)
iplot(event_ftern,
      xlab = "Event Time",
      main = "Event Study: Full Time Equivalent RN and 2012 Penalties")

ggsave("plots/event_ftern.png")

# differenece in difference regression 
did_model <- feols(
  FTERN ~ did + post + treatment | MCRNUM,  # Fixed effects for hospitals
  data = hosp_data
)
summary(did_model)

# FTELPN 
# check for parallel trends
event_lpn <- feols(
  FTELPN ~ i(event_time, treatment, ref = -1) | MCRNUM + YEAR,
  data = hosp_data,
  cluster = ~MCRNUM
)
summary(event_lpn)

iplot(event_lpn,
      xlab = "Event Time",
      main = "Event Study: Full Time Equivalent LPNs and 2012 Penalties")

# diff in diff regression 
did_model_lpn <- feols(
  FTELPN ~ did + post + treatment | MCRNUM,  # Fixed effects for hospitals
  data = hosp_data
)
summary(did_model_lpn)
ggsave("plots/event_ftelpn.png")
