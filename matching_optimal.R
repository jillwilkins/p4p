# repeat the matching process using optimal matching
match_optimal <- matchit(
  treatment ~ bed_avg + ADMTOT,
  data = hosp_2012 %>% filter(!is.na(bed_avg)),
  method = "optimal",
  distance = "logit",
)

matched_df_o <- match.data(match_optimal)
matched_mcrnums <- matched_df_o$MCRNUM

panel_opt <- hosp_2012 %>%
  filter(MCRNUM %in% matched_df_o$MCRNUM) %>%
  left_join(
    matched_df_o %>% 
      select(MCRNUM, weights) %>% 
      distinct(MCRNUM, .keep_all = TRUE),
    by = "MCRNUM"
  ) %>%
  mutate(event_time = YEAR - 2012)

pscores_opt <- match_optimal$distance
summary(pscores_opt)
# event study on matched data
# Run model balanced panel

#FTEMND
esmatch_ftemd_opt <- feols(
  FTEMD ~ i(event_time, treatment, ref = -1) | MCRNUM + YEAR,
  data = panel_opt,
  cluster = ~MCRNUM,
)

summary(esmatch_ftemd_opt)

png("FTEMD_es_opt.png", width = 800, height = 800)
iplot(esmatch_ftemd_opt,
      xlab = "Event Time",
      main = "Event Study: FTEMD and 2012 Penalties (matched optimal)")
dev.off()

did <- feols(
  FTEMD ~ did + post + treatment | MCRNUM + YEAR, 
  data = panel_opt) 
summary(did)

# FTERN 
esmatch_ftern_opt <- feols(
  FTERN ~ i(event_time, treatment, ref = -1) | MCRNUM + YEAR,
  data = panel_opt,
  cluster = ~MCRNUM,
)
summary(esmatch_ftern_opt)

png("FTERN_es_opt.png", width = 800, height = 800)
iplot(esmatch_ftern_opt,
      xlab = "Event Time",
      main = "Event Study: FTERN and 2012 Penalties (matched optimal)")
dev.off()

did <- feols(
  FTERN ~ did + post + treatment | MCRNUM + YEAR, 
  data = panel_opt) 
summary(did)

# FTRNTF
esmatch_ftrntf_opt <- feols(
  FTRNTF ~ i(event_time, treatment, ref = -1) | MCRNUM + YEAR,
  data = panel_opt,
  cluster = ~MCRNUM,
)
summary(esmatch_ftrntf_opt)
# FTELPN 
esmatch_ftelpn_opt <- feols(
  FTELPN ~ i(event_time, treatment, ref = -1) | MCRNUM + YEAR,
  data = panel_opt,
  cluster = ~MCRNUM,
)
summary(esmatch_ftelpn_opt)

png("FTELPN_es_opt.png", width = 800, height = 800)
iplot(esmatch_ftelpn_opt,
      xlab = "Event Time",
      main = "Event Study: FTELPN and 2012 Penalties (matched optimal)")
dev.off()

did <- feols(
  FTELPN ~ did + post + treatment | MCRNUM + YEAR, 
  data = panel_opt) 
summary(did)

# FTERES
esmatch_fteres_opt <- feols(
  FTERES ~ i(event_time, treatment, ref = -1) | MCRNUM + YEAR,
  data = panel_opt,
  cluster = ~MCRNUM,
)
summary(esmatch_fteres_opt)
png("FTERES_es_opt.png", width = 800, height = 800)
iplot(esmatch_fteres_opt,
      xlab = "Event Time",
      main = "Event Study: FTERES and 2012 Penalties (matched optimal)")
dev.off()
