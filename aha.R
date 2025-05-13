library(readr)
library(dplyr)
library(tidyr)

# read csv --------------------------------------------------------------------
aha <- read_csv("/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/input/AHAdata_20052023.csv")
hcris <- read_csv(paste0(raw_data_path, "/final_HCRIS_data.csv"))
