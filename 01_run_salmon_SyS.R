### 01_run_salmon_SyS ###
# Run SyS with the defined settings in "02_settings_salmon_SyS.R".
# Contributors: Victor H. S. Oliveira, Fernanda C. Dórea, Katharine R. Dean, Britt Bang Jensen

Sys.setlocale("LC_TIME", "C")

# Packages ----
library(here)
library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)
library(stringr)
library(MASS)
library(readr)

cohort_performance <- TRUE # If TRUE, runs performance script at cohort-level. If false, at monthly-level.

source(here("settings_salmon_SyS.R"))
source(here("functions_salmon_SyS.R"))

# Load data ----

df_sys <- readRDS(here("data", "df_sys.rds"))

# Prospective df ----

prosp_df <- df_sys 
prosp_df$baseline <- prosp_df$death_count
prosp_df$baseline[prosp_df$year %in% seq(start.year, end.year)]<-NA
prosp_df$exp <- NA
prosp_df$LCL <- NA
prosp_df$UCL <- NA
prosp_df$aberration <- NA 
prosp_df$aberr_cons <- NA 
prosp_df$alarm <- NA 

# Implement SyS ----

df_sys_prosp <- prosp.detect(
  start.year = start.year,
  end.year = end.year,
  start.month = start.month,
  end.month = end.month,
  zones = zones,  
  data = prosp_df,
  training.model = training.model,
  mort.max = mort.max,
  z.detect = z.detect,
  n.cons = n.cons,
  baseline.correction = baseline.correction
)

# Save ----
lastUpdate <- Sys.time()
lastUpdate <- substr(lastUpdate, 1, 10)
saveRDS(df_sys_prosp, file=paste0(here("data", "df_sys_prosp"),"_", lastUpdate, ".rds"))



