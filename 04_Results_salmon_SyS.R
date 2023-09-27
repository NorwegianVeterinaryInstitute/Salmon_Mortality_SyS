### 04_Results_salmon_SyS ###
# Tables and Figures used in the Results section of the manuscript.
# Contributors: Victor H. S. Oliveira, Fernanda C. Dórea, Katharine R. Dean, Britt Bang Jensen

source(here("01_run_salmon_SyS.R"))

# Table 3 ----

# Full study period 

table(df_sys$zone) # Months per production zone

df_sys %>% 
  group_by(cohort_id) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(zone) %>%
  tally() # 

df_sys %>% 
  group_by(location) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(zone) %>%
  tally()

# performance period

df_sys %>%
  filter(date >= "2018-01-01") %>%
  group_by(zone) %>%
  tally()

df_sys %>% 
  filter(date >= "2018-01-01") %>%
  group_by(cohort_id) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(zone) %>%
  tally() # 

df_sys %>% 
  filter(date >= "2018-01-01") %>%
  group_by(location) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(zone) %>%
  tally()

# Table 4 ----

# Title: Parameters of the proposed syndromic surveillance system (SyS) using mortality data from salmon farms in Norway. 
# Please, see manuscript for formatted table. 

# Table 5 ----

# Title: Performance of the proposed syndromic surveillance system using mortality data in detecting salmon pancreas disease outbreaks monthly and at cohort level. 

# 1. Change settings in "settings_salmon_SyS.R to those described in table 4, specifying a single zone of interest.
# 2. Run "01_run_salmon_SyS.R".
# 3. Make performance tables using code below

# For monthly performance values:
performance_data <- df_sys_prosp %>%  
  group_by(cohort_id) %>%
  mutate(start_date = dplyr::first(date)) %>%
  filter(sum(!is.na(alarm)) >= n.cons) %>% # cohort
  filter(start_date >= as.Date("2018-01-01")) %>%
  mutate(
    pred = alarm,
    gold_stand = case_when(is.na(pd_status) ~ 0, TRUE ~ pd_status), # production month
    parameter = parameter) %>%
  dplyr::select(date, zone, cohort_id, pred, gold_stand) %>%
  ungroup() %>%
  mutate(across(everything(), as.character))

performance_data %>%
  summarise(
    TP = length(which(pred == 1 & gold_stand == 1)),
    FP = length(which(pred == 1 & gold_stand == 0)),
    TN = length(which(pred == 0 & gold_stand == 0)),
    FN = length(which(pred == 0 & gold_stand == 1)),
    FPR = FP/(FP+TN), # aka false alarm rate (FAR)
    `TPR(Se)` = TP/(TP+FN),
    `TNR(Sp)` = TN/(TN+FP),
    PPV = TP/(TP+FP),
    NPV = TN/(TN+FN))

# For cohort performance values:
performance_data = df_sys_prosp %>%  
  group_by(cohort_id) %>%
  mutate(sys_outbreak_month = 
           case_when(alarm == 1 & (lag(alarm == 0) | is.na(lag(alarm))) ~ date,
                     TRUE ~ NA_real_)) %>%
  tidyr::fill(sys_outbreak_month, .direction = "updown") %>%
  mutate(start_date = dplyr::first(date)) %>%
  filter(sum(!is.na(alarm)) >= n.cons) %>% # cohort
  filter(start_date >= as.Date("2018-01-01")) %>%
  mutate(
    pred = alarm,
    pred = case_when(any(alarm == 1) ~ 1, TRUE~ 0), # cohort
    gold_stand = case_when(any(pd_status == 1) ~ 1, TRUE ~ 0), # cohort
  )%>%
  dplyr::select(date, zone, cohort_id, pred, gold_stand, 
                pd_suspicion_month, sys_outbreak_month) %>%
  slice(1) %>% #cohort
  ungroup() %>%
  mutate(timeliness = interval(pd_suspicion_month, sys_outbreak_month) %/% months(1)) %>%
  mutate(across(everything(), as.character))

performance_data %>%
  summarise(
    TP = length(which(pred == 1 & gold_stand == 1)),
    FP = length(which(pred == 1 & gold_stand == 0)),
    TN = length(which(pred == 0 & gold_stand == 0)),
    FN = length(which(pred == 0 & gold_stand == 1)),
    FPR = FP/(FP+TN), # aka false alarm rate (FAR)
    `TPR(Se)` = TP/(TP+FN),
    `TNR(Sp)` = TN/(TN+FP),
    PPV = TP/(TP+FP),
    NPV = TN/(TN+FN),
    median_ttd = median(as.numeric(timeliness), na.rm = TRUE),
    q1_ttd = quantile(as.numeric(timeliness), 0.25, na.rm=TRUE),
    q3_ttd = quantile(as.numeric(timeliness), 0.75, na.rm=TRUE))