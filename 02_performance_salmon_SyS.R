### 02_performance_salmon_SyS ###
# Performance assessment of SyS for choosing proper settings.
# Contributors: Victor H. S. Oliveira, Fernanda C. Dórea, Katharine R. Dean, Britt Bang Jensen

library(here)
source(here("01_run_salmon_SyS.R"))

# Without a model ----

# Assessing the performance of SyS without any predictive model, but with other parameters

df_sys$ext_mort <-  ifelse(df_sys$mort_rate <= mort.max, 0, 1) # ext_mort is an assumed extreme mortality
df_sys$n_ext_mort <- NA

tmp <- "x"
for(i in 1:dim(df_sys)[1]){ 
  if(df_sys$cohort_id[i] == tmp){
    if(df_sys$ext_mort[i] == 0){
      df_sys$n_ext_mort[i] <- 0
    } else {
      df_sys$n_ext_mort[i] <- sum(df_sys$ext_mort[i], df_sys$n_ext_mort[i-1])
    }  
  } else {df_sys$n_ext_mort[i] <- df_sys$ext_mort[i]
  tmp <- df_sys$cohort_id[i]}
}
rm(tmp)

# Outbreak alarm without model
df_sys$outbreak_wo_model <- NA

df_sys <- df_sys %>% 
  mutate(outbreak_wo_model = case_when(n_ext_mort == n.cons ~ 1, 
                                       TRUE ~ NA_real_)) %>%
  group_by(cohort_id) %>%
  tidyr::fill(outbreak_wo_model, .direction = "down") %>%
  ungroup() %>%
  mutate(outbreak_wo_model = case_when(is.na(outbreak_wo_model)~0,
                                       TRUE ~ outbreak_wo_model))


if(cohort_performance == TRUE) {
  
  training.model_performance_no_model <- df_sys %>% 
    group_by(cohort_id) %>%
    mutate(sys_outbreak_month = 
             case_when(outbreak_wo_model == 1 & (lag(outbreak_wo_model == 0) | is.na(lag(outbreak_wo_model))) ~ date,
                       TRUE ~ NA_real_)) %>%
    tidyr::fill(sys_outbreak_month, .direction = "updown") %>%
    mutate(start_date = dplyr::first(date)) %>%
    filter(sum(!is.na(outbreak_wo_model)) >= n.cons) %>% # minimum number to be classified as an outbreak alarm in settings
    filter(start_date >= as.Date("2018-01-01")) %>% # Date of implementation of SyS
    mutate(
      pred = outbreak_wo_model,
      pred = case_when(any(outbreak_wo_model == 1) ~ 1, TRUE~ 0),
      gold_stand = case_when(any(pd_status == 1) ~ 1, TRUE ~ 0),
      parameter = "training.model",
      parameter_setting = "no_model") %>%
    dplyr::select(date, zone, cohort_id, pred, gold_stand, parameter, parameter_setting, 
                  pd_suspicion_month, sys_outbreak_month) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(timeliness = interval(pd_suspicion_month, sys_outbreak_month) %/% months(1)) %>%
    mutate(across(everything(), as.character)) 
  
}else {
  
  training.model_performance_no_model <- df_sys %>% 
    group_by(cohort_id) %>%
    mutate(start_date = dplyr::first(date)) %>%
    filter(sum(!is.na(outbreak_wo_model)) >= n.cons) %>% # minimum number to be classified as an outbreak
    filter(start_date >= as.Date("2018-01-01")) %>% # Date of implementation of SyS
    mutate(
      pred = outbreak_wo_model,
      gold_stand = case_when(is.na(pd_status) ~ 0, TRUE ~ pd_status),
      parameter = "training.model",
      parameter_setting = "no_model",
      timeliness = NA) %>%
    dplyr::select(date, zone, cohort_id, pred, gold_stand, parameter, parameter_setting, timeliness) %>% 
    ungroup() %>%
    mutate(across(everything(), as.character))
  
}

# With a model ----

# Assessing the performance of SyS with a predictive model. Different models are tested for parametrization.

## Parameters ----

# Varying parameters tested are: 
# Mortality rate cutoff (MR cutoff), predictive models (Model), upper control limit (UCL),
# number of consecutive aberrations to generate alarms (N aberrations), and baseline correction

### MR cutoff ----

mort.max_vector <- c(seq(0.003, 0.05, by = 0.001))

### Models ----

training.model1 <- "baseline ~ 1"

training.model2 <- "baseline ~ offset(log(at_risk_count))"

training.model3 <- "baseline ~ lag_1_deaths + offset(log(at_risk_count))"

training.model4 <- "baseline ~ lag_1_deaths + temp + offset(log(at_risk_count))"

training.model5 <- "baseline ~ lag_1_deaths + temp + w_fish + offset(log(at_risk_count))"

training.model6 <- "baseline ~ lag_1_deaths + temp + w_fish + treat_count_cat + offset(log(at_risk_count))"

training.model7 <- "baseline ~ lag_1_deaths + temp + w_fish + treat_count_cat + start_m + offset(log(at_risk_count))"

training.model8 <- "baseline ~ lag_1_deaths + temp + w_fish + treat_count_cat + start_m + w_start_fish + offset(log(at_risk_count))"

training.model_vector <- c(training.model1, training.model2, training.model3,
                           training.model4, training.model5, training.model6,
                           training.model7, training.model8)

### UCL ----

z.detect_vector <- c(seq(.05, .95, by = 0.05))

### N aberrations ---- 

n.cons_vector <- c(1,2,3,4,5,6)

### Baseline correction ----

# Correction with substitution or not of the observed data with the detection limit
baseline.correction_vector <- c(TRUE, FALSE)

## Predictive models ----

training.model_performance <- merge.performance.data(prosp_df, training.model_vector, "training.model")

model_info <- data.frame(training.model_vector, id= c(1:length(training.model_vector)))

training.model_performance <- training.model_performance %>%
  left_join(model_info, by = c("parameter_setting" = "training.model_vector")) %>%
  mutate(parameter_setting = as.character(id)) %>%
  dplyr::select(-id)

# Adding performance without any model 
training.model_performance <- bind_rows(training.model_performance, training.model_performance_no_model)

## Maximum baseline ----
mort.max_performance <- merge.performance.data(prosp_df, mort.max_vector, "mort.max")

## Prediction intervals ----
z.detect_performance <- merge.performance.data(prosp_df, z.detect_vector, "z.detect")

## Consecutive months high mortality ---- 
n.cons_performance <- merge.performance.data(prosp_df, n.cons_vector, "n.cons")

## Baseline correction
baseline.correction_performance <- merge.performance.data(prosp_df, baseline.correction_vector, "baseline.correction")

# Select best cut-offs -----

df_performance <- rbind(training.model_performance,
                        mort.max_performance,
                        z.detect_performance,
                        n.cons_performance, 
                        baseline.correction_performance)

#save(df_performance, file=paste0(here("formatted_data", "df_performance"),"_", lastUpdate, ".rds"))

table_performance <- table_performance(
  df_performance,
  c("parameter", "parameter_setting", "zone")
)

save(table_performance, file=paste0(here("data", "table_performance"),"_", lastUpdate, ".rds"))
write_csv2(table_performance, file=paste0(here("data", "table_performance"),"_", lastUpdate, ".csv"))
