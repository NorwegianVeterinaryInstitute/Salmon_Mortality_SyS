### functions_salmon_SyS ###
# Functions used to run and assess performance of a syndromic surveillance system (SyS) for Atlantic salmon aquaculture.
# Contributors: Victor H. S. Oliveira, Fernanda C. Dórea, Katharine R. Dean, Britt Bang Jensen

# SyS function ----
# Function to generate alarms prospectively

prosp.detect <- function(
    start.year,
    end.year,
    start.month,
    end.month,
    data, 
    training.model, 
    zones,
    mort.max,
    z.detect, 
    n.cons,
    baseline.correction  
)
{
  
  data$id <- 1:dim(data)[1] # counter for the alarms
  
  # train the model 
  for(z in zones){ 
    
    for(m in seq(ymd(paste0(start.year, str_pad(start.month, 2, pad = "0"), "01")),
                 ymd(paste0(end.year, str_pad(end.month, 2, pad = "0"), "01")), 
                 by = "months")) {
      
      #historical data, before our current month m
      retro.model <- glm.nb(training.model,  
                            data %>% 
                              filter(zone %in% z) %>%
                              filter(group_counts != "Inaccurate counting") %>%
                              filter(mort_rate <= mort.max) %>% 
                              filter(!date >= as.Date(m))) 
      
      #our current month m
      detect_df <- data %>% 
        filter(zone %in% z) %>%
        filter(date %in% as.Date(m)) 

      detect_df <- mutate(detect_df, lag_1_deaths = case_when(is.na(lag_1_deaths) ~ 0, TRUE ~ lag_1_deaths))
      
      #prediction for current month, based on retro.model
      prosp.model <- predict(retro.model, detect_df, se.fit = T, type= "response", interval = "prediction")
      
      exp <- prosp.model$fit
      
      UCL <- qnbinom(z.detect, size=retro.model$theta, mu=prosp.model$fit)
      LCL <- qnbinom(1-z.detect, size=retro.model$theta, mu=prosp.model$fit)
      
      #detect df is a data frame with a column called aberration
      detect_df$aberration <- 0 #starts with zeros
      detect_df$aberration[detect_df$death_count > UCL] <- 1 #given 1 if its above the UCL
      detect_df$aberration[detect_df$mort_rate > mort.max] <- 1 #given 1 if its above our threshold (mort.max) 
      
      detect_df$baseline <- detect_df$death_count #observed counts 
      
      #assigning the previous month, in year-month format
      previous.month = as.Date(m) - months(1)
      
      #alarms!
      for(r in 1:dim(detect_df)[1]){ #r=2   #going through row-wise      
        if(detect_df$aberration[r]==0){          #if its zero, its still zero
          detect_df$aberr_cons[r] <- 0      
        }else{
          if(baseline.correction==TRUE){
            detect_df$baseline[r] <- UCL[r] 
          }
          detect_df$aberr_cons[r] <- sum(detect_df$aberration[r], 
                                         data$aberr_cons[data$cohort_id==detect_df$cohort_id[r]&
                                                           data$date == previous.month],
                                         na.rm=T)
        }
      }
      
      detect_df$mort_rate <- detect_df$baseline/detect_df$at_risk_count
      
      # alarm
      for(g in unique(detect_df$cohort_id)) {
        if(any(detect_df$aberr_cons[detect_df$cohort_id == g] >= n.cons))
          detect_df$alarm[detect_df$cohort_id == g] <- 1
        else
          detect_df$alarm[detect_df$cohort_id == g] <- 0
      }
      
      data$exp[detect_df$id] <- exp
      data$LCL[detect_df$id] <- LCL
      data$UCL[detect_df$id] <- UCL
      data$aberration[detect_df$id] <- detect_df$aberration
      data$aberr_cons[detect_df$id] <- detect_df$aberr_cons
      data$baseline[detect_df$id] <- detect_df$baseline
      data$alarm[detect_df$id] <- detect_df$alarm
      data$mort_rate[detect_df$id] <- detect_df$mort_rate
      
    } 
  } 
  
  return(data)
} #function

# Performance functions ----

if(cohort_performance){ # For cohort performance
  
  sys.performance <- function(sys.data, n.cons, i, parameter){ 
    
    performance_data = sys.data %>%  
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
        parameter = parameter,
        parameter_setting = i) %>%
      dplyr::select(date, zone, cohort_id, pred, gold_stand, parameter, parameter_setting, 
                    pd_suspicion_month, sys_outbreak_month) %>%
      slice(1) %>% #cohort
      ungroup() %>%
      mutate(timeliness = interval(pd_suspicion_month, sys_outbreak_month) %/% months(1)) %>%
      mutate(across(everything(), as.character))
    
    return(performance_data)
    
  }
}else { # For monthly performance
  
  sys.performance <- function(sys.data, n.cons, i, parameter){ 
    
    performance_data = sys.data %>%  
      group_by(cohort_id) %>%
      mutate(start_date = dplyr::first(date)) %>%
      filter(sum(!is.na(alarm)) >= n.cons) %>% # cohort
      filter(start_date >= as.Date("2018-01-01")) %>%
      mutate(
        pred = alarm,
        gold_stand = case_when(is.na(pd_status) ~ 0, TRUE ~ pd_status), # production month
        parameter = parameter,
        parameter_setting = i,
        timeliness = NA) %>%
      dplyr::select(date, zone, cohort_id, pred, gold_stand, parameter, parameter_setting, timeliness) %>%
      ungroup() %>%
      mutate(across(everything(), as.character))
    
    return(performance_data)
  }
}

merge.performance.data <- function(data, my_vector, parameter){
  
  merged_performance_data <- data.frame(matrix(ncol=9, nrow=0, dimnames=list(NULL, c("zone", "cohort_id", "pred", "gold_stand", "parameter", "parameter_setting", "pd_suspicion_month", "sys_outbreak_month", "timeliness")))) %>%
    mutate(across(everything(), as.character))
  
  for(i in my_vector){
    
    if(parameter == "training.model"){
      training.model <- i
    } else {
      training.model <- training.model
    }
    
    if(parameter == "mort.max"){
      mort.max <- i
    } else {
      mort.max <- mort.max 
    }
    
    if(parameter == "z.detect"){
      z.detect <- i
    } else {
      z.detect <- z.detect 
    }
    
    if(parameter == "n.cons"){
      n.cons <- i
    } else {
      n.cons <- n.cons 
    }
    
    if(parameter == "baseline.correction"){
      baseline.correction <- i
    } else {
      baseline.correction <- baseline.correction 
    }
    
    prosp.df.sys <- prosp.detect(
      start.year = start.year,
      end.year = end.year,
      start.month = start.month,
      end.month = end.month,
      zones = zones,
      data = data,
      training.model,
      mort.max,
      z.detect,
      n.cons,
      baseline.correction 
    )
    
    tmp_performance <- sys.performance(prosp.df.sys, n.cons, i, parameter)
    
    merged_performance_data <- full_join(merged_performance_data, tmp_performance)
    
  }
  return(merged_performance_data)
} # function

table_performance <- function(df_performance, group_vars){   
  
  df_performance %>%
    group_by(across(all_of(group_vars))) %>%
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
  } # function



