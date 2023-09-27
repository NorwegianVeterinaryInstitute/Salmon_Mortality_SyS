### settings_salmon_SyS ###
# Define settings to run SyS.
# Contributors: Victor H. S. Oliveira, Fernanda C. Dórea, Katharine R. Dean, Britt Bang Jensen

# The SyS settings below were chosen to illustrate SyS implementation. These settings are customizable.  
# To choose settings based on performance assessments of SyS with varying parameters, see "02_performance_salmon_SyS.R"

start.year = 2018
end.year = 2021  #for an up-to-date system, format(Sys.Date(), format="%Y")
start.month = 1
end.month = 12 #for an up-to-date system, format(Sys.Date(), format="%m")

zones = c(3, 4, 5, 6) 

training.model <- baseline ~ lag_1_deaths + temp + w_fish + treat_count_cat + offset(log(at_risk_count))

mort.max = 0.02
z.detect = .65
n.cons = 3
baseline.correction = TRUE