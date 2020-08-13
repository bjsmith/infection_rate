life_exp_thresh <- 70
default_assumed_ifr_percent<-0.6
default_quarantine_failure_odds<-21 # 3 in 65
default_assumed_effectiveness <- 0.7
default_assumed_spread <- 0.02
default_general_travel_rate <- 0.2
default_current_lockdown_passenger_volume <- 9037
#https://www.customs.govt.nz/covid-19/more-information/passenger-statistics/
default_traveler_relative_prevalence <- 1.2 
#derived to bring our predicted "status quo" predicted cases/month numbers to match what we are actually observing at the border.


# life_exp_thresh <- 70
# default_assumed_ifr_percent<-0.6
# default_early_quarantine_failure_odds<-12 # 1 in 12 #this is the quarantine as it was from March to July.
# default_level_measure_effectiveness <- c(0.1,1-0.18,1-0.0246,1-0.01) #likelihood of preventing the case remaining infectious.
# default_spread_quotient <- c(0.023,0.0225,0.0095,0.0202) #likelihood of the virus spreading to a second person at different levels
# #proportion of cases prevented from arriving - not including spread
# #default_assumed_sensitivity <- 0.3
# #default_general_travel_rate <- 0.2
# default_travel_rates <- c(0.5,0.4,0.2,0.1)
# 
# default_current_lockdown_passenger_volume <- 9037
# #https://www.customs.govt.nz/covid-19/more-information/passenger-statistics/
# default_traveler_relative_prevalence <- 1.25
# #derived to bring our predicted "status quo" predicted cases/month numbers to match what we are actually observing at the border.

#Ben J:
#Assume 0.5% transmission risk without mask (it’s a conservative worst case rate)
#If mask use reduces risk by 90% as per Arthur’s email then in-flight transmission risk reduced to 0.05%
#Therefore one case expected to be acquired ‘in-flight’ for every 125 infectious cases carried
#default_aircraft_infection_rate <- 0.005 #without mask
#default_aircraft_mask_effectiveness_percent <- 90

#run_date<-as.Date("2020-07-15")#Sys.Date()
run_date<-Sys.Date()

month_name <- format(run_date,"%B")
