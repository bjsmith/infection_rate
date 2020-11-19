
#default_run_date<-as.Date("2020-08-22")
default_run_date<-Sys.Date()
default_month_name <- format(default_run_date,"%B")

setwd("/Users/benjaminsmith/Google Drive/politics/coronavirus/code/infection_rate/app/")
debugSource("utils.R")
print_elapsed_time("START")

debugSource("simulation.R")
source("country_classification_rules.R")
source("simJourneyPanel.R")
source("defaults.R")
source("journey_simulation_procedural.R")
verbose<-FALSE

print_elapsed_time("loaded dependencies")
#Ben J:
#Assume 0.5% transmission risk without mask (it’s a conservative worst case rate)
#If mask use reduces risk by 90% as per Arthur’s email then in-flight transmission risk reduced to 0.05%
#Therefore one case expected to be acquired ‘in-flight’ for every 125 infectious cases carried
#default_aircraft_infection_rate <- 0.005 #without mask
#default_aircraft_mask_effectiveness_percent <- 90

#run_date<-as.Date("2020-07-15")#Sys.Date()
#run_date<-as.Date("2020-08-15")#Sys.Date()
#run_date<-Sys.Date()
#run_date<-as.Date("2020-08-14")
#run_date<-as.Date("2020-08-22")

#month_name <- format(run_date,"%B")


print_elapsed_time("LOADING DATA 2...")
default_nogeo_world_basic_data <- get_geomapped_covid_data(life_exp_thresh,default_run_date,separate_aussie_states_and_hk = TRUE,include_geo_data = FALSE)
default_simulation_data <- simulate_treatment_for_countries(
  default_nogeo_world_basic_data,
  treatment_effectiveness = default_assumed_effectiveness,
  extra_spread = 0,
  assumed_ifr = default_assumed_ifr_percent/100,
  traveler_relative_prevalence=default_traveler_relative_prevalence,
  current_lockdown_passenger_volume = default_current_lockdown_passenger_volume)

countries_to_choose_from<-
  default_nogeo_world_basic_data$Location %>%
  sort %>%
  .[.!="New Zealand"]


small_country_health_data <-data.frame(
  "Country"=c("Samoa", "Cook Islands"),
  "Website"=c("https://www.health.gov.ws/", "https://www.health.gov.ck/")
)



source("key_interest_countries.R")


###########create data table for table tab
#lose geometry info and convert to datatable. note: not the same as data.table!

###########main dashboard.
source("components/method_and_approach.R")
source('components/intervention_simulation.R')
source('components/journey_page.R')
source('components/proposal.R')
source('components/summary_map.R')
source('components/summary_page.R')


default_geo_world_basic_data <- 
  simulate_treatment_for_countries(
  get_geomapped_covid_data(life_exp_thresh,default_run_date),
  treatment_effectiveness = 0.5/100,
  extra_spread = default_assumed_spread,
  assumed_ifr = 0.6/100,
  current_lockdown_passenger_volume = 9037)%>% 
  filter(LifeExp>=life_exp_thresh)


  show_leaflet(data_to_show = default_geo_world_basic_data%>% filter(!is.na(ActiveCases)),
               primary_col = "ActiveCases",
               rounding_func = function(x){scales::comma(round(x,1))},
               legend_title =  "Observed active cases",
               quant_grades = 4,
               pal_reverse = FALSE
  )
