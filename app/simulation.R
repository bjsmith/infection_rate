## Over time, move all functionality to calculate statistics and simulation out of app.R into here.

simulate_treatment_for_countries_with_naive_levels <- function(world_with_covid_data){

  stop("not implemented yet")


}

simulate_treatment_for_countries <- function(
  world_with_covid_data,
  treatment_effectiveness,
  extra_spread=0.02,
  travel_volume_proportion=1,
  assumed_ifr=0.006,
  traveler_relative_prevalence=1,
  current_lockdown_passenger_volume=NULL
  ){
  
  dataset_is_geographic <- FALSE
  if("sf" %in% class(world_with_covid_data)){
    dataset_is_geographic <- TRUE
  }
  
  if(is.null(current_lockdown_passenger_volume)){
    stop("an explicit argument for current_lockdown_passenger_volume is required in get_analysis_covid_data")
    current_lockdown_passenger_volume <- sum(world_with_covid_data$MonthlyArrivalsLockdownScaled1,na.rm=TRUE)
  }
  
  #now we want to scale again according to the change in passenger volume since May
  world_with_covid_data <- 
    world_with_covid_data %>% mutate(
      MonthlyArrivalsScaled2 = MonthlyArrivalsScaled1, # no change
      LocationResidentMonthlyArrivalsScaled2 = LocationResidentMonthlyArrivalsScaled1, #no change
      MonthlyArrivalsLockdownScaled2 = current_lockdown_passenger_volume/sum(MonthlyArrivalsLockdownScaled1,na.rm = TRUE)*MonthlyArrivalsLockdownScaled1
    ) 
  #w2 %>% select(Location,NewDeaths,LaggedNewCases,CaseFatalityRatio) %>% View()
  world_with_covid_data <- 
    world_with_covid_data %>% 
    mutate(CaseFatalityRatio = NewDeaths/LaggedNewCases) %>%
    mutate(InferredDetectionRate = case_when(
      #if there are NO deaths then we infer detection rate is 100%
      NewDeaths==0~1.0,
      #if there are NO cases but there ARE deaths then we set InferredDetectionRate to NA--
      #we infer that the detection rate is completely unknown
      #this indicates probably quite severe under-reporting
      (NewDeaths>0) & (LaggedNewCases==0) ~ as.double(NA),
      #otherwise we set it using our formula
      TRUE~(CaseFatalityRatio/assumed_ifr)
    )
    )
  
  world_with_covid_data <-
    world_with_covid_data %>% 
    mutate(InferredActiveCases= (ActiveCases*InferredDetectionRate))
  
  world_with_covid_data <- 
    world_with_covid_data %>% 
    mutate(ActiveCasePopRate = ActiveCases/Population)
  
  world_with_covid_data <- 
    world_with_covid_data %>% 
    mutate(InferredActiveCasePopRate = (InferredActiveCases/Population)) %>% 
    mutate(InferredActiveCaseTravelerRate = InferredActiveCasePopRate*traveler_relative_prevalence)
  
  ### calculate the probability of at least one COVID-19 case
  
  world_with_covid_data$LocationResidentMonthlyArrivalsWeighted<-(
    world_with_covid_data$LocationResidentMonthlyArrivalsScaled2*travel_volume_proportion
  )
  
  world_with_covid_data <- (
    world_with_covid_data %>% mutate(
      MonthlyArrivalsWeighted = (
        MonthlyArrivalsLockdownScaled2 + pmax(0,MonthlyArrivalsScaled2-MonthlyArrivalsLockdownScaled2)*travel_volume_proportion
      )
    )%>% mutate(
      StatusQuoMonthlyArrivals = (
        MonthlyArrivalsLockdownScaled2
      )
      )%>% 
        mutate(
        Total2019MonthlyArrivals = (
          MonthlyArrivalsScaled2
        )
      )
  )
  
  
  #assume that each new person has an independent chance of carrying COVID-19
  #this is conservative when we are trying to work out the probability of at least one case
  #so the probability is going to be 
  #1-((Population-ProbableActiveCases)/Population)^LocationResidentMonthlyArrivals
  #if 10% of the population of 5000 has covid, and 20 of them come to NZ, that's
  #1-((5000-500)/5000)^20
  #1-(4500/5000)^20
  #0.8784233

  world_with_covid_data <- 
    world_with_covid_data %>% mutate(
      ProbabilityOfMoreThanZeroCases=1-(1-InferredActiveCaseTravelerRate)^MonthlyArrivalsWeighted,
      ExpectedCasesAtBorder=InferredActiveCaseTravelerRate*MonthlyArrivalsWeighted,
      ExpectedCasesAtBorderAt2019Levels=InferredActiveCaseTravelerRate*Total2019MonthlyArrivals,
      ExpectedCasesAtBorderUnderLockdown=InferredActiveCaseTravelerRate*StatusQuoMonthlyArrivals
    )
  
  
  
  ######### TREATMENT
  ###
  ### EXCLUDES the dampening effect of quarantine on travel - this is taken into account above.
  ### This function currently can only calculate the effect of a particular treatment
  ### Run the function multiple times to see different treatments
  
  prob_infected_arr_reaches_community <- (1-treatment_effectiveness)+(extra_spread)
  
  #odds that a detainee in quarantine with COVID-19 leaves before being COVID-19
  # if(is.null(quarantine_odds_override)){
  #   probability_an_infected_arrival_escapes_quarantine <- 1/50
  #   probability_an_infected_arrival_leaves_quarantine_undetected <- 0.01
  #   prob_infected_arr_reaches_community<-probability_an_infected_arrival_escapes_quarantine+probability_an_infected_arrival_leaves_quarantine_undetected
  # }else{
  #   prob_infected_arr_reaches_community<-quarantine_odds_override
  # }
  # 
  
  ######### SCREENING TREATMENT
  ### EXCLUDES the dampening effect of quarantine on travel - this is taken into account above.
  #treatment_effectiveness <- 0.7
  # screening_specificity <- 0.999
  #I think we need negative predictive value as well as positive predictive value.
  #https://en.wikipedia.org/wiki/Positive_and_negative_predictive_values#Positive_predictive_value
  #https://en.wikipedia.org/wiki/Positive_and_negative_predictive_values#Negative_predictive_value
  prevalence <- world_with_covid_data$InferredActiveCaseTravelerRate
  #this is the "detection rate" or how many true positives we catch for the total number of calls
  # positive_predictive_value <- (
  #   (treatment_effectiveness * (prevalence))/
  #     (treatment_effectiveness*screening_specificity + (1-screening_specificity)*(1-prevalence))
  # )
  #this is the the proportion of true negatives we catch out of the total number of negatives
  # negative_predictive_value <- (
  #   (screening_specificity * (1-prevalence))/
  #     ((1-treatment_effectiveness)*prevalence+screening_specificity*(1-prevalence))
  # )
  #so then we can:
  #(a) from an expected number of true positive cases arriving (ExpectedNumberOfCasesAll), calculate the number we'll catch by
  #     number of positive test results = N(true positives)/PPV
  #(b) we can get the False Omission Rate which is just the complement of NPV (FOR = 1-NPV) which is the rate of 
  
  #or are we really just interested in SENSITIVITY for now?
  #sensitivity tells us how much we'll correctly identify those with the disease - what proportion of positive patients we'll catch
  #so this is actually a lot easier than all the above...
  # world_with_covid_data <- 
  #   world_with_covid_data %>% mutate(
  #     ExpectedNumberOfCasesEscapingOneScreen=ExpectedNumberOfCasesAll*(1-treatment_effectiveness))
      
  
  # world_with_covid_data <- 
  #   world_with_covid_data %>% mutate(
  #     ProbabilityOfMoreThanZeroCommunityCases=
  #       1-((Population-InferredActiveCases*prob_infected_arr_reaches_community)/Population)^MonthlyArrivalsWeighted)
  
  world_with_covid_data <- 
    world_with_covid_data %>% mutate(
      ExpectedNumberOfCasesInCommunity=ExpectedCasesAtBorder*prob_infected_arr_reaches_community,
      ExpectedNumberOfCasesInCommunityUnderLockdown=ExpectedCasesAtBorderUnderLockdown*prob_infected_arr_reaches_community,
      TreatmentCommunitySpreadProportion=prob_infected_arr_reaches_community
      )
  
  #get the predicted two-week prevalence
  #this is quite rough - is just hte predicted cases over the next two weeks
  world_with_covid_data <- world_with_covid_data %>% ungroup %>%
    group_by(LocationCode) %>% 
    mutate(PredictedActiveCases = next_two_weeks_cases,
           PredictedActiveCasesPerMillion = next_two_weeks_cases / Population * 10^6
           
    ) %>% 
    mutate(
      PredictedInfActiveCases = PredictedActiveCases*InferredDetectionRate,
      PredictedInfActiveCasesPerMillion = PredictedActiveCasesPerMillion*InferredDetectionRate,
      
    ) %>%
    ungroup
  
  world_with_covid_data$InfActiveCasesPerMillion <- world_with_covid_data$InferredActiveCaseTravelerRate*10^6
  world_with_covid_data$InfActiveCasesPerThousand <- world_with_covid_data$InferredActiveCaseTravelerRate*10^3
  # world_with_covid_data$ActiveCasesPerThousand <- world_with_covid_data$ActiveCasePopRate*10^3
  # world_with_covid_data$ActiveCasesPerMillion <- world_with_covid_data$ActiveCasePopRate*10^6
  
  #rate overall prevalence
  world_with_covid_data <- 
    world_with_covid_data %>% 
    rowwise() %>%
    mutate(PrevalenceRating=classify_country_prevalence(LifeExp, InfActiveCasesPerMillion),
           OutlookRating=classify_country_prevalence(LifeExp, PredictedInfActiveCasesPerMillion),
           DataReliablityRating = classify_country_trust(LifeExp,Location)
           ) %>%
    ungroup()
    
  #need to relabel as geographic due to the rowwise above.
  if (dataset_is_geographic){
    world_with_covid_data <- st_as_sf(world_with_covid_data)
  }
  
  return(world_with_covid_data)
}

