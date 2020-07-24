## Over time, move all functionality to calculate statistics and simulation out of app.R into here.
get_analysis_covid_data <- function(world_with_covid_data,quarantine_odds_override=NULL,general_travel_rate=1){
  print(quarantine_odds_override)
  print(general_travel_rate)
  
  world_with_covid_data$InferredDetectionRate[is.infinite(world_with_covid_data$InferredDetectionRate)]<-1
  #can't be infinite
  world_with_covid_data$InferredDetectionRate[world_with_covid_data$InferredDetectionRate>1]<-1
  #can't be more than 1
  
  
  ### calculate the probability of at least one COVID-19 case
  
  #world_with_covid_data$LocationResidentMonthlyArrivals<-as.numeric(world_with_covid_data$LocationResidentMonthlyArrivals)
  world_with_covid_data$LocationResidentMonthlyArrivalsWeighted<-(
    world_with_covid_data$LocationResidentMonthlyArrivalsScaled1*general_travel_rate
  )
  
  world_with_covid_data <- (
    world_with_covid_data %>% mutate(
      NZResidentMonthlyArrivalsWeighted = (
        NZResMonthlyArrivalsLatest + pmax(0,NZResMonthlyArrivals-NZResMonthlyArrivalsLatest)*general_travel_rate
      )
    )
  )
  
  world_with_covid_data <- (
    world_with_covid_data %>% mutate(
      TotalExpectedMonthlyArrivals = (
        NZResidentMonthlyArrivalsWeighted + LocationResidentMonthlyArrivalsWeighted
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
      ProbabilityOfMoreThanZeroCases=1-((Population-InferredActiveCases)/Population)^TotalExpectedMonthlyArrivals)
  
  world_with_covid_data <- 
    world_with_covid_data %>% mutate(
      ExpectedNumberOfCasesAll=InferredActiveCasePopRate*TotalExpectedMonthlyArrivals,
      ExpectedNumberOfCasesUnderNZResidentQuarantine=InferredActiveCasePopRate*NZResMonthlyArrivalsLatest,
      ExpectedNumberOfCasesNZResident=InferredActiveCasePopRate*NZResidentMonthlyArrivalsWeighted,
      ExpectedNumberOfCasesForeign=InferredActiveCasePopRate*LocationResidentMonthlyArrivalsWeighted
      )
  
  
  
  #odds that a detainee in quarantine with COVID-19 leaves before being COVID-19
  if(is.null(quarantine_odds_override)){
    probability_an_infected_arrival_escapes_quarantine <- 1/50
    probability_an_infected_arrival_leaves_quarantine_undetected <- 0.01
    prob_infected_arr_reaches_community<-probability_an_infected_arrival_escapes_quarantine+probability_an_infected_arrival_leaves_quarantine_undetected
  }else{
    prob_infected_arr_reaches_community<-quarantine_odds_override
  }
  
  
  world_with_covid_data <- 
    world_with_covid_data %>% mutate(
      ProbabilityOfMoreThanZeroCommunityCases=
        1-((Population-InferredActiveCases*prob_infected_arr_reaches_community)/Population)^TotalExpectedMonthlyArrivals)
  
  world_with_covid_data <- 
    world_with_covid_data %>% mutate(
      ExpectedNumberOfCasesInCommunity=ExpectedNumberOfCasesAll*prob_infected_arr_reaches_community)
  
  
  world_with_covid_data$InfActiveCasesPerMillion <- world_with_covid_data$InferredActiveCasePopRate*10^6
  world_with_covid_data$InfActiveCasesPerThousand <- world_with_covid_data$InferredActiveCasePopRate*10^3
  world_with_covid_data$ActiveCasesPerThousand <- world_with_covid_data$ActiveCasePopRate*10^3
  world_with_covid_data$ActiveCasesPerMillion <- world_with_covid_data$ActiveCasePopRate*10^6
  
  return(world_with_covid_data)
}

#this method will focus on returning resident risk for country
#using the infection rates from that country, same as we do for non-NZ residents
#we may be able to use the _actual_ rates of COVID-19 among returnees to re-weight these up or down
#but for now we'll just use the John-Hopkins-reported results.
get_nz_returning_resident_risk_for_country <- function(
  observed_monthly_arrivals_under_quarantine,
  observed_monthly_arrivals_pre_covid,
  origin_infection_rate,
  general_travel_rate
){
  stop("obsolete. this logic is incorporated into get_analysis_covid_data")
  #the General Travel Rate is the figure we use to measure the proportion of foreign residents who will come to NZ
  #Pre-COVID, the General Travel Rate is 100%.
  #under NZ citizens and residents only policy, it's 0%.
  #we expect under restrictions, e.g., mandatory quarantine, only a proportion of visitors are motivated to come to NZ
  
  #make the NZ returning resident count equal to nz monthly arrivals under quarantine,
  #plus DIFFERENCE between the current and pre-covid travel volume, multiplied by the GTR
  returning_residents_per_month <- (
    observed_monthly_arrivals_under_quarantine+
      max(observed_monthly_arrivals_pre_covid-observed_monthly_arrivals_under_quarantine,0)*general_travel_rate
  )
  risk <- returning_residents_per_month * origin_infection_rate
  
  return(risk)
}