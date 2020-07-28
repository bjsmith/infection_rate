## Over time, move all functionality to calculate statistics and simulation out of app.R into here.




#helper function for structuring data about the travel rate.
set_general_travel_rate <- function(l1_bubble,l2_screeners,l3_quarantine,l4_statusquo){
  
  return(data.frame(
    "condition"=c("l1_bubble","l2_screeners","l3_quarantine","l4_statusquo"),
    "travel_rate"=c(l1_bubble,l2_screeners,l3_quarantine,l4_statusquo)
  ))
}

#wraps get_analysis_covid_data
#breaks out locations into different risk categories and summarizes depending on their risk level.
#we don't need this yet, though.
# get_differential_analysis_covid_data <- function(
#   world_with_covid_data,
#   main_country_filter,
#   countries_bubble,
#   countries_screener,
#   countries_quarantine,
#   general_travel_rate){
#   if(length(general_travel_rate)==1){
#     travel_rate <- set_general_travel_rate(general_travel_rate,general_travel_rate,general_travel_rate,general_travel_rate)
#   }else if(nrow(general_travel_rate)==4){
#     #4 levels are:
#     #1. within our bubble (no controls at all on country)
#     #2. allow travelers in with screener
#     #3. allow travelers in with 14 day quarantine
#     #4. allow only NZ resident travelers (status quo)
#     travel_rate <- general_travel_rate
#   }else{
#     stop("general_travel_rate must be either a single value or a data frame of length 4")
#   }
#   
#   #bubble
#   get_analysis_covid_data(world_with_covid_data %>% )
#   #screener
#   
#   #quarantine
#   #status quo
# }

get_analysis_covid_data <- function(
  world_with_covid_data,
  quarantine_odds_override=NULL,
  general_travel_rate=1,
  assumed_ifr=0.005){
  

  #assumed_cfr<-0.005
  world_with_covid_data<- 
    world_with_covid_data %>% 
    mutate(InferredDetectionRate = (assumed_ifr*LaggedNewCases/NewDeaths))
  
  #if there are NO deaths then we infer detection rate is 100%
  world_with_covid_data <- 
    world_with_covid_data %>% 
    mutate(InferredDetectionRate = ifelse(NewDeaths==0,1,InferredDetectionRate)
    )
  
  world_with_covid_data <-
    world_with_covid_data %>% 
    mutate(InferredActiveCases= (ActiveCases/InferredDetectionRate))
  
  
  world_with_covid_data <- 
    world_with_covid_data %>% 
    mutate(InferredActiveCasePopRate = (InferredActiveCases/Population))
  
  world_with_covid_data <- 
    world_with_covid_data %>% 
    mutate(ActiveCasePopRate = ActiveCases/Population)
  
  
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
        NZResMonthlyArrivalsLatest + pmax(0,NZResMonthlyArrivalsScaled1-NZResMonthlyArrivalsLatest)*general_travel_rate
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
  
  
  
  ######### QUARANTINE TREATMENT
  ### EXCLUDES the dampening effect of quarantine on travel - this is taken into account above.
  
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
  
  
  ######### SCREENING TREATMENT
  ### EXCLUDES the dampening effect of quarantine on travel - this is taken into account above.
  screening_sensitivity <- 0.7
  screening_specificity <- 0.999
  #I think we need negative predictive value as well as positive predictive value.
  #https://en.wikipedia.org/wiki/Positive_and_negative_predictive_values#Positive_predictive_value
  #https://en.wikipedia.org/wiki/Positive_and_negative_predictive_values#Negative_predictive_value
  prevalence <- world_with_covid_data$InferredActiveCasePopRate
  #this is the "detection rate" or how many true positives we catch for the total number of calls
  positive_predictive_value <- (
    (screening_sensitivity * (prevalence))/
      (screening_sensitivity*screening_specificity + (1-screening_specificity)*(1-prevalence))
  )
  #this is the the proportion of true negatives we catch out of the total number of negatives
  negative_predictive_value <- (
    (screening_specificity * (1-prevalence))/
      ((1-screening_sensitivity)*prevalence+screening_specificity*(1-prevalence))
  )
  #so then we can:
  #(a) from an expected number of true positive cases arriving (ExpectedNumberOfCasesAll), calculate the number we'll catch by
  #     number of positive test results = N(true positives)/PPV
  #(b) we can get the False Omission Rate which is just the complement of NPV (FOR = 1-NPV) which is the rate of 
  
  #or are we really just interested in SENSITIVITY for now?
  #sensitivity tells us how much we'll correctly identify those with the disease - what proportion of positive patients we'll catch
  #so this is actually a lot easier than all the above...
  world_with_covid_data <- 
    world_with_covid_data %>% mutate(
      ExpectedNumberOfCasesEscapingOneScreen=ExpectedNumberOfCasesAll*(1-screening_sensitivity))
  world_with_covid_data <- 
    world_with_covid_data %>% mutate(
      ExpectedNumberOfCasesEscapingTwoScreens=ExpectedNumberOfCasesAll*(1-screening_sensitivity)^2)
      
    
  
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