#let's start with a 5 night quarantine intervention.

#how do we calculate the probability that a random case will get into the community?

#probability of a case being infectious on release
#probability of a case infecting another person in quarantine
#probability of a case infecting another person on the aeroplane


verbose <- TRUE

printv <-function(to_print){
  if(verbose){
    cat(to_print)
    cat("\n")
  }
}

run_sim <- function(
  pcr_test_list_to_avoid_boarding = list(list(-2,-3)),
  pcr_test_to_remain_in_quarantine = c(5),
  quarantine_release_day = 6,
  temp_and_symptoms_test_to_avoid_boarding = c(0),
  p_flight_infection_risk_per_case_contact = 0.005*.15, #with mask wearing
  quarantine_contacts_per_day=2,
  set_density_at_1_per_day = FALSE
  ){
  #let's start a list of all cases started in the prior 14 days from day 0
  case_list_day_begin = c()
  case_list_weight = c()

  #all these should start from day 0 the day of infection
  #not infectious on 0th day of infection.
  
  p_pcr_detectable_by_day <- c(0,0, 0,5, 40, 65, 75, 80, 85, 82, 80, 78, 76, 74, 72, 70, 68, 66, 64, 62, 60, 58,rep(0,20))/100
  p_symptomatic_by_day <- plnorm(0:40, 1.621,0.418)*0.4
  p_infection_remains_infectious_by_day <<- c(0,rep(1,14),0.75,0.5,0.25,rep(0,30))
  get_p_case_remains_infectious <- function(case_list_weight,case_list_day_begin,t){
    
    #now calculate the odds that each current case remains infectious
    if(length(case_list_weight)>0){
      p_case_remains_infectious = vector("numeric",length(case_list_weight))
      for (case_n in 1:length(case_list_weight)){
        #case_n<-1
        case_n_days_ago = t-case_list_day_begin[case_n]
        p_case_remains_infectious[case_n] = p_infection_remains_infectious_by_day[case_n_days_ago+1]
      }
    }
    return(p_case_remains_infectious)
  }
  
  
  sim_days_before_flight_start = 20
  
  days_to_measure <- sim_days_before_flight_start+quarantine_release_day
  community_exposure_by_day <- vector("numeric",days_to_measure)
  proportional_infections_by_day <- vector("numeric",days_to_measure)
  p_infectious_in_pipeline_by_day <- vector("numeric",days_to_measure)
  

  #duration_to_flight
  #spawn one case from t-13 onward to t=0
  for (t in 1:days_to_measure){
    # by convention t will be 20 days before flight.
    duration_to_flight <- t-sim_days_before_flight_start
    
    #flight occurs on day 14 then
    
    printv("\n\n")
    printv(paste("days after flight:",as.character(duration_to_flight)))
    
    
    #create one infection per day constant, if we're pre-flight
    
    if(duration_to_flight==0){ #include equal infections of day of flight
      warning("excluded day of flight from infections. No special accounting for airport risk occurs.")
      
    }
    if(duration_to_flight<0){ #exclude day of flight
    #if(duration_to_flight<=0){ #include day of flight
      case_list_day_begin[length(case_list_day_begin) + 1] <- t
      if(set_density_at_1_per_day){ #just for debugging
        proportional_infections_by_day[t] <- 1
      }else{
        proportional_infections_by_day[t] <- 1/sum(p_infection_remains_infectious_by_day)
        #we add cases using this very specific and odd figure so that we're scaling to 1 infectious case over the whole period
        #that way we can talk about percentage of infectious cases
        #I think we need to think about this a little bit more....
        #was easier when it was a uniform distribution.
      }
      case_list_weight[length(case_list_weight) + 1] <- proportional_infections_by_day[t]
      
        #this weight should really be divided by the sum of our "remains infectious" value
        #but we'll leave that to start.
    }
    
    #mark the cases by the days since they occured
    case_list_days_since_case <- t - case_list_day_begin
    

    p_case_remains_infectious <- get_p_case_remains_infectious(case_list_weight,case_list_day_begin,t)
    
    #at t=0 there is flight spread risk.
    #each case, proportionally to its current infectiousness, can result in another case being generated
    #that begins on the day of flight.
    if(duration_to_flight==0){
      # case_list_weight[t] <- case_list_weight[t]
      # probably will deprecate this at some point when we go probabilistic
      # for now it's useful to count the probability
      # probably will deprecate this at some point when we go probabilistic
      # case WEIGHT (the odds this is still in pipeline) is important
      # case INFECTIOUSNESS is also important)
      printv("flying")
      num_contacts_on_flight=16
      infectious_cases_on_flight <- sum(case_list_weight*p_case_remains_infectious)
      #probability of infection per case 
      infectiousness_on_flight_per_case = p_flight_infection_risk_per_case_contact*num_contacts_on_flight
      #I want to add an extra value to this series rather than 
      #augment the one already on there
      #because strictly speaking each item in the case_list series is one case,
      #weighted down by case_list_weight
      #not necessarily a days worth of cases
      case_list_day_begin[length(case_list_day_begin) + 1] <- t
      proportional_infections_by_day[t] <- infectiousness_on_flight_per_case
      case_list_weight[length(case_list_weight) + 1] <- infectiousness_on_flight_per_case
    }
    
    #OK great, we've got to the border
    #now we want to know the probability of infection within quarantine each day
    if(duration_to_flight>0){
      # case WEIGHT (the odds this is still in pipeline) is important
      # case INFECTIOUSNESS is also important)
      #num_contacts_per_case_per_day=2.5
      infectious_cases_in_environment <- sum(case_list_weight*p_case_remains_infectious)
      #probability of infection per case 
      infectiousness_per_contact = 0.0036 
        #calibrated to produce a roughly 0.02% difference in success when moving from 5 contacts a day to 0
      # as in Steyn, Binny, Hendy
      
      expected_infections_today <-infectious_cases_in_environment*infectiousness_per_contact*quarantine_contacts_per_day
      #I want to add an extra value to this series rather than 
      #augment the one already on there
      #because strictly speaking each item in the case_list series is one case,
      #weighted down by case_list_weight
      #not necessarily a days worth of cases
      case_list_day_begin[length(case_list_day_begin) + 1] <- t
      case_list_weight[length(case_list_weight) + 1] <- expected_infections_today
      proportional_infections_by_day[t] <- expected_infections_today
    }
    
    #mark the cases by the days since they occured again
    case_list_days_since_case <- t - case_list_day_begin
    
    
    p_case_remains_infectious <- get_p_case_remains_infectious(case_list_weight,case_list_day_begin,t)
    
    current_infectious_cases <- case_list_weight*p_case_remains_infectious
    current_infectious_cases_sum <- sum(current_infectious_cases)
    
    if(duration_to_flight>0){
      #OK. What's the odds of community exposure?
      #breach odds per case should be something like 
      #breach odds are calculated from
      breach_odds_per_day_with_30_cases <- exp(mean(c(log(1/30),log(12/9000))))
      breach_odds_per_case <- breach_odds_per_day_with_30_cases/30
      #this is the odds of any individual breaking out on any day
      #we need the current 
      expected_quarantine_breaches_today <- current_infectious_cases_sum*breach_odds_per_case
      community_exposure_by_day[t]=+expected_quarantine_breaches_today
      
    }
    #then the next day, we're gonna let the travelers out.
    if(duration_to_flight==quarantine_release_day){
      printv("releasing all travelers who haven't been tested positive.")
      printv(paste("There are",current_infectious_cases_sum,"cases being released."))
      
      #this is the probability that each case is detected and removed
      #we now adjust our weights
      community_exposure_by_day[t]=+current_infectious_cases_sum
      printv("cases detected are removed and results shown on next day")
    }

    

    
    #PCR tests
    for (pcr_test_set_to_avoid_boarding in pcr_test_list_to_avoid_boarding){
      #pcr_test_set_to_avoid_boarding<-pcr_test_list_to_avoid_boarding[[1]]
      #we have a specific set of tests
      #how many tests are there?
      test_possibilities <- length(pcr_test_set_to_avoid_boarding)
      
      test_set_max_day <- max(unlist(pcr_test_set_to_avoid_boarding))
      #what we want to do is apply equally to the case list weight based on the LAST day
      if(duration_to_flight==test_set_max_day){
        printv("doing PCR test...")
        #if we're on the last day then iterate through each of the days to build a combined probability of 
        #either test detecting cases
        #we assume that each test has an equal probability of being run
        test_set_relative_to_now <- unlist(pcr_test_set_to_avoid_boarding) - test_set_max_day
        test_set_combined_prob <- rep(0,length(case_list_days_since_case))
        for (test_relative in test_set_relative_to_now){
          test_day_prob <- c(p_pcr_detectable_by_day[case_list_days_since_case+1+test_relative],rep(0,-test_relative))
          test_set_combined_prob= test_set_combined_prob + test_day_prob/test_possibilities
        }
        p_case_list_detected <- test_set_combined_prob#p_pcr_detectable_by_day[case_list_days_since_case+1]*(1/test_possibilities)
        printv(p_case_list_detected)
        # printv("\n")
        cases_detected_by_tests <- case_list_weight*p_case_list_detected
        case_list_weight <- case_list_weight - cases_detected_by_tests
        #so the cases detected are the reciprocal of this????
        print("cases detected are removed and results shown on next day")
      }
      
      #now probabilistically a PCR test on this day, the result of which will be to avoid boarding.
      # if(duration_to_flight %in% pcr_test_set_to_avoid_boarding){
      #   print("doing PCR test...")
      #   warning("can't calculate PCR test this way because you're effectively multiplying the two probabilities together. They need to be added or averaged instead.")
      #   # better way is to find the LAST day in the test set
      #   # then run it on there but stagger the probability detections and work on that basis.
      #   p_case_list_detected <- p_pcr_detectable_by_day[case_list_days_since_case+1]*(1/test_possibilities)
      #   print(p_case_list_detected)
      #   print(t)
      #   print(duration_to_flight)
      #   #weighted to account for the probabiliyt of actually running the test
      #   #this is the probability that each case is detected and removed
      #   #we now adjust our weights
      #   case_list_weight <- case_list_weight*(1-p_case_list_detected)
      #   print("cases detected are removed and results shown on next day")
      #   
      # }
    }
    
    #now we do a temperature test if there is one on this day.
    if(duration_to_flight %in% temp_and_symptoms_test_to_avoid_boarding){
      printv("doing temperature and symptom screening...")
      p_case_list_detected <- p_symptomatic_by_day[case_list_days_since_case+1]
      #this is the probability that each case is detected and removed
      #we now adjust our weights
      case_list_weight <- case_list_weight*(1-p_case_list_detected)
      printv("cases detected are removed and results shown on next day")
    }
    
    #now we do a PCR test within the quarantine if there is one on this day.
    if(duration_to_flight %in% pcr_test_to_remain_in_quarantine){
      printv("doing PCR test to hold patients in quarantine...")
      p_case_list_detected <- p_pcr_detectable_by_day[case_list_days_since_case+1]
      #this is the probability that each case is detected and removed
      #we now adjust our weights
      case_list_weight <- case_list_weight*(1-p_case_list_detected)
      printv("cases detected are removed and results shown on next day")
    }else if (duration_to_flight>0){
      warning("not counting daily health checks. But to do that we need to keep track of a specific subpopulation of the infected population who are symptomatic")
      printv("doing managed isolation health check...")
      #we remove 1/3 of the symptomatic patients from both the symptomatic pool and the total pool
      #p_case_list_detected <- p_symptomatic_by_day[case_list_days_since_case+1]*1/3
      #we'd need a "cases by day symptomatic" here.
      #cases_by_day_symptomatic[case_list_days_since_case+1] = p_symptomatic_by_day[case_list_days_since_case+1]- p_case_list_detected
      #round(p_case_list_detected,2)
      round(p_symptomatic_by_day,2)
      # #if there is no PCR on a given day, 
      # #then patients who are symptomatic have 33% odds of being detected during an interview
      # #I don't think we can do this because we haven't actually modeled separate populations
      # #of people who are symptomatic and asymptomatic
      # #we would need to actually store a list of asymptomatic patients
      # p_case_list_detected <- p_symptomatic_by_day[case_list_days_since_case+1]*1/3
      # case_list_weight <- case_list_weight*(1-p_case_list_detected)
      
    }
    
    
    
      

    #now look at hte odds cases remain infectious and undetected
    if(length(case_list_day_begin)>0){
      printv("Case beginning by case:")
      printv(round(case_list_day_begin,3))
      printv("Case infectiousness by case:")
      printv(round(current_infectious_cases,3))
      printv("total infectious cases in the journey at the moment:")
      printv(current_infectious_cases_sum)
      printv("expected community exposure today:")
      printv(community_exposure_by_day[t])
      # print("log community exposure, by day, to date:")
      # print(log(community_exposure_by_day))
      
    }

    #the below would be useful to visualize infectiousness from each day.    
    # data.frame("day_of_infection" = case_list_day_begin-sim_days_before_flight_start,
    #            "p_remains_in_pipeline" = case_list_weight,
    #            "p_remains_infectious" = p_case_remains_infectious)
    
    p_infectious_in_pipeline_by_day[t] <- sum(current_infectious_cases)
    
  }
  
  return(list(
    "data_by_infection_source" = data.frame(
      "day_of_infection" = case_list_day_begin-sim_days_before_flight_start,
      "cases_undetected" = case_list_weight,
      "p_case_remains_infectious"=p_case_remains_infectious,
      "p_remains_infectious_and_in_pipeline" = current_infectious_cases),
    "data_by_day" = data.frame(
      "days_past_flight"= 1:days_to_measure - sim_days_before_flight_start,
      "p_community_exposure_by_day" = community_exposure_by_day,
      "proportional_infections_by_day" = proportional_infections_by_day,
      "p_infectious_in_pipeline_by_day" = p_infectious_in_pipeline_by_day
      ),
    "total_community_exposure_by_day" = sum(community_exposure_by_day)
    )
  )
}


library(ggplot2)
# df_result <- run_sim()
# df_result$simulation <- c("with temp check")
# df_result2 <- run_sim(temp_and_symptoms_test_to_avoid_boarding=c())
# df_result2$simulation <- c("no temp check")
# ggplot(rbind(df_result,df_result2),
#        aes(x=day_of_infection,y=p_remains_in_pipeline*p_remains_infectious,
#            group=simulation,color=simulation))+geom_line()+
#   labs(y="P(risk)",
#        title="Probability density of case risk over the period by day of original infection")
# #later we may incorporate prevalence into this
# #and apply interventions along the way


# 
# ####MATCHING STEYN, BINNY, HENDY
# #14 day quarantine
# verbose=TRUE
# quarantine_length <- 14
# sim_result <- run_sim(
#   pcr_test_list_to_avoid_boarding = list(list(-2,-3)),
#   pcr_test_to_remain_in_quarantine = c(3,12),
#   quarantine_release_day = quarantine_length,
#   temp_and_symptoms_test_to_avoid_boarding = c(0),
#   p_flight_infection_risk_per_case_contact=0 #EXCLUDE flight risk.
# )
# ggplot(sim_result$data_by_infection_source,aes(x=day_of_infection,y=p_remains_infectious_and_in_pipeline))+geom_point()
# 
# sim_result <- run_sim(
#   pcr_test_list_to_avoid_boarding = list(list(-2,-3)),
#   pcr_test_to_remain_in_quarantine = c(3),
#   quarantine_release_day = quarantine_length,
#   temp_and_symptoms_test_to_avoid_boarding = c(0),
#   p_flight_infection_risk_per_case_contact=0 #EXCLUDE flight risk.
# )
# ggplot(sim_result$data_by_infection_source,aes(x=day_of_infection,y=p_remains_infectious_and_in_pipeline))+geom_point()
# 

#now, this includes:
# inherent risk
# cabin exposure
# breaches (escapees and exceptions)
# spread to other guests during quarantine


#Does NOT include:
# - spread to workers during quarantine

