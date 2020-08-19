#let's start with a 5 night quarantine intervention.

#how do we calculate the probability that a random case will get into the community?

#probability of a case being infectious on release
#probability of a case infecting another person in quarantine
#probability of a case infecting another person on the aeroplane






run_sim <- function(
  pcr_test_list_to_avoid_boarding = list(list(-2,-3)),
  pcr_test_to_remain_in_quarantine = c(5),
  quarantine_release_day = 6,
  temp_and_symptoms_test_to_avoid_boarding = c(0),
  p_flight_infection_risk_per_case_contact = 0.005*.15 #with mask wearing
  ){
  #let's start a list of all cases started in the prior 14 days from day 0
  case_list_day_begin = c()
  case_list_weight = c()

  #all these should start from day 0 the day of infection
  #not infectious on 0th day of infection.
  
  p_pcr_detectable_by_day <- c(0,0, 0,5, 40, 65, 75, 80, 85, 82, 80, 78, 76, 74, 72, 70, 68, 66, 64, 62, 60, 58,rep(0,20))/100
  #we really need a probability distribution over that 0.4 but we're not at that level of sophistication just yet...
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
  community_exposure <- vector("numeric",days_to_measure)
  

  #duration_to_flight
  #spawn one case from t-13 onward to t=0
  for (t in 0:days_to_measure){
    # by convention t will be 20 days before flight.
    duration_to_flight <- t-sim_days_before_flight_start
    
    #flight occurs on day 14 then
    
    cat("\n\n")
    print(paste("days after flight:",as.character(duration_to_flight)))
    
    
    #create one infection per day constant, if we're pre-flight
    if(duration_to_flight<=0){
      case_list_day_begin[length(case_list_day_begin) + 1] <- t
      case_list_weight[length(case_list_weight) + 1] <- 1/sum(p_infection_remains_infectious_by_day)
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
      print("flying")
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
      infectiousness_per_case = 0.0022/14/2 #Steyn figure, crudely rendered.
      expected_infections_today <-infectious_cases_in_environment*infectiousness_per_case
      #I want to add an extra value to this series rather than 
      #augment the one already on there
      #because strictly speaking each item in the case_list series is one case,
      #weighted down by case_list_weight
      #not necessarily a days worth of cases
      case_list_day_begin[length(case_list_day_begin) + 1] <- t
      case_list_weight[length(case_list_weight) + 1] <- expected_infections_today
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
      community_exposure[t]=+expected_quarantine_breaches_today
      
    }
    #then the next day, we're gonna let the travelers out.
    if(duration_to_flight==quarantine_release_day){
      cat("releasing all travelers who haven't been tested positive.")
      cat(paste("There are",current_infectious_cases_sum,"cases being released."))
      
      #this is the probability that each case is detected and removed
      #we now adjust our weights
      community_exposure[t]=+current_infectious_cases_sum
      cat("cases detected are removed and results shown on next day")
    }

    

    
    #PCR tests
    for (pcr_test_set_to_avoid_boarding in pcr_test_list_to_avoid_boarding){
      #pcr_test_set_to_avoid_boarding<-pcr_test_list_to_avoid_boarding[[1]]
      #we have a specific set of tests
      #how many tests are there?
      test_possibilities <- length(pcr_test_set_to_avoid_boarding)
      
      #now probabilistically a PCR test on this day, the result of which will be to avoid boarding.
      if(duration_to_flight %in% pcr_test_set_to_avoid_boarding){
        cat("doing PCR test...")
        p_case_list_detected <- p_pcr_detectable_by_day[case_list_days_since_case+1]*(1/test_possibilities)
        #weighted to account for the probabiliyt of actually running the test
        #this is the probability that each case is detected and removed
        #we now adjust our weights
        case_list_weight <- case_list_weight*(1-p_case_list_detected)
        cat("cases detected are removed and results shown on next day")
        
      }
    }
    
    #now we do a temperature test if there is one on this day.
    if(duration_to_flight %in% temp_and_symptoms_test_to_avoid_boarding){
      cat("doing temperature and symptom screening...")
      p_case_list_detected <- p_symptomatic_by_day[case_list_days_since_case+1]
      #this is the probability that each case is detected and removed
      #we now adjust our weights
      case_list_weight <- case_list_weight*(1-p_case_list_detected)
      cat("cases detected are removed and results shown on next day")
    }
    
    #now we do a PCR test within the quarantine if there is one on this day.
    if(duration_to_flight %in% pcr_test_to_remain_in_quarantine){
      cat("doing PCR test to hold patients in quarantine...")
      p_case_list_detected <- p_pcr_detectable_by_day[case_list_days_since_case+1]
      #this is the probability that each case is detected and removed
      #we now adjust our weights
      case_list_weight <- case_list_weight*(1-p_case_list_detected)
      cat("cases detected are removed and results shown on next day")
    }
    
      

    #now look at hte odds cases remain infectious and undetected
    if(length(case_list_day_begin)>0){
      print("Case beginning by case:")
      print(round(case_list_day_begin,3))
      print("Case infectiousness by case:")
      print(round(current_infectious_cases,3))
      print("total infectious cases in the journey at the moment:")
      print(current_infectious_cases_sum)
      print("expected community exposure today:")
      print(community_exposure[t])
      # print("log community exposure, by day, to date:")
      # print(log(community_exposure))
      
    }

    #the below would be useful to visualize infectiousness from each day.    
    # data.frame("day_of_infection" = case_list_day_begin-sim_days_before_flight_start,
    #            "p_remains_in_pipeline" = case_list_weight,
    #            "p_remains_infectious" = p_case_remains_infectious)
    
  }
  
  return(sum(community_exposure))
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



p_expected_cases_in_community <- run_sim()
p_expected_cases_in_community_no_temp_check <- run_sim(temp_and_symptoms_test_to_avoid_boarding=c())




