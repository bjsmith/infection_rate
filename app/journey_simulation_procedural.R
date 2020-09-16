#The previous journey simulation made the unit of analysis categories of cases
#that got difficult because they divide into so many paths
#this journey simulator generates cases over distributions but each case is considered individually.
#this means that we can model more complex interactions between events.



library(data.table)

verbose <- FALSE

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
  #temp_and_symptoms_test_to_avoid_boarding = c(0),
  p_flight_infection_risk_per_case_contact = 0.005*.15, #with mask wearing
  quarantine_contacts_per_day=2,
  set_density_at_1_per_day = FALSE
  ){
  warning("flight risk variable ignored.")
  #let's start a list of all cases started in the prior 14 days from day 0
  # case_list_day_begin = c()
  # case_list_weight = c()

  #all these should start from day 0 the day of infection
  #not infectious on 0th day of infection.
  
  p_pcr_detectable_by_day <- c(0,0, 0,5, 40, 65, 75, 80, 85, 82, 80, 78, 76, 74, 72, 70, 68, 66, 64, 62, 60, 58,rep(0,20))/100
  p_symptomatic_by_day <- plnorm(0:40, 1.621,0.418)
  p_ever_symptomatic <- 0.6
  p_becomes_symptomatic_by_day <- p_symptomatic_by_day-lag(p_symptomatic_by_day,default=0)
  
  #should really be changed now but we'll keep consistent with what's written in the paper.
  p_infection_remains_infectious_by_day <- c(1,rep(1,14),0.75,0.5,0.25,rep(0,30))
  p_infectiousness_stops <- lag(p_infection_remains_infectious_by_day,default=1)-p_infection_remains_infectious_by_day
  
  sim_days_before_flight_start = 20
  
  days_to_measure <- sim_days_before_flight_start+quarantine_release_day
  community_exposure_by_day <- vector("numeric",days_to_measure)
  #proportional_infections_by_day <- vector("numeric",days_to_measure)
  
  
  
  
  cases_n=100000
  
  cases_df <- data.table(
    "start_day_rtf" = vector(mode="numeric",length=cases_n),
    "symptomatic_day_rtf" = vector(mode="numeric",length=cases_n),
    "detected_day_rtf" = as.numeric(rep(NA,cases_n)),
    "infectious_cease_rtf" = vector(mode="logical",length=cases_n)
    )
  cases_n_revised<-cases_n #running total of cases in case it changes.

  sample_case_symptomaticity_delay <- function(n=1){
    #mark symptomaticity from the distribution
    symptomaticity_raw<-sample(1:length(p_becomes_symptomatic_by_day),n,prob=p_becomes_symptomatic_by_day,replace=TRUE)
    #40% are just asymptomatic
    symptomaticity_raw[sample(1:n,ceiling((1-p_ever_symptomatic)*n),replace=FALSE)]<-length(p_becomes_symptomatic_by_day)
    return(symptomaticity_raw)
  }
  sample_infectiousness_stops <-function(n=1){
    sample(1:length(p_infectiousness_stops),n,prob=p_infectiousness_stops,replace=TRUE) 
  }
  #generate a case
  
  #case starts along the distribution set by p_infection_remains_infectious_by_day/sum(p_infection_remains_infectious_by_day)
  case_start_before_flight_day <-sample(0-1:length(p_infection_remains_infectious_by_day),cases_n,prob=p_infection_remains_infectious_by_day,replace=TRUE)
  
  cases_df[,start_day_rtf:=case_start_before_flight_day]
  cases_df[,symptomatic_day_rtf:=case_start_before_flight_day+sample_case_symptomaticity_delay(cases_n)]
  cases_df[,infectious_cease_rtf:=sample_infectiousness_stops(cases_n)+case_start_before_flight_day]
  
  sim_days_before_flight_start = 20
  
  
  # timer1_cumulative<-0
  # timer2_cumulative<-0
  # timer1_start<-proc.time()[3]
  # timer1_cumulative<- timer1_cumulative + (proc.time()[3]-timer1_start)
  # timer1_cumulative
  
  #pre-departure test.
  #cycle through each pre-departure test
  for (pre_departure_test in pcr_test_list_to_avoid_boarding){
    
    #choose the day for the test from the range allowed, for each subject in the test
    day_for_pd_test<- sample(unlist(pre_departure_test),cases_n,replace = TRUE)
    
    
    #calculate the days from the infection to the test for every subject who hasn't been tested
    days_from_infection_to_test <- day_for_pd_test - cases_df[is.na(detected_day_rtf),start_day_rtf]
    
    #mark the infections occurring BEFORE the test
    infection_starts_before_test<-days_from_infection_to_test>0
    #randomize success of each test
    success_rand<-runif(sum(infection_starts_before_test))
    #get the odds of detection for each case
    odds_of_detection <- p_pcr_detectable_by_day[days_from_infection_to_test[infection_starts_before_test]]
    #work out if we 'beat the odds'
    test_success <- odds_of_detection>success_rand
    
    #where infection started before the test
    #and where the test was successful
    #mark the date of the test in the "detected_day_rtf" column to identify a successful test.
    cases_df[is.na(detected_day_rtf),
    ][which(infection_starts_before_test),
    ][test_success,]$detected_day_rtf <- day_for_pd_test[infection_starts_before_test][test_success]+1 #add one, result takes a day to come through.
    
  }

  
  cases_infectious_on_flight <- cases_df[,is.na(detected_day_rtf)] & (cases_df[,infectious_cease_rtf]>0)
  #how many cases begin on the flight?
  #let's not make this probabilistic; let's just multiply the probability by the number of cases...
  #the only probabilistic use this would have would be to model across low, medium, high scenarios and we are not currently doing that.
  cases_begun_on_flight<-ceiling(sum(cases_infectious_on_flight)* 0.43/100)
  #cases_begun_on_flight <- rbinom(n=1,size=sum(cases_infectious_on_flight),prob = 0.43/100)
  
  #now add any cases that come from in-flight spread.
  append_new_cases <- function(cases_df,cases_to_append,cases_start_day_rtf=0){
    #cases_n_revised <- nrow(cases_df)+cases_to_append
    new_cases <-data.table(
      cases_start_day_rtf,
      sample_case_symptomaticity_delay(cases_to_append)+cases_start_day_rtf, #cases_symptomatic_day_rtf
      rep(as.integer(NA), cases_to_append),
      sample_infectiousness_stops(cases_to_append)+cases_start_day_rtf)
    names(new_cases)<-names(cases_df)

    cases_df <-rbind(cases_df,new_cases)#bind it.
    return(cases_df)
  }
  cases_df<-append_new_cases(cases_df,cases_begun_on_flight)

  #now we work out what happens on arrival.
  #on each day, there is:
  # - spread risk
  # - breach risk
  # - and there are PCR tests on specific days that will filter out some cases.
  
  breach_odds_per_day <- exp(mean(c(log(1/34),log(12/9000))))/14
  spread_risk_per_case_per_day <- 0.022*(quarantine_contacts_per_day/5)/14
  #14 because that's the number of days; then 5 contacts per day is default.
  #I think we need to now do a sequential loop because we have to calculate odds of a number of things on each day.
  for(t in 0:quarantine_release_day){
    printv(paste0("day ",as.character(t)))
    is_infectious_today<-cases_df[,infectious_cease_rtf]>t
    is_symptomatic_today<-t > cases_df[,symptomatic_day_rtf]
    
    #for symptomatic patients
    is_undetected <- is.na(cases_df$detected_day_rtf)
    health_check_odds<-1/3
    #calculate health check.
    printv("doing health check")
    printv("symptomatic today:")
    health_check_test_success<-health_check_odds>runif(sum(is_symptomatic_today&is_undetected))#just for symptomatic patients.
    printv("health check success:")
    printv(sum(health_check_test_success))
    cases_df[is_symptomatic_today & is_undetected,][which(health_check_test_success),]$detected_day_rtf<-t
    #now mark these as detected...
    
    #calculate breach risk on this day
    community_exposure_by_day[t+sim_days_before_flight_start]<-community_exposure_by_day[t+sim_days_before_flight_start] + breach_odds_per_day*sum(is_infectious_today)
    
    #calculate anything else on this day
    #if PCR test occurs on this day, then run it.
    if(t %in% pcr_test_to_remain_in_quarantine){
      
      printv("doing PCR test to hold patients in quarantine...")
      
      #calculate the days from the infection to the test for every subject who hasn't been tested
      days_from_infection_to_test <- t - cases_df[is.na(detected_day_rtf),start_day_rtf]
      
      #mark the infections occurring BEFORE the test
      infection_starts_before_test<-days_from_infection_to_test>0
      #randomize success of each test
      success_rand<-runif(sum(infection_starts_before_test))
      #get the odds of detection for each case
      odds_of_detection <- p_pcr_detectable_by_day[days_from_infection_to_test[infection_starts_before_test]]
      #work out if we 'beat the odds'
      test_success <- odds_of_detection>success_rand
      
      #where infection started before the test
      #and where the test was successful
      #mark the date of the test in the "detected_day_rtf" column to identify a successful test.
      cases_df[is.na(detected_day_rtf),
               ][which(infection_starts_before_test),
                 ][test_success,]$detected_day_rtf <- t+1 #add one because PCR test results take a day to come through.
      
    }
    
    #calculate the spread risk on this day
    n_infectious_cases_day_t<-sum(is_infectious_today)
    new_cases <- rbinom(1,n_infectious_cases_day_t,spread_risk_per_case_per_day)
    #spread risk will be the number of currently infectious cases.
    cases_df<-append_new_cases(cases_df,new_cases,t)
    
  }
  community_exposure_by_day[t+sim_days_before_flight_start]<- (
    community_exposure_by_day[t+sim_days_before_flight_start] +sum(is.na(cases_df$detected_day_rtf)))
  
  
  table(is.na(cases_df$detected_day_rtf))
  table(cases_df$detected_day_rtf)

  #TO DO: WORK OUT HOW TO RETURN WHAT WE DID HERE.
  cases_aggregate_wide<-cases_df[,.(cases=.N),.(start_day_rtf,detected=ifelse(!is.na(detected_day_rtf),"detected","undetected"))]
  cases_aggregate<- cases_aggregate_wide%>% dcast(start_day_rtf~detected,value.var="cases")
  colnames(cases_aggregate)[1] <- c("day_of_infection")
  cases_aggregate[,p_remains_infectious_and_in_pipeline:=undetected/cases_n]
  
  #how to get data_by_day?
  #let's do it procedurally first. So Day -6...
  #is the number of infections which...
  #...(a) started on or before that day...
  #...(b) finished after that day...
  #...(c) were not detected at all or detected after that day
  
  detected_by_day_x <- function(detected_day,day_x){
    detected_day[is.na(detected_day)]<-day_x+1 #dirty trick to ensure that the result of evaluating this is false detected day is NA.
    return(detected_day<=day_x)
  }
  data_by_day_days<-(-sim_days_before_flight_start+1):quarantine_release_day
  p_infectious_in_pipeline_by_day <- sapply(data_by_day_days,
         function(day){
           cases_df[(start_day_rtf<=day) & (day<infectious_cease_rtf) & (detected_by_day_x(detected_day_rtf,day)==FALSE),.N]
         })/cases_n
  
  
  return(list(
    "data_by_infection_source" = cases_aggregate,
    "data_by_day" = data.frame(
      "days_past_flight"= data_by_day_days,
      "p_community_exposure_by_day" = community_exposure_by_day/cases_n,
      #"proportional_infections_by_day" = proportional_infections_by_day,
      "p_infectious_in_pipeline_by_day" = p_infectious_in_pipeline_by_day
      ),
    "total_community_exposure_by_day" = sum(community_exposure_by_day)/cases_n
    )
  )
}
