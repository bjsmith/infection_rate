source("misc/monte_carlo.R")

##### MATCHING MY OWN DATASET
verbose=FALSE
#overnight quarantine with temp check
res<-run_sim(
  pcr_test_list_to_avoid_boarding = list(list(-2)),
  pcr_test_to_remain_in_quarantine = c(0),
  quarantine_release_day = 1,
  quarantine_contacts_per_day = 5,
  p_flight_infection_risk_per_case_contact = 0.005*.15, #with mask wearing
  temp_and_symptoms_test_to_avoid_boarding=c(0)
)
res #efficacy
#THIS CHECKS OUT !!! :-) 
#THEN WE CAN APPLY FURTHER REFINEMENT

res1<-run_sim(
  pcr_test_list_to_avoid_boarding = list(list(-2,-3)),
  pcr_test_to_remain_in_quarantine = c(3,12),
  quarantine_release_day = 14,
  quarantine_contacts_per_day=5,
  p_flight_infection_risk_per_case_contact = 0.005*.15, #with mask wearing
  temp_and_symptoms_test_to_avoid_boarding=c(0)
)
res1


res2<-run_sim(
  pcr_test_list_to_avoid_boarding = list(list(-2,-3)),
  pcr_test_to_remain_in_quarantine = c(3,12),
  quarantine_release_day = 14,
  quarantine_contacts_per_day=0,
  p_flight_infection_risk_per_case_contact = 0.005*.15, #with mask wearing
  temp_and_symptoms_test_to_avoid_boarding=c(0)
)
res1$total_community_exposure-res2$total_community_exposure
res2 
  #why is this still 1.5?
  #seems to be almost all from risk accured in the last two days - so this is because of our function
  #that assumes some residual risk left after 14 days
  #Steyn, Binny, Hendy have assumed all that away; they assumed that all cases will be detected
  #so they get the much lower figure of 0.1%

# verbose=FALSE
# #overnight quarantine with temp check
# res<-run_sim(
#   pcr_test_list_to_avoid_boarding = list(list(-3)),
#   pcr_test_to_remain_in_quarantine = c(0),
#   quarantine_release_day = 1,
#   temp_and_symptoms_test_to_avoid_boarding = c(0)#,
#   #p_flight_infection_risk_per_case_contact = 0.005*.15 #with mask wearing
# )
# res$total_community_exposure
# res<-run_sim(
#   pcr_test_list_to_avoid_boarding = list(list(-2,-3)),
#   pcr_test_to_remain_in_quarantine = c(0),
#   quarantine_release_day = 1,
#   temp_and_symptoms_test_to_avoid_boarding = c(0)#,
#   #p_flight_infection_risk_per_case_contact = 0.005*.15 #with mask wearing
# )
# res$total_community_exposure
# 
# 
# 
# res<-run_sim(
#   pcr_test_list_to_avoid_boarding = list(list(-3)),
#   pcr_test_to_remain_in_quarantine = c(4),
#   quarantine_release_day = 5,
#   temp_and_symptoms_test_to_avoid_boarding = c(0)#,
#   #p_flight_infection_risk_per_case_contact = 0.005*.15 #with mask wearing
# )
# res$total_community_exposure


quar_length<-1
res1<-run_sim(
  pcr_test_list_to_avoid_boarding = list(list(-2,-3)),
  pcr_test_to_remain_in_quarantine = c(quar_length-1),
  quarantine_release_day = quar_length,
  quarantine_contacts_per_day=2.5,
  p_flight_infection_risk_per_case_contact = 0.005*.15, #with mask wearing
  temp_and_symptoms_test_to_avoid_boarding=c(0)
)
res1$total_community_exposure
ggplot(res1$data_by_infection_source,aes(x=day_of_infection,y=cases_undetected))+geom_point()

quar_length<-2
res1<-run_sim(
  pcr_test_list_to_avoid_boarding = list(list(-2,-3)),
  pcr_test_to_remain_in_quarantine = c(quar_length-1),
  quarantine_release_day = quar_length,
  quarantine_contacts_per_day=2.5,
  p_flight_infection_risk_per_case_contact = 0.005*.15, #with mask wearing
  temp_and_symptoms_test_to_avoid_boarding=c(0)
)
res1$total_community_exposure

quar_length<-3
res1<-run_sim(
  pcr_test_list_to_avoid_boarding = list(list(-2,-3)),
  pcr_test_to_remain_in_quarantine = c(quar_length-1),
  quarantine_release_day = quar_length,
  quarantine_contacts_per_day=2.5,
  p_flight_infection_risk_per_case_contact = 0.005*.15, #with mask wearing
  temp_and_symptoms_test_to_avoid_boarding=c(0)
)
res1$total_community_exposure

quar_length<-4
res1<-run_sim(
  pcr_test_list_to_avoid_boarding = list(list(-2,-3)),
  pcr_test_to_remain_in_quarantine = c(quar_length-1),
  quarantine_release_day = quar_length,
  quarantine_contacts_per_day=2.5,
  p_flight_infection_risk_per_case_contact = 0.005*.15, #with mask wearing
  temp_and_symptoms_test_to_avoid_boarding=c(0)
)
res1$total_community_exposure

quar_length<-5
res1<-run_sim(
  pcr_test_list_to_avoid_boarding = list(list(-2,-3)),
  pcr_test_to_remain_in_quarantine = c(quar_length-1),
  quarantine_release_day = quar_length,
  quarantine_contacts_per_day=2.5,
  p_flight_infection_risk_per_case_contact = 0.005*.15, #with mask wearing
  temp_and_symptoms_test_to_avoid_boarding=c(0)
)
res1$total_community_exposure

quar_length<-6
res1<-run_sim(
  pcr_test_list_to_avoid_boarding = list(list(-2,-3)),
  pcr_test_to_remain_in_quarantine = c(quar_length-1),
  quarantine_release_day = quar_length,
  quarantine_contacts_per_day=2.5,
  p_flight_infection_risk_per_case_contact = 0.005*.15, #with mask wearing
  temp_and_symptoms_test_to_avoid_boarding=c(0)
)
res1$total_community_exposure

#compare with and with out a temperature check
results_to_graph<-NULL
res<-run_sim(
  pcr_test_list_to_avoid_boarding = list(list(-2,-3)),
  pcr_test_to_remain_in_quarantine = c(quar_length-1),
  quarantine_release_day = quar_length,
  quarantine_contacts_per_day=2.5,
  p_flight_infection_risk_per_case_contact = 0.005*.15, #with mask wearing
  temp_and_symptoms_test_to_avoid_boarding=c(0) #with temperature check
)
res$data_by_infection_source$label="with temperature check"
results_to_graph<-res$data_by_infection_source
res<-run_sim(
  pcr_test_list_to_avoid_boarding = list(list(-2,-3)),
  pcr_test_to_remain_in_quarantine = c(quar_length-1),
  quarantine_release_day = quar_length,
  quarantine_contacts_per_day=2.5,
  p_flight_infection_risk_per_case_contact = 0.005*.15, #with mask wearing
  temp_and_symptoms_test_to_avoid_boarding=c() #with temperature check
)
res$data_by_infection_source$label="without temperature check"
results_to_graph<-rbind(results_to_graph,res$data_by_infection_source)
ggplot(results_to_graph,aes(x=day_of_infection,y=current_infectious_cases,group=label,color=label))+geom_point()+geom_line()
