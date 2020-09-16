#source("misc/monte_carlo.R")
source("app/journey_simulation_procedural.R")

##### doing this to design a new graph
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
  quarantine_contacts_per_day=2.5,
  p_flight_infection_risk_per_case_contact = 0.005*.15#, #with mask wearing
  #temp_and_symptoms_test_to_avoid_boarding=c(0)
)
res1


res2<-run_sim(
  pcr_test_list_to_avoid_boarding = list(list(-2,-3)),
  pcr_test_to_remain_in_quarantine = c(4),
  quarantine_release_day = 5,
  quarantine_contacts_per_day=5,
  p_flight_infection_risk_per_case_contact = 0.005*.15, #with mask wearing
  temp_and_symptoms_test_to_avoid_boarding=c(0)
)
res2



res2<-run_sim(
  pcr_test_list_to_avoid_boarding = list(list(-2,-3)),
  pcr_test_to_remain_in_quarantine = c(1),
  quarantine_release_day = 2,
  quarantine_contacts_per_day=5,
  #p_flight_infection_risk_per_case_contact = 0.005*.15, #with mask wearing
  temp_and_symptoms_test_to_avoid_boarding=c(0)
)
res2

test_data <- data.frame("datalabel"=c("label1","label1","label2"),
                        "dataval"=c(2,3,5))
res2$data_by_day$label="test series"
ggplot(res2$data_by_day,aes(x=days_past_flight,y=p_infectious_in_pipeline_by_day,color=label,group=label))+
  guides(color=guide_legend(nrow=length(unique(res2$data_by_day$label)),byrow=TRUE))+
  geom_vline(xintercept = 0,linetype="dashed",color="#000000")+
  geom_vline(data=test_data,aes(xintercept=dataval))+
  #geom_label(data=journey_key_milestones,aes(x=journey_event_day,color=journey_label,group=journey_label),y=0)+
  geom_point()+geom_line()+
  scale_y_continuous(labels=scales::percent_format())+
  annotate(geom="text",x=-0.2,y=0,hjust=1,label="Day of travel")+
  labs(title="Cases remaining infectious and undetected", subtitle = "By day", y="Percent remaining infectious and undetected",x="Day through travel journey")+
  theme(legend.position="bottom")
#so what should it look like?
#I think we can add a column to data_by_day
#That column describes the cumulative percentage of all infectious in the pipeline that are undetected and still infectious.
sum(res2$data_by_infection_source$p_remains_infectious_and_in_pipeline)
#so let's add to data_by_day the sum of p_remains_infectious_and_in_pipeline at each point in time...
#I think we are going to need to calculate a "baseline" of all infections
