sum(world_w_covid_data$StatusQuoMonthlyArrivals,na.rm=TRUE)
sum(world_w_covid_data$ExpectedCasesAtBorder,na.rm=TRUE)
#therefore preavlence per 100k is 
(
  sum(world_w_covid_data$ExpectedCasesAtBorder,na.rm=TRUE)
  /
    sum(world_w_covid_data$StatusQuoMonthlyArrivals,na.rm=TRUE)
  )*10^5
sum(world_w_covid_data$InferredActiveCaseTravelerRate,na.rm=TRUE)

treatment_effectiveness<-0.999
extra_spread<-0.0179
prob_infected_arr_reaches_community <- (1-treatment_effectiveness)+(extra_spread)
sum(world_w_covid_data$ExpectedCasesAtBorder,na.rm=TRUE)*prob_infected_arr_reaches_community
sum(world_w_covid_data$ExpectedNumberOfCasesInCommunity,na.rm=TRUE)
sum(world_w_covid_data$ExpectedNumberOfCasesInCommunityUnderLockdown,na.rm=TRUE)

fractions(exp(mean(c(log(1/34),log(12/9000)))))

fractions(exp(mean(c(log(1/30),log(12/9000)))))