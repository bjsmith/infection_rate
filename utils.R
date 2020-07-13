
library(shiny)
library(dplyr)
library(lubridate)
library(readr)
library(stringr)
library(sf)
library(dplyr)
library(spData)
library(leaflet)
library(ggplot2) # tidyverse vis package
library(shiny)   # for web applications
library(rnaturalearth)
library(rnaturalearthdata)
library(data.table)
library(magrittr)


show_leaflet <- function(data_to_show,primary_col,rounding_func,legend_title,
                         quant_grades=5,pal_reverse=TRUE){
  
  pal<-colorQuantile(palette="YlOrRd",domain= data_to_show[[primary_col]],n=quant_grades,reverse=pal_reverse)
  
  chloro_labels <- paste0(data_to_show$name_long, ": ", as.character(rounding_func(data_to_show[[primary_col]])))
  
  m <- leaflet(data_to_show) %>% 
    addProviderTiles(providers$OpenStreetMap) %>%
    addPolygons(data=data_to_show,weight=2,fillColor=~pal(data_to_show[[primary_col]]),
                fillOpacity = 0.8,
                highlightOptions = highlightOptions(color='white',weight=1,
                                                    bringToFront = TRUE),
                label=chloro_labels,
                labelOptions=labelOptions(textsize="15px")) %>% 
    addLegend("bottomright", pal = pal, values = ~data_to_show[[primary_col]],
              labFormat=function(type,cuts,p){
                n=length(cuts)
                return(paste0(rounding_func(cuts[-n])," to ", rounding_func(cuts[-1])))
                #return(quantile(world_with_covid_data_inc$TestsPerCase,))
              },
              
              #labFormat = labelFormat(transform=function(x){x}),
              title = legend_title,
              opacity = 1
    ) 
  return(m)
}



######### map visualization

get_world_with_supplements<-function(){
  #create a polygon for Singapore because we can't miss out Singapore!
  
  add_circle_polygon <- function(iso_a2,name_long,lat,long){
    polygon_generic<-rbind(
      c(-0.3,-0.1),
      c(-0.3,0.1),
      c(-0.1,0.3),
      c(0.1,0.3),
      c(0.3,0.1),
      c(0.3,-0.1),
      c(0.1,-0.3),
      c(-0.1,-0.3),
      c(-0.3,-0.1))
    
    sg_poly<-t(t(polygon_generic) + c(long,lat))
    sg_multipolygon_list <- list(list(
      sg_poly
    ))
    
    sg_mp<-st_multipolygon(sg_multipolygon_list)
    
    world_template <- world[world$name_long=="Malaysia",]
    
    world_template[,c("continent",      "area_km2",  "pop",       "lifeExp",   "gdpPercap")]<-NA
    world_template$iso_a2<-iso_a2
    world_template$name_long<-name_long
    world_template$geom[[1]]<-sg_mp
    return(world_template)
  }
  
  worldc<-sf::st_as_sf(data.table::rbindlist(list(
    world,
    add_circle_polygon("SG","Singapore",1.2,103.8),
    add_circle_polygon("WS","Western Samoa",-13.9,-171.9),
    add_circle_polygon("AS","American Samoa",-14.3,-170.7),
    add_circle_polygon("TO","Tonga",-21.2,-175.2),
    add_circle_polygon("PN","Pitcairn Islands",-25.1,-130.1),
    add_circle_polygon("KI","Kiribati",1.9,-157.4),
    add_circle_polygon("NR","Nauru",-0.53,155.9),
    add_circle_polygon("TV","Tuvalu",-8.5,179.1),
    add_circle_polygon("VU","Vanuatu",-15.7,167.1)
    )))
  
  return(worldc)
}

get_owid <- function(){

}
get_geomapped_covid_data <- function(life_exp_thresh=50,run_date=Sys.Date(),separate_australian_states=FALSE,include_geo_data=TRUE){
  #separate_australian_states<-TRUE
  #include_geo_data=FALSE
  #### NEXT TO DO: ADD AUSTRALIA
  #### THIS IS GONNA BE A TOUGH BECAUSE OUR WORLD IN DATA DOESN'T HAVE AUSTRALIAN STATES
  #### JH DATA DOES, BUT IT ONLY HAS CUMULATIVE CASES. SO NEED TO:
  #### 1. CREATE A NEW TABLE THAT RECORDS NON-CUMULATIVE CONFIRMED CASES BY DOING A LEAD/LAG ON THE CUMULATIVE CONFIRMED CASES.
  #### 2. USE THAT TO DO MY DEATHS COUNTS BELOW
  #### 3. THAT SHOULD BE ALL. MAY NEED TO REPLACE OTHER INSTANCES OF OUR WORLD IN DATA. WE NO LONGER NEED IT FOR TESTING INFORMATION.
  
  ### when listing countries and subdivisions it gets very confusing what kind of code I should use for matching
  ### settling on matching using a "LocationCode", 
  ### which is the ISO-3166-1 alpha-3 for countries, and ISO-3166-2 for subdivisions
  ### this has a TWO LETTER country code.
  ### will need to carry a "LocationCode" and "CountryCode" separately to determine appropriate behaviour.
  
  if(separate_australian_states & include_geo_data){
    stop("Cannot include geo data and separate australian states; don't have polygons for Australian states.")
  }

  jh_cases_recovered<-readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
  jh_cases_recovered$EventType<-"Recoveries"
  jh_cases_confirmed <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
  jh_cases_confirmed$EventType<-"CasesConfirmed"
  jh_deaths<-readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
  jh_deaths$EventType<-"Deaths"
  jh_data<-rbind(jh_cases_confirmed,jh_cases_recovered,jh_deaths)
  
  
  if (separate_australian_states){
    jh_data %<>% 
      mutate(Location = 
               ifelse(`Country/Region`=="Australia",
                      paste0(`Province/State`,", ",`Country/Region`),
                      `Country/Region`),
             .before=`Province/State`)
  }else{
    jh_data %<>% 
      mutate(Location = `Country/Region`,
             .before=`Province/State`)
  }
  jh_bycountry<-jh_data %>% 
    dplyr::select(-Lat,-Long,-`Province/State`,-`Country/Region`) %>% 
    group_by(Location,EventType) %>% summarise_all(sum,na.rm=TRUE)
  

  jh_long<-jh_bycountry %>% tidyr::gather("Date","Count",3:ncol(.))
  jh_long$Date<-as.Date(jh_long$Date,format="%m/%d/%y")
  
  jh_dxc <- jh_long %>% tidyr::spread("EventType","Count")
  
  jh_country_mapping <- readr::read_csv("data/mapping/country_mapping_jh.csv")
  
  jh_dxc <- left_join(jh_dxc,jh_country_mapping,by=c("Location" = "John_Hopkins_Name"))
  
  #world pop
  world_pop <- readr::read_csv("data/mapping/world_population/API_SP.POP.TOTL_DS2_en_csv_v2_1217749.csv") %>%
    select(`Country Code`,`Country Name`,`2019`)
  
  
  #now get life expectancy
  life_exp<-readr::read_csv("data/mapping/lifeexpectancy-verbose.csv") %>% 
    filter(GhoDisplay=="Life expectancy at birth (years)" & SexCode=="BTSX") %>%
    select(CountryCode,Numeric,YearCode) %>% group_by(CountryCode) %>% filter(YearCode==max(YearCode)) %>%ungroup %>%
    rename(LocationCode=CountryCode)
    
  
  colnames(life_exp)[colnames(life_exp)=="Numeric"]<-"LifeExp"
  
  world_data <-left_join(life_exp,world_pop,by=c("LocationCode"="Country Code"))
  colnames(world_data)[5]<-"Population"
  
  world_data<-rbind(world_data,
                  tibble::tibble(
                    LocationCode="TWN","LifeExp"=80.4,"YearCode"=2017,"Country Name"="Taiwan","Population"=23.78*10^6))
  
  #add the Australian states
  if(separate_australian_states){
    australian_states_data <- readr::read_csv("data/mapping/australian-state-population.csv") %>%
      select(LocationCode=ISO, Population=Value,`Country Name`=Region) %>% 
      mutate(LifeExp=as.numeric(world_data %>% filter(LocationCode=="AUS") %>% select(LifeExp) %>% .[[1]])) %>%
      mutate(YearCode=2013) %>%
      mutate(`Country Name`=paste0(`Country Name`,", Australia"))
    world_data <- rbind(world_data,australian_states_data)
  }
  
  #mapping to get iso2 to iso3
  country_codes<-read.csv("data/mapping/country-codes.csv")
  country_iso_2_to_3_map<-country_codes[,c("ISO3166.1.Alpha.2","ISO3166.1.Alpha.3","official_name_en")]
  
  #### arrivals data
  #load New Zealand arrivals data.
  arrivals<-read_csv("data/stats-nz-arrivals-by-country.csv")
  #pick out the month we are interested in
  
  month_code <- paste0("2019M",str_pad(as.character(lubridate::month(run_date)),2,side="left",pad="0"))
  
  arrivals_this_month <- arrivals %>% 
    filter(`Total passenger movements by EVERY country of residence (Monthly)`==month_code) %>% 
    .[,2:ncol(.)] %>%
    tidyr::gather("Country","MonthlyArrivals")
  country_mapping_stats_nz <- read_csv("data/mapping/country_mapping_stats_nz_to_iso.csv")
  statsnz_arr <- left_join(arrivals_this_month,country_mapping_stats_nz,by=c("Country" = "Stats_NZ_Arrivals_Name"))
  
  if(include_geo_data){
    worldc <- get_world_with_supplements()
    
    world_health<-worldc %>% 
      left_join(country_iso_2_to_3_map,by=c("iso_a2" = "ISO3166.1.Alpha.2"),name="iso_a2") %>%
      left_join(world_data,by=c("ISO3166.1.Alpha.3"="LocationCode")) %>%
      rename("LocationCode"="ISO3166.1.Alpha.3")
  }else{
    world_health<-world_data
  }
  
  
  #merge it in
  world_with_covid_data<-
    #left_join(world,country_iso_2_to_3_map,by=c("iso_a2" = "ISO3166.1.Alpha.2"),name="iso_a2") %>%
    world_health #%>%
    # left_join(owid_7_day_average_testing_observable,by=c("ISO3166.1.Alpha.3" = "iso_code"))
  
  vals_to_include <- 
    (
      #is.finite(world_with_covid_data$TestsPerCase) & !is.na(world_with_covid_data$TestsPerCase)
    # & 
    world_with_covid_data$LifeExp>=life_exp_thresh
    )
  
  
  world_with_covid_data_inc<-world_with_covid_data[vals_to_include,]
  
  
  
  
  jh_dxc <- jh_dxc %>%
    group_by(CountryDivisionCodeMixed) %>%
    arrange(Date) %>% 
    mutate(NewDeaths = Deaths - lag(Deaths)) %>%
    ungroup
  
  jh_dxc <- jh_dxc %>% group_by(CountryDivisionCodeMixed) %>%
    arrange(Date) %>% mutate(NewCases = CasesConfirmed-lag(CasesConfirmed)) %>% ungroup
  
  library(zoo)
  jh_dxc <- jh_dxc %>%
    group_by(CountryDivisionCodeMixed) %>%
    arrange(Date) %>% mutate(ActiveCases1 = CasesConfirmed-Deaths-Recoveries) %>%
    mutate(ActiveCases2 = rollapply(NewCases,28,sum,align='right',fill=NA)) %>% 
    mutate(ActiveCases = pmin(ActiveCases1,ActiveCases2,na.rm=TRUE)) %>%
    ungroup
  #some locations don't reliably report recoveries.
  #to put a ceiling on cases, I follow NSW procedure and only include a case as active if it's been reported in four weeks from the beginning of my case period
  
  #https://www.nsw.gov.au/covid-19/find-facts-about-covid-19

  
  latest_date <- max(jh_dxc$Date)
  date_period_begin<- latest_date - days(7)
  
  #select the cases 3 weeks ago
  jh_dxc_7_day_cases_lagged <- jh_dxc %>%
    filter(Date>=(date_period_begin - days(21)) & Date<(date_period_begin - days(14))) %>% 
    select(CountryDivisionCodeMixed, Location, contains("Cases")) %>%
    group_by(CountryDivisionCodeMixed, Location) %>%
    summarise_all(mean,na.rm=TRUE)
  colnames(jh_dxc_7_day_cases_lagged)[3:ncol(jh_dxc_7_day_cases_lagged)]<-
    paste0("Lagged",colnames(jh_dxc_7_day_cases_lagged)[3:ncol(jh_dxc_7_day_cases_lagged)])
  
  
  jh_dxc_7_day_deaths <- jh_dxc %>%
    filter(Date>=date_period_begin) %>%
    select(CountryDivisionCodeMixed, Location, contains("Deaths")) %>%
    group_by(CountryDivisionCodeMixed, Location) %>%
    summarise_all(mean,na.rm=TRUE)
    

  deaths_with_lagged_cases <- jh_dxc_7_day_cases_lagged %>% left_join(jh_dxc_7_day_deaths)
  
  assumed_cfr<-0.005
  deaths_with_lagged_cases<- 
    deaths_with_lagged_cases %>% 
    mutate(InferredDetectionRate = (assumed_cfr*LaggedNewCases/NewDeaths))
  #if there are NO deaths then we infer detection rate is 100%
  deaths_with_lagged_cases[deaths_with_lagged_cases$NewDeaths==0,"InferredDetectionRate"]<-1
  
  
  
  
  #deaths_with_lagged_cases$CountryPopulation<-deaths_with_lagged_cases$total_cases/deaths_with_lagged_cases$total_cases_per_million*10^6
  #we want the inferred case population rate
  #this has to come from the john hopkins data because we can get active cases from that.
  jh_key_stats<-jh_dxc %>% ungroup %>% 
    filter(Date>=date_period_begin) %>% 
    select(CasesConfirmed,Recoveries,ActiveCases, CountryDivisionCodeMixed,Alpha3CountryOnly) %>%
    group_by(CountryDivisionCodeMixed,Alpha3CountryOnly) %>%
    summarise_all(mean)
  deaths_with_lagged_cases <- deaths_with_lagged_cases %>% left_join(jh_key_stats,by=c("CountryDivisionCodeMixed"="CountryDivisionCodeMixed"))
  
  deaths_with_lagged_cases <-
    deaths_with_lagged_cases %>% 
    mutate(InferredActiveCases= (ActiveCases/InferredDetectionRate))
  
  world_with_covid_data<-
    #left_join(world,country_iso_2_to_3_map,by=c("iso_a2" = "ISO3166.1.Alpha.2"),name="iso_a2") %>%
    world_health %>%
    left_join(deaths_with_lagged_cases,by=c("LocationCode" = "CountryDivisionCodeMixed")) %>%
    left_join(
      statsnz_arr %>% 
        filter(`ISO3166-1-Alpha-3`!="") %>% 
        select(MonthlyArrivals,`ISO3166-1-Alpha-3`),
      by=c("Alpha3CountryOnly" =  "ISO3166-1-Alpha-3"))
  world_with_covid_data$MonthlyArrivals<-as.numeric(world_with_covid_data$MonthlyArrivals)
  
  world_with_covid_data <- 
    world_with_covid_data %>% 
    group_by(Alpha3CountryOnly) %>% 
    #if monthly arrivals from a country e.g. Australia need to be spread over mmultiple states, do it proportional to population, 
    #if we want to be conservative, we can multiply it by the square root of the size of the population.
    mutate(MonthlyArrivalsScaled1 = Population/sum(Population)*MonthlyArrivals#*sqrt(length(Population))
           ) %>% 
    ungroup
  
  world_with_covid_data <- 
    world_with_covid_data %>% 
    mutate(InferredActiveCasePopRate = (InferredActiveCases/Population))
  
  world_with_covid_data <- 
    world_with_covid_data %>% 
    mutate(ActiveCasePopRate = ActiveCases/Population)
  
  
  
  
  
  return(world_with_covid_data)
}

get_analysis_covid_data <- function(world_with_covid_data,quarantine_odds_override=NULL,travel_volume_weighting=1){
  print(quarantine_odds_override)
  print(travel_volume_weighting)
  
  world_with_covid_data$InferredDetectionRate[is.infinite(world_with_covid_data$InferredDetectionRate)]<-1
  
  
  ### calculate the probability of at least one COVID-19 case
  
  #world_with_covid_data$MonthlyArrivals<-as.numeric(world_with_covid_data$MonthlyArrivals)
  world_with_covid_data$MonthlyArrivalsScaled<-world_with_covid_data$MonthlyArrivalsScaled1*travel_volume_weighting

  #assume that each new person has an independent chance of carrying COVID-19
  #this is conservative when we are trying to work out the probability of at least one case
  #so the probability is going to be 
  #1-((Population-ProbableActiveCases)/Population)^MonthlyArrivals
  #if 10% of the population of 5000 has covid, and 20 of them come to NZ, that's
  #1-((5000-500)/5000)^20
  #1-(4500/5000)^20
  #0.8784233
  world_with_covid_data <- 
    world_with_covid_data %>% mutate(
      ProbabilityOfMoreThanZeroCases=1-((Population-InferredActiveCases)/Population)^MonthlyArrivalsScaled)
  
  world_with_covid_data <- 
    world_with_covid_data %>% mutate(
      ExpectedNumberOfCases=InferredActiveCasePopRate*MonthlyArrivalsScaled)
  
  
  
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
        1-((Population-InferredActiveCases*prob_infected_arr_reaches_community)/Population)^MonthlyArrivalsScaled)
  
  world_with_covid_data <- 
    world_with_covid_data %>% mutate(
      ExpectedNumberOfCasesInCommunity=InferredActiveCasePopRate*MonthlyArrivalsScaled*prob_infected_arr_reaches_community)
  
  
  world_with_covid_data$InfActiveCasesPerMillion <- world_with_covid_data$InferredActiveCasePopRate*10^6
  world_with_covid_data$InfActiveCasesPerThousand <- world_with_covid_data$InferredActiveCasePopRate*10^3
  world_with_covid_data$ActiveCasesPerThousand <- world_with_covid_data$ActiveCasePopRate*10^3
  world_with_covid_data$ActiveCasesPerMillion <- world_with_covid_data$ActiveCasePopRate*10^6

  return(world_with_covid_data)
}



