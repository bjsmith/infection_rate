
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
library(googlesheets4)



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

get_data_closure <- function() {
  #initialize
  data_list <- NULL
  f <- function() {
    
    if(is.null(data_list)){
      print("Fetching data...")
      dl_local<-list()
      jh_cases_recovered<-readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
      jh_cases_recovered$EventType<-"Recoveries"
      jh_cases_confirmed <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
      jh_cases_confirmed$EventType<-"CasesConfirmed"
      jh_deaths<-readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
      jh_deaths$EventType<-"Deaths"
      jh_data<-rbind(jh_cases_confirmed,jh_cases_recovered,jh_deaths)
      
      
      dl_local[["jh_data"]] <- jh_data
      
      world_pop <- readr::read_csv("data/mapping/world_population/API_SP.POP.TOTL_DS2_en_csv_v2_1217749.csv") %>%
        select(`Country Code`,`Country Name`,`2019`)
      
      dl_local[["world_pop"]] <- world_pop
      
      gs4_deauth()
      
      manual_corrections<-read_sheet("1hkpfinHpxT1KcTI8umh55aaiFug12jKKSMZoae4ttlA")
      
      dl_local[["manual_corrections"]] <- manual_corrections
      data_list <<- dl_local
    }else{
      print("returning cached data")
    }
    
    return(data_list)
  }
  return(f)
}
get_data <- get_data_closure()



#this function contains some calculation of derived statistics
#this is deprecated and I am trying to separate out data import from data analysis
#this function should exclude all analysis including calculation of rates of disease
#focus should be on importing and cleaning data.
get_geomapped_covid_data <- function(life_exp_thresh=50,run_date=Sys.Date(),separate_aussie_states_and_hk=FALSE,include_geo_data=TRUE){
  #separate_aussie_states_and_hk<-TRUE
  #include_geo_data=FALSE
  
  
  ### when listing countries and subdivisions it gets very confusing what kind of code I should use for matching
  ### settling on matching using a "LocationCode", 
  ### which is the ISO-3166-1 alpha-3 for countries, and ISO-3166-2 for subdivisions
  ### this has a TWO LETTER country code.
  ### will need to carry a "LocationCode" and "CountryCode" separately to determine appropriate behaviour.
  
  if(separate_aussie_states_and_hk & include_geo_data){
    stop("Cannot include geo data and separate australian states; don't have polygons for Australian states.")
  }

  # jh_cases_recovered<-readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
  # jh_cases_recovered$EventType<-"Recoveries"
  # jh_cases_confirmed <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
  # jh_cases_confirmed$EventType<-"CasesConfirmed"
  # jh_deaths<-readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
  # jh_deaths$EventType<-"Deaths"
  # jh_data<-rbind(jh_cases_confirmed,jh_cases_recovered,jh_deaths)
  
  data_list <- get_data()
  jh_data <- data_list[["jh_data"]]
  
  if (separate_aussie_states_and_hk){
    jh_data %<>% 
      mutate(Location = 
      case_when(
        `Country/Region`=="Australia" ~ paste0(`Province/State`,", ",`Country/Region`),
        `Country/Region`=="China" & `Province/State`=="Hong Kong" ~ "Hong Kong, China",
        `Country/Region`=="China" & `Province/State`=="Macao" ~ "Macao, China",
        `Country/Region`=="China" & `Province/State`!="Hong Kong" ~ "China (mainland)",
        TRUE ~ `Country/Region`
        )
      )
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
  world_pop <- data_list[["world_pop"]]
  
  
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
  if(separate_aussie_states_and_hk){
    australian_states_data <- readr::read_csv("data/mapping/australian-state-population.csv") %>%
      select(LocationCode=ISO, Population=Value,`Country Name`=Region) %>% 
      mutate(LifeExp=as.numeric(world_data %>% filter(LocationCode=="AUS") %>% select(LifeExp) %>% .[[1]])) %>%
      mutate(YearCode=2013) %>%
      mutate(`Country Name`=paste0(`Country Name`,", Australia"))
    
    #need to manually add Hong Kong and Macao to this world data
    hk_macao_data <- 
      data.table(
        "LocationCode"=c("HKG","MAC"),
        "Population"=c(7.451*10^6,631636),
        `Country Name`=c("Hong Kong, China","Macao, China"),
        "LifeExp"=c(84.7,84.0),
        "YearCode"=c(2017,2017))
      readr::read_csv("data/mapping/australian-state-population.csv") %>%
      select(LocationCode=ISO, Population=Value,`Country Name`=Region) %>% 
      mutate(LifeExp=as.numeric(world_data %>% filter(LocationCode=="AUS") %>% select(LifeExp) %>% .[[1]])) %>%
      mutate(YearCode=2013) %>%
      mutate(`Country Name`=paste0(`Country Name`,", Australia"))
    
    world_data <- rbind(world_data,australian_states_data,hk_macao_data)
    
    
  }
  
  #mapping to get iso2 to iso3
  country_codes<-read.csv("data/mapping/country-codes.csv")
  country_iso_2_to_3_map<-country_codes[,c("ISO3166.1.Alpha.2","ISO3166.1.Alpha.3","official_name_en")]
  
  #### non-resident arrivals data
  #load New Zealand arrivals data.
  arrivals<-read_csv("data/stats-nz-arrivals-by-country.csv")
  #pick out the month we are interested in
  month_code <- paste0("2019M",str_pad(as.character(lubridate::month(run_date)),2,side="left",pad="0"))
  
  arrivals_this_month <- arrivals %>% 
    filter(`Total passenger movements by EVERY country of residence (Monthly)`==month_code) %>% 
    .[,2:ncol(.)] %>%
    tidyr::gather("Country","LocationResidentMonthlyArrivals")
  country_mapping_stats_nz <- read_csv("data/mapping/country_mapping_stats_nz_to_iso.csv")
  statsnz_arr <- left_join(arrivals_this_month,country_mapping_stats_nz,by=c("Country" = "Stats_NZ_Arrivals_Name"))
  
  
  #### NZ resident monthly arrivals from pre-covid time
  nz_resident_arrivals <- readr::read_csv("data/stats-nz-infoshare-nz-NZ-resident traveller arrivals by EVERY country of main destination and purpose (Monthly)_formatted.csv")
  colnames(nz_resident_arrivals)[1] <- "MonthLabel"
  nz_res_arrivals_this_month <- nz_resident_arrivals %>% 
    filter(MonthLabel==month_code) %>% 
    .[,2:ncol(.)] %>%
    tidyr::gather("NZRArrivalsMainDestination","NZResMonthlyArrivals")
  statsnz_nzres_arr <- left_join(nz_res_arrivals_this_month,country_mapping_stats_nz,by=c("NZRArrivalsMainDestination" = "Stats_NZ_Arrivals_Name"))
  
  #### NZ resident monthly arrivals from now 
  nz_res_arrivals_latest <- nz_resident_arrivals %>% 
    filter(MonthLabel==MonthLabel[nrow(.)]) %>%  #get the latest month
    .[,2:ncol(.)] %>%
    tidyr::gather("NZRArrivalsMainDestination","NZResMonthlyArrivalsLatest")
  statsnz_nzres_latest_arr <- left_join(nz_res_arrivals_latest,country_mapping_stats_nz,by=c("NZRArrivalsMainDestination" = "Stats_NZ_Arrivals_Name"))
  
  
  
  
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
  
  #adjust new cases to subtract cases that have been manually validated as imported
  #this will help us to reduce our prevalence estimate for a few Locations with mostly imported cases.
  #only works for our "active cases 2" definition, but that's OK.
  
  #what we can do is:
  #get a manually corrected "Active Cases" on a certain day
  #For ActiveCases1, which is confirmed minus recoveries minus deaths,
  #we just reset the total counts from the specified day
  #For days after that, for the ActiveCases2, 
  
  
  manual_corrections<-data_list[["manual_corrections"]]
  mc_merge <- manual_corrections%>%
    select(Code,`Date recorded`,`Active local cases on date override`,`Recent local fatalities on date override`)
  mc_merge <- mc_merge %>% rename("ManualCorrectDate" = "Date recorded")
  
  jh_dxc <- jh_dxc%>% 
    left_join(mc_merge,by = c(
      "CountryDivisionCodeMixed"="Code"
    ))
  
  #But it will "reset" the new cases on particular days where we have a manual measure
  #once each country only!
  #then counting new cases from that date is useful for calculating active cases.
  jh_dxc <- jh_dxc %>% mutate(OnBeforeManualCorrectDate=Date<=ManualCorrectDate)
  jh_dxc <- jh_dxc %>% 
    mutate(NewCasesImportAdjusted = 
             case_when(
               is.na(OnBeforeManualCorrectDate) ~ NewCases,
               OnBeforeManualCorrectDate ~ `Active local cases on date override`,
               OnBeforeManualCorrectDate==FALSE ~ NewCases
             ))
  #This is used for calculating "Active Cases"
  #in practice this is mainly useful where acountry has had few or no local cases
  #and we can show that the country is therefore covid-free
  
  #############
  
  if (separate_aussie_states_and_hk){
    #move the Ruby princess cases from 3 July to 19 March
    ruby_princess_category<-"AU-NSW"
    
  }else{
    ruby_princess_category<-"AU"
    
  }
  
  #On 3 July, 189 historic cases reported in crew members on board a ship were classified as Australian cases and included in NSW totals.
  #https://www.health.gov.au/news/health-alerts/novel-coronavirus-2019-ncov-health-alert/coronavirus-covid-19-current-situation-and-case-numbers#total-cases-recoveries-deaths-and-new-cases-in-the-last-24-hours
  #moving these back to 19 March when these cases entered Sydney.
  jh_dxc[which((jh_dxc$Alpha2CountrySubdivision==ruby_princess_category) & (jh_dxc$Date=="2020-07-03")),"NewCases"]<-(
      jh_dxc[which((jh_dxc$Alpha2CountrySubdivision==ruby_princess_category) & (jh_dxc$Date=="2020-07-03")),"NewCases"] - 189
    )
  jh_dxc[which((jh_dxc$Alpha2CountrySubdivision==ruby_princess_category) & (jh_dxc$Date=="2020-03-19")),"NewCases"]<-(
    jh_dxc[which((jh_dxc$Alpha2CountrySubdivision==ruby_princess_category) & (jh_dxc$Date=="2020-03-19")),"NewCases"] + 189
  )
  
    
  
  ########### calculate active cases.
  
  library(zoo)
  jh_dxc <- jh_dxc %>%
    group_by(CountryDivisionCodeMixed) %>%
    arrange(Date) %>% mutate(ActiveCases1Raw = CasesConfirmed-Deaths-Recoveries) %>%
    mutate(ActiveCases1 = ifelse(ActiveCases1Raw>=0,ActiveCases1Raw,NA)) %>%  
      #do not use this if it's returning a negative value.
      #when countries or states change their reporting, there is sometimes a discontinuity in CasesConfirmed that leads to a negative value.
    mutate(ActiveCases2 = rollapply(NewCasesImportAdjusted,21,sum,align='right',fill=NA)) %>% 
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
    filter(Date>=date_period_begin & Date<latest_date) %>%
    select(CountryDivisionCodeMixed, Location, contains("Deaths")) %>%
    group_by(CountryDivisionCodeMixed, Location) %>%
    summarise_all(mean,na.rm=TRUE) %>%
    mutate(NewDeaths=pmax(0,NewDeaths)) 
      #fix a bug so that new deaths cannot be negative
      #new deaths were being recorded as negative if a death record is "corrected"
      #instead we just don't correct
    
    

  deaths_with_lagged_cases <- jh_dxc_7_day_cases_lagged %>% left_join(jh_dxc_7_day_deaths)
  

  #deaths_with_lagged_cases$CountryPopulation<-deaths_with_lagged_cases$total_cases/deaths_with_lagged_cases$total_cases_per_million*10^6
  #we want the inferred case population rate
  #this has to come from the john hopkins data because we can get active cases from that.
  # jh_key_stats<-jh_dxc %>% ungroup %>% 
  #   filter(Date>=date_period_begin  & Date<latest_date) %>% 
  #   select(CasesConfirmed,Recoveries,ActiveCases, CountryDivisionCodeMixed,Alpha3CountryOnly) %>%
  #   group_by(CountryDivisionCodeMixed,Alpha3CountryOnly) %>%
  #   summarise_all(mean)
  #for these stats, we're going to use the very latest day's data, not the average over the time.
  jh_key_stats<-jh_dxc %>% ungroup %>%
    filter(Date==latest_date) %>%
    select(CasesConfirmed,Recoveries,ActiveCases, CountryDivisionCodeMixed,Alpha3CountryOnly) %>%
    group_by(CountryDivisionCodeMixed,Alpha3CountryOnly) %>%
    summarise_all(mean)
  deaths_with_lagged_cases <- deaths_with_lagged_cases %>% left_join(jh_key_stats,by=c("CountryDivisionCodeMixed"="CountryDivisionCodeMixed"))
  
  
  get_slope_lastn<-function(y,lastn){
    
    test_series<-y[(length(y)-lastn+1):length(y)]
    x<-(1:lastn)

    slope <- lm(test_series~x)$coefficients[[2]]
    return(slope)
  }
  slopes <- jh_dxc %>% ungroup %>% 
    group_by(CountryDivisionCodeMixed) %>%
    #filter(Alpha2CountrySubdivision=="VN") %>% 
      summarise(
        slope_7days=get_slope_lastn(NewCases,7),
        slope_14days=get_slope_lastn(NewCases,14)
      )
      
  
  world_with_covid_data<-
    #left_join(world,country_iso_2_to_3_map,by=c("iso_a2" = "ISO3166.1.Alpha.2"),name="iso_a2") %>%
    world_health %>%
    left_join(deaths_with_lagged_cases,by=c("LocationCode" = "CountryDivisionCodeMixed")) %>%
    left_join(slopes,by=c("LocationCode" = "CountryDivisionCodeMixed")) %>%
    left_join(
      statsnz_arr %>% 
        filter(`ISO3166-1-Alpha-3`!="") %>% 
        select(LocationResidentMonthlyArrivals,`ISO3166-1-Alpha-3`),
      by=c("Alpha3CountryOnly" =  "ISO3166-1-Alpha-3")) %>%
    left_join(
      statsnz_nzres_arr %>% 
        filter(`ISO3166-1-Alpha-3`!="") %>% 
        select(NZResMonthlyArrivals,`ISO3166-1-Alpha-3`),
      by=c("Alpha3CountryOnly" =  "ISO3166-1-Alpha-3")) %>%
    left_join(
      statsnz_nzres_latest_arr %>% 
        filter(`ISO3166-1-Alpha-3`!="") %>% 
        select(NZResMonthlyArrivalsLatest,`ISO3166-1-Alpha-3`),
      by=c("Alpha3CountryOnly" =  "ISO3166-1-Alpha-3"))
  
  world_with_covid_data$LocationResidentMonthlyArrivals<-as.numeric(world_with_covid_data$LocationResidentMonthlyArrivals)
  
  world_with_covid_data <- 
    world_with_covid_data %>% 
    group_by(Alpha3CountryOnly) %>% 
    #if monthly arrivals from a country e.g. Australia need to be spread over mmultiple states, do it proportional to population, 
    #if we want to be conservative, we can multiply it by the square root of the size of the population.
    mutate(LocationResidentMonthlyArrivalsScaled1 = Population/sum(Population)*LocationResidentMonthlyArrivals,
           NZResMonthlyArrivalsScaled1 = Population/sum(Population)*NZResMonthlyArrivals,
           NZResMonthlyArrivalsLatestScaled1 = Population/sum(Population)*NZResMonthlyArrivalsLatest
           ) %>% 
    ungroup
  

  return(world_with_covid_data)
}


get_max_quantiles <- function(display_data,col_of_interest,max_quantiles=8){
  
  use_bin_value<-max_quantiles
  
  for (attempt_bin_value in 1:max_quantiles){
    print("attempting...")
    summary_bin_stats <- display_data %>%
      data.frame %>%
      mutate(bin=ntile(!!as.symbol(col_of_interest),attempt_bin_value)) %>% 
      group_by(bin) %>% 
      summarise(
        minval=min(!!as.symbol(col_of_interest)),maxval=max(!!as.symbol(col_of_interest))
      ) %>%
      mutate(binnable = (lag(minval)!=minval) & (lead(maxval)!=maxval))
    binnable<-all(summary_bin_stats$binnable,na.rm = TRUE)
    if(!binnable){
      use_bin_value<-attempt_bin_value-1
      break
    }
  }
  return(use_bin_value)
  
}
