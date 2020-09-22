
#default_run_date<-as.Date("2020-08-22")
default_run_date<-Sys.Date()
default_month_name <- format(default_run_date,"%B")


source("utils.R")
print_elapsed_time("START")

source("simulation.R")
source("country_classification_rules.R")
source("simJourneyPanel.R")
source("defaults.R")
source("journey_simulation_procedural.R")
verbose<-FALSE

print_elapsed_time("loaded dependencies")
#Ben J:
#Assume 0.5% transmission risk without mask (it’s a conservative worst case rate)
#If mask use reduces risk by 90% as per Arthur’s email then in-flight transmission risk reduced to 0.05%
#Therefore one case expected to be acquired ‘in-flight’ for every 125 infectious cases carried
#default_aircraft_infection_rate <- 0.005 #without mask
#default_aircraft_mask_effectiveness_percent <- 90

#run_date<-as.Date("2020-07-15")#Sys.Date()
#run_date<-as.Date("2020-08-15")#Sys.Date()
#run_date<-Sys.Date()
#run_date<-as.Date("2020-08-14")
#run_date<-as.Date("2020-08-22")

#month_name <- format(run_date,"%B")

# world_with_covid_data,
# treatment_effectiveness,
# extra_spread,
# travel_volume_proportion=1,
# assumed_ifr=0.006,
# traveler_relative_prevalence=1,

# default_geo_world_basic_data <- get_geomapped_covid_data(life_exp_thresh,default_run_date)
# print_elapsed_time("RUNNING SIM")
# default_geo_world_with_covid_data <- simulate_treatment_for_countries(default_geo_world_basic_data,
#                                                               treatment_effectiveness = default_assumed_effectiveness,
#                                                               extra_spread = 0,
#                                                               assumed_ifr = default_assumed_ifr_percent/100,
#                                                               
#                                                               traveler_relative_prevalence=default_traveler_relative_prevalence,
#                                                               current_lockdown_passenger_volume = default_current_lockdown_passenger_volume
# )

print_elapsed_time("LOADING DATA 2...")
default_nogeo_world_basic_data <- get_geomapped_covid_data(life_exp_thresh,default_run_date,separate_aussie_states_and_hk = TRUE,include_geo_data = FALSE)
default_simulation_data <- simulate_treatment_for_countries(
  default_nogeo_world_basic_data,
  treatment_effectiveness = default_assumed_effectiveness,
  extra_spread = 0,
  assumed_ifr = default_assumed_ifr_percent/100,
  traveler_relative_prevalence=default_traveler_relative_prevalence,
  current_lockdown_passenger_volume = default_current_lockdown_passenger_volume)

countries_to_choose_from<-
  default_nogeo_world_basic_data$Location %>%
  sort %>%
  .[.!="New Zealand"]


small_country_health_data <-data.frame(
  "Country"=c("Samoa", "Cook Islands"),
  "Website"=c("https://www.health.gov.ws/", "https://www.health.gov.ck/")
)



source("key_interest_countries.R")


###########create data table for table tab
#lose geometry info and convert to datatable. note: not the same as data.table!

###########main dashboard.
source("map_page.R")
source('components/intervention_simulation.R')
source('components/journey_page.R')
source('components/proposal.R')
source('components/summary_map.R')

print_elapsed_time("Creating server component...")
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  #####################
  #universal
  get_run_date <- reactive({
    input$simsettings_run_date
  })
  get_run_month <- reactive({
    format(input$simsettings_run_date,"%B")
  })
  
  #we tie these two date inputs together to function the same.
  # observeEvent(input$summary_map_run_date,{updateDateInput(session,inputId = "simsettings_run_date",value=input$summary_map_run_date)})
  observeEvent(input$simsettings_run_date,{
    print("date changed...")
  #   #updateDateInput(session,inputId = "summary_map_run_date",value=input$simsettings_run_date)
  #   input$summary_map_run_date <- input$simsettings_run_date
     runjs(paste0(
       'document.getElementsByClassName("navbar-brand")[0].innerText="',
       app_display_title, ' (',input$simsettings_run_date,')',
       '"'))
  #   #html("navbar-brand",as.character(input$simsettings_run_date))
  } 
  )
  

  

  
  nogeo_world_basic_data <- reactive({
    print_elapsed_time("LOADING DATA VIA REACTIVE...")
    return(get_geomapped_covid_data(life_exp_thresh,input$simsettings_run_date,separate_aussie_states_and_hk = TRUE,include_geo_data = FALSE))
  })
  
  # 
  # 
  # simulation_data <- reactive({
  #   print_elapsed_time("SIMULATING DATA AGAIN 2...")
  #   simulate_treatment_for_countries(
  #   nogeo_world_basic_data(),
  #   treatment_effectiveness = default_assumed_effectiveness,
  #   extra_spread = 0,
  #   assumed_ifr = default_assumed_ifr_percent/100,
  #   traveler_relative_prevalence=default_traveler_relative_prevalence,
  #   current_lockdown_passenger_volume = default_current_lockdown_passenger_volume)})
  
  
  

  
  generate_world_with_covid_data <- reactive({
    get_intervention_risk()
  })
  
  geo_world_basic_data <- reactive({
    get_geomapped_covid_data(life_exp_thresh,default_run_date)
  })
  generate_mapped_world_with_covid_data <- reactive({
    simulate_treatment_for_countries(
      geo_world_basic_data(),
      treatment_effectiveness = input$intsim_effectiveness_level2/100,
      extra_spread = default_assumed_spread,
      assumed_ifr = input$simsettings_ifr/100,
      traveler_relative_prevalence=input$simsettings_traveler_relative_prevalence,
      current_lockdown_passenger_volume = input$simsettings_current_lockdown_passenger_volume)
  })
  
  get_filtered_mapped_world_with_covid_data <- reactive({
    generate_mapped_world_with_covid_data() %>% 
      filter(LifeExp>=life_exp_thresh)
  })
  
  observeEvent(input$simsettings_mode, {
    if(input$simsettings_mode=="Advanced"){
      showTab(inputId = "mainNavbarPage", target = "Validation")
      showTab(inputId = "mainNavbarPage", target = "Method and assumptions")
      
      hideTab(inputId = "mainNavbarPage", target = "Prevalence map")
    }else if (input$simsettings_mode=="Simple"){
      hideTab(inputId = "mainNavbarPage", target = "Validation")
      hideTab(inputId = "mainNavbarPage", target = "Method and assumptions")
      
      showTab(inputId = "mainNavbarPage", target = "Prevalence map")
    }else{
      stop("Unrecognized simsettings_mode")
    }
    
  })
  
  ###############################################################
  #country report
  generate_country_profile_report_params <-reactive({
    
    print(input$locprofile_Location)
    if(input$locprofile_Location %in% c("Cook Islands", "Samoa")){
      print("generating abridged report")
      params <- 
        list(
          location_profile = input$locprofile_Location,
          nz_info = generate_world_with_covid_data()  %>% filter(Location=="New Zealand"),
          health_furtherinfo = small_country_health_data %>% filter(Country==input$locprofile_Location) %>% .$Website
        )
      
    }else{
      print("generating full report")
      location_info <- generate_world_with_covid_data() %>% filter(Location==input$locprofile_Location)
      trust_classification <- classify_country_trust(
        location_info$LifeExp,input$locprofile_Location
      )
      country_classification = classify_country(
        location_info$LifeExp,
        location_info$ExpectedCasesAtBorder
      )
      
      # Set up parameters to pass to Rmd document
      params <- list(
        location_profile = input$locprofile_Location,
        location_info = location_info,
        nz_info = generate_world_with_covid_data()  %>% filter(Location=="New Zealand"),
        country_classification = country_classification,
        trust_rating = trust_classification,
        assumed_ifr = input$simsettings_ifr/100
      )
    }
    
    return(params)
  })
  
  
  get_report_filename <- reactive({
    
    if(input$locprofile_Location %in% c("Cook Islands", "Samoa")){
      template_filename <-"covidfree_country_profile_report.Rmd"
    }else{
      template_filename <-"country_profile_report.Rmd"
    }
    template_filename
  })
  
  
  output$onscreen_report <- renderUI({
    #https://community.rstudio.com/t/generating-markdown-reports-from-shiny/8676/5
    print(paste0("generating report for ",input$locprofile_Location))
    
    template_filename <- get_report_filename()
    params <- generate_country_profile_report_params()
    
    #temp_dir = tempdir()
    #tempReport <- file.path(temp_dir, "country_profile_report.Rmd")
    #file.copy("country_profile_report.Rmd", tempReport, overwrite = TRUE)
    
    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    #output_filename="country_profile_report_temp.html"
    
    includeHTML(
      rmarkdown::render(template_filename, 
                        output_format = "html_document",
                        params = params,
                        envir = new.env(parent = globalenv())
      ))
  })
  
  ###############################################################
  #country profile page
  
  output$downloadable_report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.pdf",
    content = function(file) {
      print(paste0("generating report for ",input$locprofile_Location))
      params <- generate_country_profile_report_params()
      
      template_filename <- get_report_filename()
      
      #temp_dir = tempdir()
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy(template_filename, tempReport, overwrite = TRUE)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        output_format = "pdf_document",
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  get_country_table <- reactive({
    
    
    wwcd <- generate_world_with_covid_data()
    
    wwcd1 <- wwcd %>% 
      #filter(LifeExp>=life_exp_thresh) %>%
      filter(Location!="New Zealand") %>% #doesn't make sense to display New Zealand here.
      filter(Total2019MonthlyArrivals>=input$countrylist_travelerfilter) %>%
      data.frame %>%
      arrange(InfActiveCasesPerMillion)
    
    #classic version
    if(input$countrylist_type=="Classic"){
      display_tibble <- 
        wwcd1 %>%
        select(#LocationCode, 
          Location, #Population, #total_cases,
          ActiveCases,#InferredActiveCases,
          InfActiveCasesPerMillion,
          PrevalenceRating,
          OutlookRating,
          Total2019MonthlyArrivals,
          ExpectedCasesAtBorder,
          InterventionLabel,
          MonthlyArrivalsWeighted,
          ExpectedNumberOfCasesInCommunity
        ) %>% as_tibble()
      
      colnames(display_tibble) <- c(#"ISO",
        "Location",
        #"Population",
        "Active Cases",
        "Prevalence (infections / mil)",
        "Prevalence Rating",
        "Outlook Rating",
        "2019 Monthly arrivals",
        "Expected number of cases arriving per month",
        "Assigned intervention",
        "Est. arrivals per month (based on 2019)", 
        "Expected number of cases lasting through quarantine per month"
      )
    }else if(input$countrylist_type=="Comprehensive high-level"){
      #comprehensive
      #designed to show as much detail as Arthur asked for at a high-level.
      display_tibble <- 
        wwcd1 %>%
        select(
          Location,
          owid_population_density, #add
          owid_new_tests_per_million, #add
          owid_tests_per_case, #add
          ActiveCases,
          NewDeaths, #add
          InfActiveCasesPerMillion,
          PrevalenceRating,
          OutlookRating,
          DataReliabilityRating,#add
          Total2019MonthlyArrivals,
          MonthlyArrivalsWeighted,
          ExpectedCasesAtBorder,
          ExpectedNumberOfCasesInCommunity
        ) %>% as_tibble()
      
      colnames(display_tibble) <- c(#"ISO",
        "Location",
        "Population density",
        "New tests / mil (last 7 days average)",
        "New tests / Case (last 7 days average)",
        "Active Cases",
        "New deaths (last 7 days average)",
        "Prevalence (infections / mil)",
        "Prevalence Rating",
        "Outlook Rating",
        "Data Reliability",
        "2019 Monthly arrivals",
        "Est. arrivals per month (based on 2019)", 
        "Expected number of cases arriving per month",
        "Expected number of cases lasting through quarantine per month"
      )
    }else if(input$countrylist_type=="Prevalence detail"){
      #prevalence detail
      display_tibble <- 
        wwcd1 %>%
        select(
          Location,
          Population,
          LaggedNewCases,
          NewDeaths,
          InferredDetectionRate,
          ActiveCases,
          InferredActiveCases,
          InfActiveCasesPerMillion,
          PrevalenceRating,
          PredictedInfActiveCasesPerMillion,
          OutlookRating,
          LifeExp,
          DataReliabilityRating
        ) %>% as_tibble()
      
      colnames(display_tibble) <- c(#"ISO",
        "Location",
        "Population",
        "Cases confirmed 14-21 days ago (daily average)",
        "Deaths last 7 days (daily average)",
        "Inferred detection rate",
        "Active cases",
        "Est. active infections",
        "Prevalence (infections / mil)",
        "Prevalence Rating",
        "Predicted prevalence in 14 days (infections / mil)",
        "Outlook Rating",
        "Life expectancy",
        "Data Reliability"
      )
    }else if(input$countrylist_type=="Raw"){
      #raw
      display_tibble <- 
        wwcd1
      #all the columns. chaos!
    }else{
      stop("unknown list type")
    }
    
    display_tibble
  }
  
  )
  
  output$country_table<-DT::renderDataTable({
    display_tibble<- get_country_table()
    
    display_dt <- DT::datatable(
      display_tibble,
      rownames=FALSE
      #filter="top",
    )
    
    #round some columns by 1 dp
    cols_1_dp <- intersect(colnames(display_tibble),c(
      'Prevalence (infections / mil)',
      "New tests / mil (last 7 days average)",
      "New tests / Case (last 7 days average)",
      "Predicted prevalence in 14 days (infections / mil)"))
    #and some by 2dp
    cols_2_dp <- intersect(colnames(display_tibble),
                           c('Expected number of cases arriving per month',
                             'Expected number of cases escaping screening per month',
                             'Expected number of cases lasting through quarantine per month',
                             "Inferred detection rate",
                             "Population density"
                           ))
    #and others by other amounts.
    cols_0_dp <- intersect(colnames(display_tibble),
                           c("2019 Monthly arrivals",
                             'Active Cases','Active cases',
                             "New deaths (last 7 days average)",
                             "Cases confirmed 14-21 days ago (daily average)",
                             "Deaths last 7 days (daily average)",
                             "Est. active infections",
                             'Est. arrivals per month (based on 2019)',
                             "Life expectancy",
                             "Population"
                           ))
    
    #if we're using the raw table, then round ALL numeric columns
    # if(input$countrylist_type=="Raw"){
    #   display_tibble <- 
    #     display_tibble %>%
    #     mutate(across(is.numeric, formatRound, 1, mark=","))
    # }
    
    if(length(cols_1_dp)>0){
      display_dt <- 
        display_dt %>% 
        formatRound(cols_1_dp,1,mark=",")
    }
    if(length(cols_2_dp)>0){
      display_dt <- 
        display_dt %>% 
        formatRound(cols_2_dp,2)
    }
    if(length(cols_0_dp)>0){
      display_dt <- 
        display_dt %>% 
        formatRound(cols_0_dp,0,mark=",")
    }
    display_dt
  })
  
  output$countrylist_downloadcsv <- downloadHandler(
    filename = function() {
      paste("risk_matrix_", get_run_date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(get_country_table(), file, row.names = FALSE)
    }
  )
  
  output$country_table_notes <- renderUI({HTML(paste0(
    "Estimated arrivals per month are calculated assuming arrivals under existing quarantine regime, plus a ", 
    "% of 2019 arrivals, corresponding to each intervention, as set on the \"Intervention simulation\" tab. In reality these will differ from treatment to treatment.<br/> <br/> ",
    "<em>Cook Islands</em> and <em>Western Samoa</em> currently report COVID-free status, are rated zero risk. Due to their zero-risk status, it hasn't been necessary to include them in the dataset and they are not listed above.<br /><br />",
    "."
  ))})
  
  #######################################
  #intervention simulation page
  
  
  sim_world_with_covid_data_level0 <- reactive({
    world_w_covid_data <- simulate_treatment_for_countries(
      nogeo_world_basic_data(),
      treatment_effectiveness = input$intsim_effectiveness_level0/100,
      extra_spread = input$intsim_extraspread_level0/100,
      travel_volume_proportion = input$intsim_percent_tvolume_level0/100,
      assumed_ifr = input$simsettings_ifr/100,
      traveler_relative_prevalence=input$simsettings_traveler_relative_prevalence,
      current_lockdown_passenger_volume = input$simsettings_current_lockdown_passenger_volume)
    return(world_w_covid_data)
  })
  
  sim_world_with_covid_data_level1 <- reactive({
    world_w_covid_data <- simulate_treatment_for_countries(
      nogeo_world_basic_data(),
      treatment_effectiveness = input$intsim_effectiveness_level1/100,
      extra_spread = input$intsim_extraspread_level1/100,
      travel_volume_proportion = input$intsim_percent_tvolume_level1/100,
      assumed_ifr = input$simsettings_ifr/100,
      traveler_relative_prevalence=input$simsettings_traveler_relative_prevalence,
      current_lockdown_passenger_volume = input$simsettings_current_lockdown_passenger_volume)
    return(world_w_covid_data)
  })
  
  sim_world_with_covid_data_level2 <- reactive({
    world_w_covid_data <- simulate_treatment_for_countries(
      nogeo_world_basic_data(),
      treatment_effectiveness = input$intsim_effectiveness_level2/100,
      extra_spread = input$intsim_extraspread_level2/100,
      travel_volume_proportion = input$intsim_percent_tvolume_level2/100,
      assumed_ifr = input$simsettings_ifr/100,
      traveler_relative_prevalence=input$simsettings_traveler_relative_prevalence,
      current_lockdown_passenger_volume = input$simsettings_current_lockdown_passenger_volume)
    return(world_w_covid_data)
  })
  
  sim_world_with_covid_data_level3 <- reactive({
    world_w_covid_data <- simulate_treatment_for_countries(
      nogeo_world_basic_data(),
      treatment_effectiveness = input$intsim_effectiveness_level3/100,
      extra_spread = input$intsim_extraspread_level3/100,
      travel_volume_proportion = input$intsim_percent_tvolume_level3/100,
      assumed_ifr = input$simsettings_ifr/100,
      traveler_relative_prevalence=input$simsettings_traveler_relative_prevalence,
      current_lockdown_passenger_volume = input$simsettings_current_lockdown_passenger_volume)
    return(world_w_covid_data)
  })
  
  
  #basically NZ citizens only.
  sim_world_with_covid_data_statusquo <- reactive({
    print_elapsed_time("running sim_world_with_covid_data_statusquo....")
    world_w_covid_data <- simulate_treatment_for_countries(
      nogeo_world_basic_data(),
      treatment_effectiveness = input$intsim_effectiveness_level4/100,
      extra_spread = input$intsim_extraspread_level4/100,
      travel_volume_proportion=0,
      assumed_ifr = input$simsettings_ifr/100,
      traveler_relative_prevalence=input$simsettings_traveler_relative_prevalence,
      current_lockdown_passenger_volume = input$simsettings_current_lockdown_passenger_volume)
    print_elapsed_time("returning sim_world_with_covid_data_statusquo")
    return(world_w_covid_data)
    
  })
  #NZ citizens only, with the new pre-departure testing
  sim_world_with_covid_data_predeparture_testing <- reactive({
    world_w_covid_data <- simulate_treatment_for_countries(
      nogeo_world_basic_data(),
      treatment_effectiveness = input$intsim_effectiveness_level3/100,
      extra_spread = input$intsim_extraspread_level3/100,
      travel_volume_proportion=0,
      assumed_ifr = input$simsettings_ifr/100,
      traveler_relative_prevalence=input$simsettings_traveler_relative_prevalence,
      current_lockdown_passenger_volume = input$simsettings_current_lockdown_passenger_volume)
    return(world_w_covid_data)
    
  })
  
  countries_level0_df <- reactive({
    return(get_intsim_dt(get_countries_allocated_to_level0(),
                         sim_world_with_covid_data_level0())
    )
  })
  
  countries_level1_df <- reactive({
    return(get_intsim_dt(get_countries_allocated_to_level1(),
                         sim_world_with_covid_data_level1())
    )
  })
  
  countries_level2_df <- reactive({
    return(get_intsim_dt(get_countries_allocated_to_level2(),
                         sim_world_with_covid_data_level2())
    )
  })
  
  countries_level3_df <- reactive({
    return(get_intsim_dt(get_countries_allocated_to_level3(),
                         sim_world_with_covid_data_level3())
    )
  })

  get_status_quo_risk <- reactive({
    
    #just all countries, but we'll only get expected cases from NZ Residents
    status_quo_risk <- 
      sim_world_with_covid_data_statusquo() %>%
      mutate(
        InterventionLevel=NA,
        InterventionLabel="None")
    
    return(status_quo_risk)
  })
  
  get_countries_allocated_to_level0 <-reactive({
    return(get_countries_allocated_to_leveln(input,0,sim_world_with_covid_data_statusquo()))
  })
  get_countries_allocated_to_level1 <-reactive({
    return(get_countries_allocated_to_leveln(input,1,sim_world_with_covid_data_statusquo()))
  })
  get_countries_allocated_to_level2 <-reactive({
    return(get_countries_allocated_to_leveln(input,2,sim_world_with_covid_data_statusquo()))
  })
  get_countries_allocated_to_level3 <-reactive({
    return(get_countries_allocated_to_leveln(input,3,sim_world_with_covid_data_statusquo()))
  })
  
  get_countries_allocated_to_levels0to3<-reactive({
    c(
      get_countries_allocated_to_level0(),
      get_countries_allocated_to_level1(),
      get_countries_allocated_to_level2(),
      get_countries_allocated_to_level3()
    )
  })
  
  get_intervention_risk <- reactive({
    print_elapsed_time("running get_intervention_risk")
    countries_in_intervention <- get_countries_allocated_to_levels0to3()
    #this is a bit different to status quo risk
    #because we're applying an intervention to reduce the number of people coming from other countries.
    if(input$intsim_universalPCR){
      world_raw <- sim_world_with_covid_data_predeparture_testing()
    }else{
      world_raw <- sim_world_with_covid_data_statusquo()
    }
    status_quo_countries <- world_raw %>%
      mutate(
        InterventionLevel=NA,
        InterventionLabel="None")%>% 
      filter((Location %in% countries_in_intervention)==FALSE) %>%
      mutate(
        InterventionLevel=NA,
        InterventionLabel="None")
    
    intervened_countries_risk <- rbind(
      sim_world_with_covid_data_level0() %>%
        filter(Location %in% get_countries_allocated_to_level0()) %>%
        mutate(
          InterventionLevel=0),
      sim_world_with_covid_data_level1() %>%
        filter(Location %in% get_countries_allocated_to_level1()) %>%
        mutate(
          InterventionLevel=1),
      sim_world_with_covid_data_level2() %>%
        filter(Location %in% get_countries_allocated_to_level2()) %>%
        mutate(
          InterventionLevel=2),
      sim_world_with_covid_data_level3() %>%
        filter(Location %in% get_countries_allocated_to_level3()) %>%
        mutate(
          InterventionLevel=3)
    ) %>% rowwise() %>% mutate(InterventionLabel=get_intervention_name(InterventionLevel))
    
    intervention_risk <- 
      rbind(status_quo_countries ,
            intervened_countries_risk
      )

    print_elapsed_time("returning get_intervention_risk")
    return(intervention_risk)
  })
  
  total_risk_text <- render_total_risk_text(
    get_world_with_covid_data_level0 = sim_world_with_covid_data_level0,
    get_countries_allocated_to_levels0to3 = get_countries_allocated_to_levels0to3,
    get_status_quo_risk = get_status_quo_risk,
    get_intervention_risk = get_intervention_risk
  )
  
  observeEvent(input$intsim_20countries,{
    print("reacting")
    #shinyjs::hide(id = "intsim_level1_max_prevalence")
    #let's get the risk for the 20 countries,
    #and apply them into these categories as appropriate.
    
    intervention_risk <- get_intervention_risk()
    intervention_risk_keycountries <- 
      intervention_risk %>% 
      filter(Location %in% key_interest_countries)
    
    
    updateSelectInput(
      session=session,
      inputId="intsim_countries_level0",
      selected = 
        intervention_risk_keycountries %>% filter(PrevalenceRating %in% "COVID-free") %>% .$Location
    )
    updateSelectInput(
      session=session,
      inputId="intsim_countries_level1",
      selected = 
        intervention_risk_keycountries %>% filter(PrevalenceRating %in% "Low") %>% .$Location
    )
    updateSelectInput(
      session=session,
      inputId="intsim_countries_level2",
      selected = 
        intervention_risk_keycountries %>% filter(PrevalenceRating %in% "Moderate") %>% .$Location
    )
    updateSelectInput(
      session=session,
      inputId="intsim_countries_level3",
      selected = 
        intervention_risk_keycountries %>% filter(PrevalenceRating %in% "High") %>% .$Location
    )
    
  })
  
  output$intsim_AreaPlot <- renderPlot({
    intervention_risk <- get_intervention_risk()
    generate_areaPlot(intervention_risk)
  })
  output$total_risk_graph <-(
    renderPlot({
      get_total_risk_graph(get_status_quo_risk(),get_intervention_risk(),get_countries_allocated_to_levels0to3())
    })
  )
  output$cumulative_risk_plot <-renderPlot({
      get_cumulative_risk_plot(get_status_quo_risk(),get_intervention_risk())
    })
  
  
  output$dt_countries_level0<-DT::renderDataTable(countries_level0_df())
  output$dt_countries_level1<-DT::renderDataTable(countries_level1_df())
  output$dt_countries_level2<-DT::renderDataTable(countries_level2_df())
  output$dt_countries_level3<-DT::renderDataTable(countries_level3_df())
  
  output$intsim_totalrisk<-
    renderUI({
      withMathJax(HTML(paste0(
        total_risk_text()
      )))
    })
  
  
  render_intsim_page(input, output,sim_world_with_covid_data_statusquo())#can't include everything :/ but we will try to put what we can in here.
  
  
  ######################################################################
  ### Validation table.
  output$cases_at_border_graph <- renderCasesAtBorderGraph(get_status_quo_risk())
  get_daily_nz_data <- reactive({
    daily_nz_data <- get_daily_data(TRUE) %>% filter(Location=="New Zealand")
    return(daily_nz_data)
  })
  
  output$new_zealand_status_quo <- renderPlot({
    #1. do a graph of "monthly total" - do a running total over last 30 days
    daily_nz_data<-get_daily_nz_data()
    daily_nz_data <-  daily_nz_data %>% 
      mutate(NewCasesOverLastMonth = rollapply(NewCasesImportAdjusted,30,sum,align='right',fill=NA))
    graph_data <- daily_nz_data %>% filter(Date>"2020-06-15") %>% 
      select(Date,NewCasesOverLastMonth,ActiveCases) %>%
      tidyr::gather(key="Metric",value="Value",2:3)
    return(
      ggplot(graph_data %>% filter(Date>"2020-06-15"),aes(x=Date,y=Value))+
        geom_line()+
        facet_wrap(~Metric, scales = "free_y",nrow = 2)+
        labs(title="New Zealand recent activity",subtitle = "Active Cases; New cases over the last 30 days (rolling total)"))
  })
  
  
  
  output$validation_description <- renderText({
    
    "
    Here, we compare values to try to check and see whether values from different sources align.
    
    The first thing to check: does the predicted number of cases under \"status quo\" line up with the observed number of cases coming in to the country?
    
    "
  })
  
  output$validation_description_2 <- renderText({
    daily_nz_data <- get_daily_nz_data()
    total_new_cases <- sum(daily_nz_data %>% filter(Date>"2020-06-01") %>% .$NewCases)
    paste0("
    What does that imply for our predicted cases getting into the community?
    Working from the New Zealand rolling new cases graph, we see about 30-40 new cases per month over June to July.
    
    The total number of new cases since June 1 in New Zealand is ",as.character(total_new_cases),".
    
    With 3 border breaches that should work out to an error rate of 3 in ",as.character(total_new_cases),".
    
    ")
  })
  
  ######################################################################
  #map page
  render_map_page(output,get_filtered_mapped_world_with_covid_data(),get_run_month())
  
  ######################################################################
  #SUMMARY map page
  render_summary_map(input = input,output = output,session=session, get_filtered_mapped_world_with_covid_data(),get_run_month())
  
  ######################################################################
  #journey page
  
  render_journey_page(input,output)
}


print_elapsed_time("Creating UI component...")
app_display_title <- "Border relaxation measures"

ui <- navbarPage(
  app_display_title,
  id="mainNavbarPage",
  selected="Intervention simulation",
  footer=div(class = "footer",
             includeHTML("footer.html")
  ),
  tabPanel(
    "Location Profiles",
    fluidPage(
      titlePanel("Location Profiles"),
      textOutput("Location Profile Options"),
      selectInput("locprofile_Location",
                  "Select a location to profile:",
                  choices = key_interest_countries,
                  multiple=FALSE),
      downloadButton("downloadable_report", "Generate report")
    )
  ),
  get_intsim_tabPanel(default_simulation_data,countries_to_choose_from),
  #get_Proposal_tabPanel(default_simulation_data,countries_to_choose_from),
  
  tabPanel(
    "Risk Matrix",
    fluidPage(
      fluidRow(
        column(4,
               titlePanel("COVID-19: Location Risk Matrix")),
        column(2,
               h4("View settings")),
        column(3,
               radioButtons("countrylist_type", 
                            "List Type:", 
                            choices = c(
                              "Classic",
                              "Comprehensive high-level",
                              "Prevalence detail",
                              "Raw"), 
                            selected = "Classic",
                            inline = FALSE
               )
        ),
        column(2,
               numericInput("countrylist_travelerfilter",
                            "Show countries with at least this number of travellers per month:",
                            min=0,max=100000,
                            step=500,
                            value=2000)
        ),
        column(1,
               # Button
               downloadButton("countrylist_downloadcsv", "Download")
        )
      ),
      hr(),
      fluidRow(
        column(12,
               div(DT::dataTableOutput("country_table"), style = "font-size:80%")
        )
      ),
      fluidRow(
        column(12,
               uiOutput("country_table_notes")
        )
      )
    )
  ),
  get_summary_map_tabPanel(default_run_date = default_run_date),
  get_map_page_tabPanel(),
  tabPanel(
    "Validation",
    fluidPage(
      titlePanel("Validation"),
      mainPanel(
        textOutput("validation_description"),
        plotOutput("new_zealand_status_quo"),
        plotOutput("cases_at_border_graph"),
        textOutput("validation_description_2"),
        width = 12
      )
    )
  ),
  get_journey_page_tabPanel(),
  tabPanel(
    "Simulation settings",
    fluidPage(
      useShinyjs(),#have to put this somewhere; I've arbitrarily put it in the simSettings page.
      titlePanel("Simulation Settings"),
      mainPanel(
        # numericInput("intsim_quarantine_failure_odds",
        #              "If someone who arrives in NZ with COVID19 and is quarantined,\nand they exit quarantine, the odds they are still contagious are 1 in ",
        #              min=5,
        #              max=10000,step=10,
        #              value = default_quarantine_failure_odds),
        textOutput("ifr_explanation"),
        numericInput("simsettings_ifr",
                     "Assumed Infection Fatality Rate (%):",
                     min=0,
                     max=5,step=0.1,
                     value = default_assumed_ifr_percent),
        numericInput("simsettings_traveler_relative_prevalence",
                     "Prevalence of COVID-19 in travellers relative to the population:",
                     min=0,
                     max=10,step=0.1,
                     value = default_traveler_relative_prevalence),
        
        numericInput("simsettings_current_lockdown_passenger_volume",
                     "Current monthly incoming passenger volume:",
                     min=0,
                     max=10^5,step=1000,
                     value = default_current_lockdown_passenger_volume),
        dateInput("simsettings_run_date",
                     "Run date:",
                     value = default_run_date,max = Sys.Date()),
        selectInput(inputId = "simsettings_mode",
                    label="Simulation mode",
                    choices = c("Simple","Advanced"),
                    selected = "Simple"
        )
      )
    )
  )
)

print_elapsed_time("Running Shiny Application...")
# Run the application 
shinyApp(ui = ui, server = server)