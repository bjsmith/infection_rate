source("utils.R")
source("simulation.R")
source("country_classification_rules.R")
source("simJourneyPanel.R")
source("defaults.R")
library(ggrepel)
library(DT)

#Ben J:
#Assume 0.5% transmission risk without mask (it’s a conservative worst case rate)
#If mask use reduces risk by 90% as per Arthur’s email then in-flight transmission risk reduced to 0.05%
#Therefore one case expected to be acquired ‘in-flight’ for every 125 infectious cases carried
#default_aircraft_infection_rate <- 0.005 #without mask
#default_aircraft_mask_effectiveness_percent <- 90

#run_date<-as.Date("2020-07-15")#Sys.Date()
#run_date<-as.Date("2020-06-15")#Sys.Date()
run_date<-as.Date("2020-08-15")#Sys.Date()
month_name <- format(run_date,"%B")

# world_with_covid_data,
# treatment_effectiveness,
# extra_spread,
# travel_volume_proportion=1,
# assumed_ifr=0.006,
# traveler_relative_prevalence=1,
geo_world_basic_data <- get_geomapped_covid_data(life_exp_thresh,run_date)
geo_world_with_covid_data <- simulate_treatment_for_countries(geo_world_basic_data,
                                                    treatment_effectiveness = default_assumed_effectiveness,
                                                    extra_spread = 0,
                                                     assumed_ifr = default_assumed_ifr_percent/100,
                                                     
                                                     traveler_relative_prevalence=default_traveler_relative_prevalence,
                                                     current_lockdown_passenger_volume = default_current_lockdown_passenger_volume
                                                     )

nogeo_world_basic_data <- get_geomapped_covid_data(life_exp_thresh,run_date,separate_aussie_states_and_hk = TRUE,include_geo_data = FALSE)
default_simulation_data <- simulate_treatment_for_countries(
  nogeo_world_basic_data,
  treatment_effectiveness = default_assumed_effectiveness,
  extra_spread = 0,
  assumed_ifr = default_assumed_ifr_percent/100,
  traveler_relative_prevalence=default_traveler_relative_prevalence,
  current_lockdown_passenger_volume = default_current_lockdown_passenger_volume)

#save.image("environ.RData")
#load("environ.RData")

small_country_health_data <-data.frame(
  "Country"=c("Samoa", "Cook Islands"),
  "Website"=c("https://www.health.gov.ws/", "https://www.health.gov.ck/")
)


# display_table<-world_with_covid_data[vals_to_include,]
######set up general simulator

countries_to_choose_from<-
  nogeo_world_basic_data$Location %>%
  sort %>%
  .[.!="New Zealand"]

source("key_interest_countries.R")

get_intsim_dt<-function(
  country_filter,
  #selected_probs="bubble",
  world_w_covid_data#,quarantine_odds_override,travel_volume_proportion=1
                        ){

  
  filtered_df <- world_w_covid_data %>%
    data.frame %>%
    filter(LifeExp>=life_exp_thresh) %>%
    filter(Location %in% country_filter) %>%
    arrange(InfActiveCasesPerMillion)
  
  #percentage_cols=c()
  rounding_cols=c("ExpectedCasesAtBorderUnderLockdown","ExpectedCasesAtBorder","ExpectedNumberOfCasesInCommunity")
  rounding_0_cols=c("StatusQuoMonthlyArrivals","MonthlyArrivalsWeighted")
  dt_colnames <- c("Location",
                   "Status quo: pax per month",
                   "Status quo: Expected Infections At Border",
                   "Intervention: pax per month",
                   "Intervention: Expected Infections At Border",
                   "Intervention: Expected infections still infectious when reaching community")
  
  df_to_return<- filtered_df %>%
    select(Location,
           StatusQuoMonthlyArrivals,ExpectedCasesAtBorderUnderLockdown,
           MonthlyArrivalsWeighted,ExpectedCasesAtBorder,ExpectedNumberOfCasesInCommunity
    )


  return(DT::datatable(df_to_return,colnames=dt_colnames)%>%
           #formatPercentage(percentage_cols,3) %>%
           formatRound(rounding_cols,2) %>%
           formatRound(rounding_0_cols,0)
  )
}

###########create data table for table tab

#lose geometry info and convert to datatable. note: not the same as data.table!

###########main dashboard.
source("map_page.R")
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
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
  
  generate_world_with_covid_data <- reactive({
    get_intervention_risk()
  })
  
  generate_mapped_world_with_covid_data <- reactive({
    simulate_treatment_for_countries(
      geo_world_basic_data,
      treatment_effectiveness = input$intsim_effectiveness_level1/100,
      extra_spread = default_assumed_spread,
      assumed_ifr = input$simsettings_ifr/100,
      traveler_relative_prevalence=input$simsettings_traveler_relative_prevalence,
      current_lockdown_passenger_volume = input$simsettings_current_lockdown_passenger_volume)
  })
  
  get_filtered_mapped_world_with_covid_data <- reactive({
    generate_mapped_world_with_covid_data() %>% 
      filter(LifeExp>=life_exp_thresh)
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
          DataReliablityRating,#add
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
          DataReliablityRating
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
  
  
  #country list page
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
      paste("risk_matrix_", run_date, ".csv", sep = "")
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
  #intervention simulation page
  
  output$intsim_notes<-
    renderUI({
      withMathJax(HTML(paste0("
<h4>Notes:</h4>
<br /><br />
Refer to the 'Simulation settings' tab for more options.

")))})
  
  
  # world_with_covid_data,
  # treatment_effectiveness,
  # extra_spread,
  # travel_volume_proportion=1,
  # assumed_ifr=0.006,
  # traveler_relative_prevalence=1,
  # current_lockdown_passenger_volume=NULL


  sim_geo_world_with_covid_data_level0 <- reactive({
    world_w_covid_data <- simulate_treatment_for_countries(
      nogeo_world_basic_data,
      treatment_effectiveness = input$intsim_effectiveness_level0/100,
      extra_spread = input$intsim_extraspread_level0/100,
      travel_volume_proportion = input$intsim_percent_tvolume_level0/100,
      assumed_ifr = input$simsettings_ifr/100,
      traveler_relative_prevalence=input$simsettings_traveler_relative_prevalence,
      current_lockdown_passenger_volume = input$simsettings_current_lockdown_passenger_volume)
    return(world_w_covid_data)
  })

  
  sim_geo_world_with_covid_data_level1 <- reactive({
    world_w_covid_data <- simulate_treatment_for_countries(
      nogeo_world_basic_data,
      treatment_effectiveness = input$intsim_effectiveness_level1/100,
      extra_spread = input$intsim_extraspread_level1/100,
      travel_volume_proportion = input$intsim_percent_tvolume_level1/100,
      assumed_ifr = input$simsettings_ifr/100,
      traveler_relative_prevalence=input$simsettings_traveler_relative_prevalence,
      current_lockdown_passenger_volume = input$simsettings_current_lockdown_passenger_volume)
    return(world_w_covid_data)
  })
  
  
  #basically NZ citizens only.
  sim_geo_world_with_covid_data_statusquo <- reactive({
    world_w_covid_data <- simulate_treatment_for_countries(
      nogeo_world_basic_data,
      treatment_effectiveness = input$intsim_effectiveness_level1/100,
      extra_spread = input$intsim_extraspread_level1/100,
        travel_volume_proportion=0,
      assumed_ifr = input$simsettings_ifr/100,
      traveler_relative_prevalence=input$simsettings_traveler_relative_prevalence,
      current_lockdown_passenger_volume = input$simsettings_current_lockdown_passenger_volume)
    return(world_w_covid_data)
    
  })
  
  countries_level0_df <- reactive({
    return(get_intsim_dt(input$intsim_countries_level0,
                         sim_geo_world_with_covid_data_level0())
    )
  })
  
  countries_level1_df <- reactive({
    return(get_intsim_dt(input$intsim_countries_level1,
                         sim_geo_world_with_covid_data_level1())
    )
  })

  # countries_level2_df <- reactive({
  #   return(get_intsim_dt(input$intsim_countries_level2,
  #                        sim_geo_world_with_covid_data_level2())
  #   )
  # })
  # 
  # countries_level3_df <- reactive({
  #   return(get_intsim_dt(input$intsim_countries_level3,
  #                        sim_geo_world_with_covid_data_level3())
  #   )
  # })
  
  # 
  # get_countries_out_of_bubble_risks <- reactive({
  #   sim_geo_world_with_covid_data_quarantine() %>%
  #     data.frame %>%
  #     filter(LifeExp>=life_exp_thresh) %>%
  #     filter(Location %in% input$intsim_countries_quarantine)
  #   #use the lower "community cases" figure here because these are going through quarantine.
  # })
  
  #these are untrusted countries, but NZ residents are ALWAYS allowed to return from these ones.
  get_countries_out_of_bubble_untrusted_risks <- reactive({
    #but only pass forward countries that we haven't actually selected
    sim_geo_world_with_covid_data_statusquo() %>%
      data.frame %>%
      filter(
        (LifeExp<life_exp_thresh) | 
          (
            ((Location %in% input$intsim_countries_quarantine)==FALSE) & 
            ((Location %in% input$intsim_countries_bubble)==FALSE)
          )
        ) 
  })
  
  get_status_quo_risk <- reactive({
    #just all countries, but we'll only get expected cases from NZ Residents
    status_quo_risk <- 
      sim_geo_world_with_covid_data_statusquo() %>%
      mutate(
        InterventionLevel=NA,
        InterventionLabel="None")# %>%
      # select(Location,
      #        ExpectedNumberOfCasesInCommunity,
      #        ExpectedCasesAtBorder
      # )
    return(status_quo_risk)
  })
  
  get_intervention_risk <- reactive({
    countries_in_intervention <- c(
      input$intsim_countries_level0,
      input$intsim_countries_level1,
      input$intsim_countries_level2,
      input$intsim_countries_level3
      )
    status_quo_countries <- get_status_quo_risk()%>% 
      filter((Location %in% countries_in_intervention)==FALSE) %>%
      mutate(
        InterventionLevel=NA,
        InterventionLabel="None")
    
    intervened_countries_risk <- rbind(
      sim_geo_world_with_covid_data_level0() %>%
        filter(Location %in% input$intsim_countries_level0) %>%
        mutate(
          InterventionLevel=0),
      sim_geo_world_with_covid_data_level1() %>%
        filter(Location %in% input$intsim_countries_level1) %>%
        mutate(
          InterventionLevel=1)#,
      # sim_geo_world_with_covid_data_level2() %>%
      #   filter(Location %in% input$intsim_countries_level2) %>%
      #   mutate(
      #     InterventionLevel=2),
      # sim_geo_world_with_covid_data_level3() %>%
      #   filter(Location %in% input$intsim_countries_level3) %>%
      #   mutate(
      #     InterventionLevel=3)
    ) %>% rowwise() %>% mutate(InterventionLabel=get_intervention_name(InterventionLevel))
    
    intervention_risk <- 
      rbind(status_quo_countries ,
            intervened_countries_risk
      )
    
    return(intervention_risk)
  })
  
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
  
  output$cases_at_border_graph <- renderPlot({
    status_quo_risk <- get_status_quo_risk()
    status_quo_risk$Condition<-"Status Quo"
    
    combined_risk_graph <- status_quo_risk
    combined_risk_graph <- combined_risk_graph%>% filter(!is.na(ExpectedCasesAtBorder))
    
    misc_countries_label <- "Other locations"
    
    combined_risk_graph<- 
      combined_risk_graph %>% 
      mutate(LocationLabel = 
               case_when(
                 (ExpectedCasesAtBorder<0.1) ~ misc_countries_label,
                 TRUE ~ Location
               ))
    
    
    
    combined_risk_graph$LocationLabel <-
      factor(combined_risk_graph$LocationLabel,
             levels = unique(combined_risk_graph$LocationLabel),
             ordered=TRUE)
    
    #combine across grouped categories
    combined_risk_graph<-
      combined_risk_graph %>% 
      group_by(LocationLabel,Condition) %>%
      summarise(ExpectedCasesAtBorder=sum(ExpectedCasesAtBorder)) %>%
      arrange(Condition,desc(LocationLabel))
    
    #do the cumulative sum
    combined_risk_graph <-
      combined_risk_graph %>%
      group_by(Condition) %>%
      arrange(desc(LocationLabel)) %>%
      mutate(LabelPosition=cumsum(ExpectedCasesAtBorder)-ExpectedCasesAtBorder/2)
    
    location_labels_in_use <- length(unique(combined_risk_graph$LocationLabel))
    
    #create color palette
    color_palette<- c()
    if (misc_countries_label %in% combined_risk_graph$LocationLabel){
      color_palette <- c(color_palette ,'#555555')
    }
    color_palette = c(
      color_palette,
      rep(
        c('#1f78b4','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#6a3d9a','#b15928'),
        ceiling(location_labels_in_use/8))[1:location_labels_in_use]
    )
    
    plot_max <- sum(combined_risk_graph$ExpectedCasesAtBorder)
    ggplot(combined_risk_graph,aes(x=Condition,y=ExpectedCasesAtBorder,fill=LocationLabel,label=LocationLabel
    ))+
      geom_bar(stat="identity",alpha=0.8)+
      scale_x_discrete(name="")+
      scale_y_continuous(name="Expected cases per month at border",
                         #breaks=0:plot_max,
                         limits = c(0,plot_max)#,
                         #limits=c(0,20),
                         #position="right"
      )+
      scale_fill_manual(values = color_palette)+
      theme(legend.position = "none",legend.box="vertical",legend.margin=margin(),text=element_text(face = "bold"),
            axis.text = element_text(size=16)
      )+
      geom_label_repel(aes(y=LabelPosition),color="white",fontface="bold")+
      coord_flip()
    
    
  })

  
  output$total_risk_graph <- renderPlot({
    #foreign_risk <- get_foreign_risk()
    status_quo_risk <- get_status_quo_risk()
    #status_quo_risk <- status_quo_risk %>% select(-ExpectedCasesAtBorder) #we won't be using this in the total risk graph.
    intervention_risk <- get_intervention_risk()# %>%
      # select(Location,
      #        ExpectedNumberOfCasesInCommunity,
      #        ExpectedCasesAtBorder
      # )
    

    #nz_resident_risk_label <- nz_resident_risk_df$Location[[1]]
    nz_res_only_label <- "Status quo restricted locations"
    # bubble_other_label <- "Other Bubble Locations"
    # quarantine_other_label <- "Other Quarantine Locations"
    # l2_screener_other_label <- "Other Screened Locations"
    other_label <- "Other Locations"
    
    #we'll pool risk from all countries that haven't been selected, for both status quo and intervention categories.
    selected_locations <- unique(c(
      input$intsim_countries_level0,
      input$intsim_countries_level1#,
      #input$intsim_countries_level2,
      #input$intsim_countries_level3
    ))
    
    intervention_risk$Condition<-"Intervention"
    
    status_quo_risk$Condition<-"Status Quo"
    
    combined_risk_graph <- rbind(status_quo_risk,intervention_risk) %>% 
      arrange(Location)
    
    warning(paste0(
      "data incomplete for",
      paste0(combined_risk_graph %>% filter(is.na(ExpectedCasesAtBorder)) %>% select(Location) %>% unique,collapse=", ")
            ))
    
    
    
    
    combined_risk_graph <- combined_risk_graph%>% filter(!is.na(ExpectedCasesAtBorder))
    

    
    #combined_risk_graph$Location[is.na(combined_risk_graph$Location)]<-nz_res_only_label
    combined_risk_graph<- 
      combined_risk_graph %>% 
      mutate(LocationLabel = 
        case_when(
        # (Location %in% input$intsim_countries_level0) & (ExpectedCasesAtBorder<0.01) ~ bubble_other_label,
        # (Location %in% input$intsim_countries_level1) & (ExpectedCasesAtBorder<0.01) ~ l2_screener_other_label,
        # (Location %in% input$intsim_countries_level2) & (ExpectedCasesAtBorder<0.01) ~ quarantine_other_label,
          ((Location %in% selected_locations)==FALSE) ~ nz_res_only_label,
          (ExpectedNumberOfCasesInCommunity<0.01)   ~ other_label,
        TRUE ~ Location
      ))
    
    combined_risk_graph$LocationLabel <-
      factor(combined_risk_graph$LocationLabel,
             #levels = c(bubble_other_label,l2_screener_other_label,quarantine_other_label,selected_locations,nz_res_only_label),
             levels = c(other_label,selected_locations,nz_res_only_label),
             ordered=TRUE)
    
    
    #combine across grouped categories
    combined_risk_graph<-
      combined_risk_graph %>% 
      group_by(LocationLabel,Condition) %>%
      summarise(ExpectedNumberOfCasesInCommunity=sum(ExpectedNumberOfCasesInCommunity)) %>%
      arrange(Condition,desc(LocationLabel))
    
    #do the cumulative sum
    combined_risk_graph <-
      combined_risk_graph %>%
      group_by(Condition) %>%
      arrange(desc(LocationLabel)) %>%
      mutate(LabelPosition=cumsum(ExpectedNumberOfCasesInCommunity)-ExpectedNumberOfCasesInCommunity/2)
    
    location_labels_in_use <- length(intersect(selected_locations,combined_risk_graph$LocationLabel))
    
    #create color palette
    color_palette<- c()

    if (other_label %in% combined_risk_graph$LocationLabel){
      color_palette <- c(color_palette ,'#555555')
    }
    if(location_labels_in_use>0){
      color_palette = c(
        color_palette,
        rep(
          c('#1f78b4','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#6a3d9a','#b15928'),
          ceiling(location_labels_in_use/8))[1:location_labels_in_use]
      )
    }
    color_palette = c(color_palette,'#222222') #NZ residents only
    
    #plot_max <- sum(combined_risk_graph$ExpectedNumberOfCasesInCommunity)
    ggplot(combined_risk_graph,aes(x=Condition,y=ExpectedNumberOfCasesInCommunity,fill=LocationLabel,label=LocationLabel
                                   ))+
      geom_bar(stat="identity",alpha=0.8)+
      scale_x_discrete(name="")+
      scale_y_continuous(name="Expected cases per month",
                         #breaks=0:plot_max,
                         minor_breaks = NULL 
                         #limits = c(0,plot_max)#,
                         #limits=c(0,20),
                         #position="right"
                         )+
      #scale_fill_brewer(palette="Set3")+
      scale_fill_manual(values = color_palette)+
      theme(legend.position = "none",legend.box="vertical",legend.margin=margin(),text=element_text(face = "bold"),
            axis.text = element_text(size=16)
            )+
      #guides(fill=guide_legend(nrow=2,byrow=TRUE))+
      geom_label_repel(aes(y=LabelPosition),color="white",fontface="bold")+
      coord_flip()
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
  
  
  total_risk_text <- reactive({
    
    countries_excluded_due_to_data<-
      sim_geo_world_with_covid_data_level0() %>%
      data.frame %>%
      #filter((name_long %in% input$intsim_countries_bubble) | name_long %in% (input$intsim_countries_qurantine)) %>%
      filter(Location %in% 
               c(input$intsim_countries_bubble,
                 input$intsim_countries_level1)) %>%
      filter(LifeExp<life_exp_thresh)
    
    
    #for countries that are let in without quarantine, we want to add 
    #countries_in_bubble_risks <- get_countries_in_bubble_risks()$ProbabilityOfMoreThanZeroCases
    #countries_screener_risks <- get_countries_level2_risks()$ProbabilityOfMoreThanZeroCommunityCases
    #countries_out_of_bubble_risks <- get_countries_out_of_bubble_risks()$ProbabilityOfMoreThanZeroCommunityCases
    
    #untrusted_countries_risks <- get_countries_out_of_bubble_untrusted_risks()$ProbabilityOfMoreThanZeroCommunityCases
    
    # foreign_risk <- get_foreign_risk()
    # nz_resident_risk <- get_intervention_risk()
    intervention_risk <- get_intervention_risk()
    status_quo_risk <- get_status_quo_risk()
    increased_risk <- sum(intervention_risk$ExpectedNumberOfCasesInCommunity,na.rm = TRUE)/sum(status_quo_risk$ExpectedNumberOfCasesInCommunity,na.rm = TRUE)-1
    
    #now...
    #how do we combine these? 
    #it is not as simple as adding each up. 
    #I think we have to multiply the complements then get the complement again.
    #total_risk_prop<-1-prod(1-na.exclude(c(countries_in_bubble_risks,countries_out_of_bubble_risks,untrusted_countries_risks)))
    
    #now we need to add a warning for excluded countries.
    #total_risk_prop
    textout<-paste0(
      "In the status quo where only NZ residents are allowed can enter, we estimate ",
      signif(sum(status_quo_risk$ExpectedCasesAtBorder,na.rm = TRUE),2),
      " cases per month will arrive at the border, of which ",
      signif(sum(status_quo_risk$ExpectedNumberOfCasesInCommunity,na.rm = TRUE),2),
      " will be exposed to the community.",
      "<br /> <br />",
      "In the specified intervention, we estimate ",
      signif(sum(intervention_risk$ExpectedCasesAtBorder,na.rm = TRUE),2),
      " cases per month will arrive at the border, of which ",
      signif(sum(intervention_risk$ExpectedNumberOfCasesInCommunity,na.rm = TRUE),2),
      " will be exposed to the community.",
      " The intervention increases the expected amount of community exposure by ",
      scales::percent(increased_risk,accuracy = 0.01),
      ".<br /> <br />",
      #"The status quo risk of exposing the community to 1 or more cases over a 1-month period is ",
      #scales::percent(total_risk_prop,accuracy = 0.01),
      #".\n\n"
      "Community exposure could be anything from one very brief encounter (e.g., stopping for directions) to an infected individual entering the community undetected."
                    )
    
    if(length(countries_excluded_due_to_data$Location)>0){
      textout<-paste0(textout,
"<br /><br /> COVID-19 Data from the following countries is considered less reliable. 
NZ resident returnees from these countries are already allowed, but we caution against relying on this data for anything further: " ,
paste0(countries_excluded_due_to_data$Location,collapse = ", "))
    }
    
    
    
    textout<-paste0(textout, "<br /> <br /><em>Cook Islands</em> and <em>Western Samoa</em> are not explicitly modeled. International sources do not track these locations and they are currently rated zero-risk, due to their COVID-free status and low or zero inbound travel. The situation should be continually monitored for any changes to their current zero-risk status.")
    
    return(textout)
  })
  

  
  output$dt_countries_level0<-DT::renderDataTable(
    countries_level0_df() 
  )
  
  output$dt_countries_level1<-DT::renderDataTable(
    countries_level1_df() 
  )
  
  
  # output$dt_countries_level2<-DT::renderDataTable(
  #   countries_level2_df() 
  #   
  # )
  # output$dt_countries_level3<-DT::renderDataTable(
  #   countries_level3_df() 
  #   
  # )
  # 
  output$intsim_totalrisk<-
    renderUI({
      withMathJax(HTML(paste0(
        total_risk_text()
      )))
    })
  
  output$intsim_level0_header <- simJourneyPanelHeader(0)
  output$intsim_level1_header <- simJourneyPanelHeader(1)

  
  observeEvent(input$intsim_20countries,{
    print("reacting")
    #let's get the risk for the 20 countries,
    #and apply them into these categories as appropriate.
    
    intervention_risk <- get_intervention_risk()
    intervention_risk_keycountries <- 
      intervention_risk %>% filter(Location %in% key_interest_countries)
    
    
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
        intervention_risk_keycountries %>% filter(PrevalenceRating %in% c("Very low","Low","Moderate")) %>% .$Location
    )
    
    })
  
  
  #map page
  output$testt<-
    renderUI({
      withMathJax(HTML(
        map_page_notes(month_name,life_exp_thresh)
        ))
    })
  
  output$graph0header<-renderText({"Figure 1: Reported active cases now (average past 7 days)"})
  output$graph0 <- renderLeaflet({
    show_leaflet(data_to_show = get_filtered_mapped_world_with_covid_data() %>% filter(!is.na(ActiveCases)),
                 primary_col = "ActiveCases",
                 rounding_func = function(x){scales::comma(round(x,1))},
                 legend_title =  "Observed active cases",
                 quant_grades = 4,
                 pal_reverse = FALSE
    )
    
  })
  output$graph1header<-renderText({"Figure 2: Hit rate or inferred case detection rate"})
  output$graph1 <- renderLeaflet({
    show_leaflet(data_to_show = get_filtered_mapped_world_with_covid_data() %>% filter(!is.na(InferredDetectionRate)),
                 primary_col = "InferredDetectionRate",
                 rounding_func = function(x){scales::percent(x,accuracy = 0.1)},
                 quant_grades = 3,
                 legend_title = "Inferred detection rate <br /> (current deaths over cases three weeks prior; <br /> countries with a life expectancy of 75 or greater)")
    
  })
  output$graph2header<-renderText({"Figure 3: Inferred active cases today"})
  output$graph2 <- renderLeaflet({
    show_leaflet(data_to_show = get_filtered_mapped_world_with_covid_data() %>% filter(!is.na(InferredActiveCases)),
                 primary_col = "InferredActiveCases",
                 rounding_func = function(x){scales::comma(round(x,1))},
                 legend_title =  "Inferred active cases<br /> (likely to underestimate in countries<br /> with poor death recording)",
                 quant_grades = 4,
                 pal_reverse = FALSE
    )
    
  })
  output$graph3header<-renderText({"Figure 4: Active cases per million"})
  output$graph3<-renderLeaflet({
    
    show_leaflet(data_to_show = get_filtered_mapped_world_with_covid_data() %>% filter(!is.na(InfActiveCasesPerMillion)),
                 primary_col = "InfActiveCasesPerMillion",
                 rounding_func = function(x){scales::comma(signif(x,2))},
                 legend_title =  "Inferred active cases per million people <br /> (likely to underestimate in countries<br /> with poor death recording)",
                 quant_grades = 4,
                 pal_reverse = FALSE
    )
  })
  
  output$graph4header<-renderText({"Figure 5: New Zealand arrivals by month (2019 figures)"})
  output$graph4<-renderLeaflet({
    
    show_leaflet(data_to_show = get_filtered_mapped_world_with_covid_data() %>% filter(!is.na(Total2019MonthlyArrivals)),
                 primary_col = "Total2019MonthlyArrivals",
                 rounding_func = function(x){scales::comma(signif(x,3))},
                 legend_title =  "NZ arrivals by month",
                 quant_grades = 5,
                 pal_reverse = FALSE
    )
  })
  output$graph5header<-renderText({paste0(
    "Figure 6: Probability of one or more cases arriving each month from residents of each country, based on arrival figures from this country in ",
    month_name,
    " 2019.")})
  output$graph5<-renderLeaflet({

    #get the maximum number of bins we can have, considering the distribution of the data
    display_data <- get_filtered_mapped_world_with_covid_data() %>% filter(!is.na(ProbabilityOfMoreThanZeroCases))
    col_of_interest <-"ProbabilityOfMoreThanZeroCases"

    show_leaflet(data_to_show =  display_data ,
                 primary_col = col_of_interest,
                 rounding_func = function(x){scales::percent(x,accuracy = 0.01)},
                 legend_title =  "Probability of more than zero cases arriving to New Zealand",
                 quant_grades = get_max_quantiles(display_data,col_of_interest,max_quantiles=8),
                 pal_reverse = FALSE
    )
  })
  output$graph6header<-renderText({paste0(
    "Figure 7: Expected number of cases to arrive each month from residents of each country, based on arrival figures from this country in ",
    month_name,
    "2019.")})
  output$graph6<-renderLeaflet({
    
    display_data <- get_filtered_mapped_world_with_covid_data() %>% filter(!is.na(ExpectedCasesAtBorder))
    col_of_interest <-"ExpectedCasesAtBorder"
    
    
    show_leaflet(data_to_show = display_data,
                 primary_col = col_of_interest,
                 rounding_func = function(x){scales::comma(signif(x,2))},
                 legend_title =  "Expected number of cases to arrive each month",
                 quant_grades = get_max_quantiles(display_data,col_of_interest,max_quantiles=8),
                 pal_reverse = FALSE
    )
  })
  # output$graph7header<-renderText({paste0(
  #   "Figure 8: Probability of one or more cases arrives and is quarantined but reaches the community, based on arrival figures from this country in ",
  #   month_name,
  #   " 2019.")})
  # output$graph7<-renderLeaflet({
  #   
  #   display_data <- get_filtered_mapped_world_with_covid_data() %>% filter(!is.na(ProbabilityOfMoreThanZeroCommunityCases))
  #   col_of_interest <- "ProbabilityOfMoreThanZeroCommunityCases"
  #   show_leaflet(data_to_show = display_data,
  #                primary_col = col_of_interest,
  #                rounding_func = function(x){scales::percent(x,accuracy = 0.01)},
  #                legend_title =  "insert legend title",
  #                quant_grades = get_max_quantiles(display_data,col_of_interest,max_quantiles=8),
  #                pal_reverse = FALSE
  #   )
  # })
  output$graph8header<-renderText({paste0(
    "Figure 8: Expected number of cases to arrive and be quarantined but still reach the community, based on arrival figures from this country in ",
    month_name,
    "2019.")})
  output$graph8<-renderLeaflet({
    
    display_data <- get_filtered_mapped_world_with_covid_data() %>% filter(!is.na(ExpectedNumberOfCasesInCommunity))
    col_of_interest <- "ExpectedNumberOfCasesInCommunity"
    
    binpal <- colorBin("YlOrRd", 
                       display_data$ExpectedNumberOfCasesInCommunity, 
                       domain=c(0,max(display_data$ExpectedNumberOfCasesInCommunity,na.rm = TRUE)),
                       bins=c(0.001,0.01,0.1,1,10,min(100,max(display_data$ExpectedNumberOfCasesInCommunity,na.rm = TRUE)), pretty = FALSE)
    )
    
    show_leaflet(data_to_show = display_data,
                 primary_col = col_of_interest,
                 rounding_func = function(x){scales::comma(signif(x,2))},
                 legend_title =  "Expected number of cases to arrive each month",
                 custom_palette = binpal
                 
    )
  })
  
  output$ifr_explanation <- renderText({
    paste0(
      "Where there are recent COVID fatalities, this is compared to confirmed cases two weeks prior. ",
      "If the number of cases look too low, then a 'hit rate' is calculated by comparing those cases two weeks prior to fatalities now.",
      "Then, inferred active cases now is caclculted by dividing the confirmed active cases by the hit rate.",
      "To do this we need to assume an infection fatality rate (IFR)."
      
    )
  })
  
  
}

ui <- navbarPage(
  "Opening the border: What's the risk?",
  selected="Intervention simulation",
  tabPanel(
    "Location Profiles",
    fluidPage(
      titlePanel("Location Profiles"),
      # sidebarLayout(
      #   sidebarPanel(
            textOutput("Location Profile Options"),
            selectInput("locprofile_Location",
                        "Select a location to profile:",
                        choices = key_interest_countries,
                        multiple=FALSE),
            downloadButton("downloadable_report", "Generate report")
          # )
        # ,
        # mainPanel(
        #   htmlOutput("onscreen_report")
        # )
      # )
    )
  ),
  tabPanel(
    "Intervention simulation",
    fluidPage(
      titlePanel("Intervention simulation"),
      sidebarLayout(
        sidebarPanel(
          actionButton("intsim_20countries",
                       "Set to 20 country reference list",
                       class="btn btn-primary"),
          get_simJourneyPanel_from_level_id(0,choices= countries_to_choose_from,selected = 
                                              default_simulation_data %>% 
                                              filter(Location %in% key_interest_countries & PrevalenceRating %in% "COVID-free") %>% .$Location),
          get_simJourneyPanel_from_level_id(1,choices= countries_to_choose_from,
                                            selected=default_simulation_data %>% 
                                              filter(Location %in% key_interest_countries & (PrevalenceRating %in% 
                                                       c("Very low","Low","Moderate"))) %>% .$Location),
          uiOutput("intsim_notes")
        ),
        mainPanel(
          titlePanel("Total risk per month"),
          uiOutput("intsim_totalrisk"),
          #plotOutput("cases_at_border_graph"),
          plotOutput("total_risk_graph"),
          titlePanel("COVID-free countries (no risk)"),
          DT::dataTableOutput("dt_countries_level0"),
          titlePanel("Other countries (low risk)"),
          DT::dataTableOutput("dt_countries_level1"),
          # titlePanel("Level 2 countries (medium risk)"),
          # DT::dataTableOutput("dt_countries_level2"),
          # titlePanel("Level 3 countries (high risk)"),
          # DT::dataTableOutput("dt_countries_level3")
        )
      )
    )
  ),

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
                            "Show countries with at least this number of travelers per month:",
                            min=0,max=100000,
                            step=500,
                            value=500)
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
  tabPanel(
   "Method and assumptions",
   
   fluidPage(
     # Application title
     titlePanel("COVID-19: Prevalence around the world"),
     
     # Sidebar with a slider input for number of bins 
     sidebarLayout(
       sidebarPanel(
         uiOutput("testt")
         
       ),
       
       # Show a plot of the generated distribution
       mainPanel(
         textOutput("graph0header"),
         leafletOutput("graph0"),
         textOutput("graph1header"),
         leafletOutput("graph1"),
         textOutput("graph2header"),
         leafletOutput("graph2"),
         textOutput("graph3header"),
         leafletOutput("graph3"),
         textOutput("graph4header"),
         leafletOutput("graph4"),
         textOutput("graph5header"),
         leafletOutput("graph5"),
         textOutput("graph6header"),
         leafletOutput("graph6"),
         # textOutput("graph7header"),
         # leafletOutput("graph7"),
         textOutput("graph8header"),
         leafletOutput("graph8")
       )
     )
   )),
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
  tabPanel(
    "Simulation settings",
    fluidPage(
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
        # numericInput("simsettings_ifr",
        #              "Aircraft infection rate (expected number of cases acquired 'in-flight' per pre-existing case):",
        #              min=0,
        #              max=10,step=0.001,
        #              value = default_aircraft_infection_rate),
        # numericInput("simsettings_maskuse",
        #              "Reduction in aircraft infections from mask use (%):",
        #              min=0,
        #              max=100,step=1,
        #              value = default_aircraft_mask_effectiveness_percent),
        numericInput("simsettings_traveler_relative_prevalence",
                     "Prevalence of COVID-19 in travelers relative to the population:",
                     min=0,
                     max=10,step=0.1,
                     value = default_traveler_relative_prevalence),

        numericInput("simsettings_current_lockdown_passenger_volume",
                     "Current monthly incoming passenger volume:",
                     min=0,
                     max=10^5,step=1000,
                     value = default_current_lockdown_passenger_volume)


      )
    )
  )


)

# Run the application 
shinyApp(ui = ui, server = server)

