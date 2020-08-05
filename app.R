
library(DT)
source("utils.R")
source("simulation.R")
source("country_classification_rules.R")
library(ggrepel)

life_exp_thresh <- 70
default_assumed_ifr_percent<-0.6
default_quarantine_failure_odds<-12
default_assumed_sensitivity <- 0.3
default_general_travel_rate <- 0.2

#Ben J:
#Assume 0.5% transmission risk without mask (it’s a conservative worst case rate)
#If mask use reduces risk by 90% as per Arthur’s email then in-flight transmission risk reduced to 0.05%
#Therefore one case expected to be acquired ‘in-flight’ for every 125 infectious cases carried
default_aircraft_infection_rate <- 0.005 #without mask
default_aircraft_mask_effectiveness_percent <- 90

run_date<-Sys.Date()
month_name <- format(run_date,"%B")

geo_world_basic_data <- get_geomapped_covid_data(life_exp_thresh,run_date)
geo_world_with_covid_data <- get_analysis_covid_data(geo_world_basic_data,
                                                     screening_sensitivity = default_assumed_sensitivity,
                                                     assumed_ifr = default_assumed_ifr_percent/100,
                                                     quarantine_odds_override=(1/default_quarantine_failure_odds))

nogeo_world_basic_data <- get_geomapped_covid_data(life_exp_thresh,run_date,separate_aussie_states_and_hk = TRUE,include_geo_data = FALSE)
# world_with_covid_data <- get_analysis_covid_data(nogeo_world_basic_data,assumed_ifr = default_assumed_ifr_percent/100,
#                                                   quarantine_odds_override=(1/default_quarantine_failure_odds))

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
  selected_probs="bubble",
  world_w_covid_data#,quarantine_odds_override,general_travel_rate=1
                        ){

  
  filtered_df <- world_w_covid_data %>%
    data.frame %>%
    filter(LifeExp>=life_exp_thresh) %>%
    filter(Location %in% country_filter) %>%
    arrange(InfActiveCasesPerMillion)
  
  if(selected_probs=="bubble"){
    df_to_return <- filtered_df %>%
      select(Location,ProbabilityOfMoreThanZeroCases,
           ExpectedNumberOfCasesAll
    )
    percentage_cols <-c('ProbabilityOfMoreThanZeroCases')
    rounding_cols <-c('ExpectedNumberOfCasesAll')#,'ExpectedNumberOfCasesUnderNZResidentQuarantine')
    dt_colnames<-c("Territory","Probability of 1 or more cases","Expected cases")
  }
  
  if(selected_probs=="screening"){
    df_to_return <- filtered_df %>%
      select(Location,ProbabilityOfMoreThanZeroCases,
             ExpectedNumberOfCasesAll,
             ExpectedNumberOfCasesEscapingOneScreen,ExpectedNumberOfCasesEscapingTwoScreens
      )
    percentage_cols <-c('ProbabilityOfMoreThanZeroCases')
    rounding_cols <-c('ExpectedNumberOfCasesAll',"ExpectedNumberOfCasesEscapingOneScreen","ExpectedNumberOfCasesEscapingTwoScreens")#,'ExpectedNumberOfCasesUnderNZResidentQuarantine')
    dt_colnames<-c("Territory","Probability of 1 or more cases","Expected cases at border","Expected cases passing a screening","Expected cases passing two independent screenings")
  }
  
  if(selected_probs=="quarantine"){
    df_to_return<- filtered_df %>%
    select(Location,ProbabilityOfMoreThanZeroCases,ProbabilityOfMoreThanZeroCommunityCases,
           ExpectedNumberOfCasesUnderLockdown,ExpectedNumberOfCasesAll,ExpectedNumberOfCasesInCommunity
    )
    percentage_cols <-c('ProbabilityOfMoreThanZeroCases','ProbabilityOfMoreThanZeroCommunityCases')
    rounding_cols <-c('ExpectedNumberOfCasesUnderLockdown','ExpectedNumberOfCasesAll','ExpectedNumberOfCasesInCommunity')
    dt_colnames<-c("Territory","Probability of 1 or more cases\n arriving in quarantine",
                   "Probability of 1 or more cases\n reaching community",
                   "Expected cases in quarantine at status quo","Expected cases in quarantine", "Expected cases reaching community")
  }

  return(DT::datatable(df_to_return,colnames=dt_colnames)%>%
           formatPercentage(percentage_cols,3) %>%
           formatRound(rounding_cols,2))
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
        location_info$ExpectedNumberOfCasesAll
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
    get_analysis_covid_data(
      nogeo_world_basic_data,
      screening_sensitivity = input$intsim_sensitivity_level2_control,
      quarantine_odds_override=(1/input$intsim_quarantine_failure_odds),
      general_travel_rate = default_general_travel_rate,
      assumed_ifr = input$simsettings_ifr/100)
  })
  
  generate_mapped_world_with_covid_data <- reactive({
    get_analysis_covid_data(
      geo_world_basic_data,
      screening_sensitivity = input$intsim_sensitivity_level2_control,
      quarantine_odds_override=(1/input$intsim_quarantine_failure_odds),
      assumed_ifr = input$simsettings_ifr/100)
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
          MonthlyArrivalsWeighted,#ProbabilityOfMoreThanZeroCases,ProbabilityOfMoreThanZeroCommunityCases,
          ExpectedNumberOfCasesAll,
          ExpectedNumberOfCasesEscapingOneScreen,
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
        "Est. arrivals per month (based on 2019)", 
        "Expected number of cases arriving per month",
        "Expected number of cases escaping screening per month",
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
          ExpectedNumberOfCasesAll,
          ExpectedNumberOfCasesEscapingOneScreen,
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
        "Expected number of cases escaping screening per month",
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
"Estimated arrivals per month are calculated assuming arrivals under existing quarantine regime, plus ", scales::percent(default_general_travel_rate), 
  "% of 2019 arrivals. In reality these will differ from treatment to treatment.<br/> <br/> ",
"Cook Islands and Western Samoa currently report COVID-free status, are rated zero risk. Due to their zero-risk status, it hasn't been necessary to include them in the dataset and they are not listed above.<br /><br />",
"Assumes screening sensitivity of ",scales::percent(input$intsim_sensitivity_level2_control),
" and quarantine failure rate of 1 in ", as.character(input$intsim_quarantine_failure_odds), "."
  ))})
  #intervention simulation page
  
  output$intsim_notes<-
    renderUI({
      withMathJax(HTML(paste0("
<h4>Notes:</h4>
<br /><br />
Refer to the 'Simulation settings' tab for more options.

")))})
  

  sim_geo_world_with_covid_data_bubble <- reactive({
    world_w_covid_data <- get_analysis_covid_data(
      nogeo_world_basic_data,
      screening_sensitivity = input$intsim_sensitivity_level2_control,
      quarantine_odds_override=(1/input$intsim_quarantine_failure_odds),
      general_travel_rate=input$intsim_percent_tvolume/100,
      assumed_ifr = input$simsettings_ifr/100)
    return(world_w_covid_data)
  })
  
  sim_geo_world_with_covid_data_level2 <- reactive({
    world_w_covid_data <- get_analysis_covid_data(
      nogeo_world_basic_data,
      screening_sensitivity = input$intsim_sensitivity_level2_control,
      quarantine_odds_override=(1/input$intsim_quarantine_failure_odds),
      general_travel_rate=input$intsim_percent_tvolume_level2_control/100,
      assumed_ifr = input$simsettings_ifr/100)
    return(world_w_covid_data)
  })
  
  sim_geo_world_with_covid_data_quarantine <- reactive({
    world_w_covid_data <- get_analysis_covid_data(
      nogeo_world_basic_data,
      screening_sensitivity = input$intsim_sensitivity_level2_control,
      quarantine_odds_override=(1/input$intsim_quarantine_failure_odds),
      general_travel_rate=input$intsim_percent_tvolume_with_quarantine/100,
      assumed_ifr = input$simsettings_ifr/100)
    return(world_w_covid_data)
  })
  
  #basically NZ citizens only.
  sim_geo_world_with_covid_data_statusquo <- reactive({
    world_w_covid_data <- get_analysis_covid_data(
      nogeo_world_basic_data,
      screening_sensitivity = input$intsim_sensitivity_level2_control,
      quarantine_odds_override=(1/input$intsim_quarantine_failure_odds),
      general_travel_rate=0,
      assumed_ifr = input$simsettings_ifr/100)
    return(world_w_covid_data)
  })
  
  countries_bubble_df <- reactive({
    return(get_intsim_dt(input$intsim_countries_bubble,"bubble",
                         #quarantine_odds_override=(1/input$intsim_quarantine_failure_odds),
                         sim_geo_world_with_covid_data_bubble())
    )
  })

  countries_l2_restrictions_df <- reactive({
    return(get_intsim_dt(input$intsim_countries_level2_control,"screening",
                         #quarantine_odds_override=(1/input$intsim_quarantine_failure_odds),
                         sim_geo_world_with_covid_data_level2())
    )
  })
  
  countries_quarantine_df <- reactive({
    return(get_intsim_dt(input$intsim_countries_quarantine,"quarantine",
                         #quarantine_odds_override=(1/input$intsim_quarantine_failure_odds),
                         sim_geo_world_with_covid_data_quarantine())
    )
  })
  
  
  get_countries_in_bubble_risks <- reactive({
    sim_geo_world_with_covid_data_bubble() %>%
      data.frame %>%
      filter(LifeExp>=life_exp_thresh) %>%
      filter(Location %in% input$intsim_countries_bubble)
  })
  
  #screener risk
  get_countries_level2_risks <- reactive({
    sim_geo_world_with_covid_data_level2() %>%
      data.frame %>%
      filter(LifeExp>=life_exp_thresh) %>%
      filter(Location %in% input$intsim_countries_level2_control)
  })
  
  get_countries_out_of_bubble_risks <- reactive({
    sim_geo_world_with_covid_data_quarantine() %>%
      data.frame %>%
      filter(LifeExp>=life_exp_thresh) %>%
      filter(Location %in% input$intsim_countries_quarantine)
    #use the lower "community cases" figure here because these are going through quarantine.
  })
  
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
      select(Location,
             "ExpectedCases"=ExpectedNumberOfCasesInCommunity
      )
  })
  
  get_intervention_risk <- reactive({
    status_quo_risk <- rbind(
      get_countries_in_bubble_risks() %>%
        select(Location,
               "ExpectedCases"=ExpectedNumberOfCasesAll,
        ),
      
      get_countries_level2_risks() %>%
        select(Location,
               "ExpectedCases"=ExpectedNumberOfCasesEscapingOneScreen
        ),
      get_countries_out_of_bubble_risks() %>%
        select(Location,
               "ExpectedCases"=ExpectedNumberOfCasesInCommunity
        ),
      get_countries_out_of_bubble_untrusted_risks() %>%
        select(Location,
               "ExpectedCases"=ExpectedNumberOfCasesInCommunity
        )
    )
  })

  
  output$total_risk_graph <- renderPlot({
    #foreign_risk <- get_foreign_risk()
    status_quo_risk <- get_status_quo_risk()
    intervention_risk <- get_intervention_risk()
    
    #OBSOLETE CODE
    #we no longer use this method
    #but should compare the new method against this one.
    #nz resident risk is the number of cases that have returned
    #roughly 39 - need to get a better figure
    #divided by our expected ratio of escaped cases
    #nz_resident_risk_df <- get_nz_resident_risk()
    
    #nz_resident_risk_label <- nz_resident_risk_df$Location[[1]]
    nz_res_only_label <- "Lockdown Returnee Restriction Locations"
    bubble_other_label <- "Other Bubble Locations"
    quarantine_other_label <- "Other Quarantine Locations"
    l2_screener_other_label <- "Other Screened Locations"
    
    
    #we'll pool risk from all countries that haven't been selected, for both status quo and intervention categories.
    selected_locations <- unique(c(
      input$intsim_countries_quarantine,
      input$intsim_countries_level2_control,
      input$intsim_countries_bubble
    ))
    
    intervention_risk$Condition<-"Intervention"
    
    status_quo_risk$Condition<-"Status Quo"
    
    combined_risk_graph <- rbind(status_quo_risk,intervention_risk) %>% 
      arrange(Location)
    
    warning(paste0(
      "data incomplete for",
      paste0(combined_risk_graph %>% filter(is.na(ExpectedCases)) %>% select(Location) %>% unique,collapse=", ")
            ))
    combined_risk_graph <- combined_risk_graph%>% filter(!is.na(ExpectedCases))
    

    #combined_risk_graph$Location[is.na(combined_risk_graph$Location)]<-nz_res_only_label
    combined_risk_graph<- 
      combined_risk_graph %>% 
      mutate(LocationLabel = 
        case_when(
        (Location %in% input$intsim_countries_bubble) & (ExpectedCases<0.01) ~ bubble_other_label,
        (Location %in% input$intsim_countries_level2_control) & (ExpectedCases<0.01) ~ l2_screener_other_label,
        (Location %in% input$intsim_countries_quarantine) & (ExpectedCases<0.01) ~ quarantine_other_label,
        ((Location %in% selected_locations)==FALSE) ~ nz_res_only_label,
        TRUE ~ Location
      ))
    
    combined_risk_graph$LocationLabel <-
      factor(combined_risk_graph$LocationLabel,
             levels = c(bubble_other_label,l2_screener_other_label,quarantine_other_label,selected_locations,nz_res_only_label),
             ordered=TRUE)
    
    
    #combine across grouped categories
    combined_risk_graph<-
      combined_risk_graph %>% 
      group_by(LocationLabel,Condition) %>%
      summarise(ExpectedCases=sum(ExpectedCases)) %>%
      arrange(Condition,desc(LocationLabel))
    
    #do the cumulative sum
    combined_risk_graph <-
      combined_risk_graph %>%
      group_by(Condition) %>%
      arrange(desc(LocationLabel)) %>%
      mutate(LabelPosition=cumsum(ExpectedCases)-ExpectedCases/2)
    
    location_labels_in_use <- length(intersect(selected_locations,combined_risk_graph$LocationLabel))
    
    #create color palette
    color_palette<- c()
    if (bubble_other_label %in% combined_risk_graph$LocationLabel){
      color_palette <- c(color_palette ,'#999999')
    }
    if (l2_screener_other_label %in% combined_risk_graph$LocationLabel){
      color_palette <- c(color_palette ,'#777777')
    }
    if (quarantine_other_label %in% combined_risk_graph$LocationLabel){
      color_palette <- c(color_palette ,'#555555')
    }
    color_palette = c(
      color_palette,
      rep(
        c('#1f78b4','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#6a3d9a','#b15928'),
        ceiling(location_labels_in_use/8))[1:location_labels_in_use],
      '#222222' #NZ residents only
      )
    
  
    
    
    plot_max <- sum(combined_risk_graph$ExpectedCases)
    ggplot(combined_risk_graph,aes(x=Condition,y=ExpectedCases,fill=LocationLabel,label=LocationLabel
                                   ))+
      geom_bar(stat="identity",alpha=0.8)+
      scale_x_discrete(name="")+
      scale_y_continuous(name="Expected cases per month",
                         #breaks=0:plot_max,
                         minor_breaks = NULL, 
                         limits = c(0,plot_max)#,
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
  
  
  total_risk_text <- reactive({
    
    countries_excluded_due_to_data<-
      sim_geo_world_with_covid_data_bubble() %>%
      data.frame %>%
      #filter((name_long %in% input$intsim_countries_bubble) | name_long %in% (input$intsim_countries_qurantine)) %>%
      filter(Location %in% 
               c(input$intsim_countries_bubble,
                 input$intsim_countries_level2_control,
                 input$intsim_countries_quarantine)) %>%
      filter(LifeExp<life_exp_thresh)
    
    
    #for countries that are let in without quarantine, we want to add 
    countries_in_bubble_risks <- get_countries_in_bubble_risks()$ProbabilityOfMoreThanZeroCases
    #countries_screener_risks <- get_countries_level2_risks()$ProbabilityOfMoreThanZeroCommunityCases
    countries_out_of_bubble_risks <- get_countries_out_of_bubble_risks()$ProbabilityOfMoreThanZeroCommunityCases
    
    untrusted_countries_risks <- get_countries_out_of_bubble_untrusted_risks()$ProbabilityOfMoreThanZeroCommunityCases
    
    # foreign_risk <- get_foreign_risk()
    # nz_resident_risk <- get_intervention_risk()
    intervention_risk <- get_intervention_risk()
    status_quo_risk <- get_status_quo_risk()
    increased_risk <- sum(intervention_risk$ExpectedCases,na.rm = TRUE)/sum(status_quo_risk$ExpectedCases,na.rm = TRUE)-1
    
    #now...
    #how do we combine these? 
    #it is not as simple as adding each up. 
    #I think we have to multiply the complements then get the complement again.
    #total_risk_prop<-1-prod(1-na.exclude(c(countries_in_bubble_risks,countries_out_of_bubble_risks,untrusted_countries_risks)))
    
    #now we need to add a warning for excluded countries.
    #total_risk_prop
    textout<-paste0(
      "In the status quo where only NZ residents are allowed can enter, we estimate ",
      signif(sum(status_quo_risk$ExpectedCases,na.rm = TRUE),2),
      " cases per month will be exposed to the community.",
      "<br /> <br />",
      "In the specified intervention, we estimate ",
      signif(sum(intervention_risk$ExpectedCases,na.rm = TRUE),2),
      " cases per month will be exposed to the community.",
      " The intervention increases the expected amount of community exposure by ",
      scales::percent(increased_risk,accuracy = 0.01),
      ".<br /> <br />",
      #"The status quo risk of exposing the community to 1 or more cases over a 1-month period is ",
      #scales::percent(total_risk_prop,accuracy = 0.01),
      #".\n\n"
      "<br /> <br />"
      ,"Community exposure could be anything from one very brief encounter (e.g., stopping for directions) to an infected individual entering the community undetected."
                    )
    
    if(length(countries_excluded_due_to_data$Location)>0){
      textout<-paste0(textout,
"<br /><br /> COVID-19 Data from the following countries is considered less reliable. 
NZ resident returnees from these countries are already allowed, but we caution against relying on this data for anything further: " ,
paste0(countries_excluded_due_to_data$Location,collapse = ", "))
    }
    
    
    
    textout<-paste0(textout, "<br /> <br />Cook Islands and Western Samoa are not explicitly modeled. These are currently rated zero-risk, due to their COVID-free status and low or zero inbound travel. The situation should be continually monitored for any changes to their current zero-risk status.")
    
    return(textout)
  })
  

  
  output$dt_countries_bubble<-DT::renderDataTable(
    countries_bubble_df() 
  )
  
  output$dt_countries_level2_control<-DT::renderDataTable(
    countries_l2_restrictions_df() 
  )
  
  
  output$dt_countries_quarantine<-DT::renderDataTable(
    countries_quarantine_df() 
    # %>%
    #   formatPercentage(c('ProbabilityOfMoreThanZeroCases','ProbabilityOfMoreThanZeroCommunityCases'),3) %>%
    #   formatRound(c('ExpectedNumberOfCases','ExpectedNumberOfCasesInCommunity'),2)
  )
  
  output$intsim_totalrisk<-
    renderUI({
      withMathJax(HTML(paste0(
        total_risk_text()
      )))
    })

  
  observeEvent(input$intsim_20countries,{
    print("reacting")
    updateSelectInput(
      session=session,
      inputId="intsim_countries_bubble",
      selected = character(0))
    
    updateSelectInput(
      session=session,
      inputId="intsim_countries_quarantine",
      selected = key_interest_countries)
    
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
  
  output$graph4header<-renderText({"Figure 5: New Zealand visitor arrivals by month"})
  output$graph4<-renderLeaflet({
    
    show_leaflet(data_to_show = get_filtered_mapped_world_with_covid_data() %>% filter(!is.na(LocationResidentMonthlyArrivals)),
                 primary_col = "LocationResidentMonthlyArrivals",
                 rounding_func = function(x){scales::comma(signif(x,3))},
                 legend_title =  "NZ Visitor arrivals by month",
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
    
    display_data <- get_filtered_mapped_world_with_covid_data() %>% filter(!is.na(ExpectedNumberOfCasesAll))
    col_of_interest <-"ExpectedNumberOfCasesAll"
    
    
    show_leaflet(data_to_show = display_data,
                 primary_col = col_of_interest,
                 rounding_func = function(x){scales::comma(signif(x,2))},
                 legend_title =  "Expected number of cases to arrive each month",
                 quant_grades = get_max_quantiles(display_data,col_of_interest,max_quantiles=8),
                 pal_reverse = FALSE
    )
  })
  output$graph7header<-renderText({paste0(
    "Figure 8: Probability of one or more cases arrives and is quarantined but reaches the community, based on arrival figures from this country in ",
    month_name,
    " 2019.")})
  output$graph7<-renderLeaflet({
    
    display_data <- get_filtered_mapped_world_with_covid_data() %>% filter(!is.na(ProbabilityOfMoreThanZeroCommunityCases))
    col_of_interest <- "ProbabilityOfMoreThanZeroCommunityCases"
    show_leaflet(data_to_show = display_data,
                 primary_col = col_of_interest,
                 rounding_func = function(x){scales::percent(x,accuracy = 0.01)},
                 legend_title =  "insert legend title",
                 quant_grades = get_max_quantiles(display_data,col_of_interest,max_quantiles=8),
                 pal_reverse = FALSE
    )
  })
  output$graph8header<-renderText({paste0(
    "Figure 9: Expected number of cases to arrive and be quarantined but still reach the community, based on arrival figures from this country in ",
    month_name,
    "2019.")})
  output$graph8<-renderLeaflet({
    
    display_data <- get_filtered_mapped_world_with_covid_data() %>% filter(!is.na(ExpectedNumberOfCasesInCommunity))
    col_of_interest <- "ExpectedNumberOfCasesInCommunity"
    
    show_leaflet(data_to_show = display_data,
                 primary_col = col_of_interest,
                 rounding_func = function(x){scales::comma(signif(x,2))},
                 legend_title =  "Expected number of cases to arrive each month",
                 quant_grades = get_max_quantiles(display_data,col_of_interest,max_quantiles=8),
                 pal_reverse = FALSE
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
          selectInput("intsim_countries_bubble",
                      "1. Level 1 locations (no quarantine, no screening):",
                      choices = countries_to_choose_from,
                      selected = c("Queensland, Australia","Tasmania, Australia",
                                   "Australian Capital Territory, Australia",
                                   "Western Australia, Australia","South Australia, Australia",
                                   "Northern Territory, Australia",
                                   #"Vietnam",
                                   "Taiwan*","Thailand"
                                   #"Malaysia","Cambodia","Sri Lanka"
                      ),
                      multiple=TRUE),
          numericInput("intsim_percent_tvolume",
                       "Expected incoming traveler volume for level 1 (% of 2019 levels):",
                       min=1,
                       max=100,step=1,
                       value = 80),
          selectInput("intsim_countries_level2_control",
                      "2. Level 2 locations (passengers screened at airport and held until cleared)",
                      choices = countries_to_choose_from,
                      selected = c(),
                      multiple=TRUE),
          numericInput("intsim_percent_tvolume_level2_control",
                       "Expected incoming traveler volume for level 2 (% of 2019 levels):",
                       min=1,
                       max=100,step=1,
                       value = 70),
          numericInput("intsim_sensitivity_level2_control",
                       "Sensitivity (NB: this setting is used to calculate sensitivity for ALL tabs):",
                       min=0,
                       max=1,step=0.1,
                       value = default_assumed_sensitivity),
          selectInput("intsim_countries_quarantine",
                      "3. Level 3 locations (full 14 day quarantine):",
                      choices = countries_to_choose_from,
                      multiple=TRUE,
                      selected = c("Korea, South")),
          numericInput("intsim_percent_tvolume_with_quarantine",
                       "Expected incoming traveler volume for level 3 (% of 2019 levels):",
                       min=1,
                       max=100,step=1,
                       value = 40),
          actionButton("intsim_20countries",
                       "Set to 20 country reference list",
                       class="btn btn-primary"),
          uiOutput("intsim_notes")
        ),
        mainPanel(
          titlePanel("Total risk per month"),
          uiOutput("intsim_totalrisk"),
          plotOutput("total_risk_graph"),
          titlePanel("Risk from travelers from Level 1 countries (in our bubble)"),
          DT::dataTableOutput("dt_countries_bubble"),
          titlePanel("Risk from travelers from Level 2 countries (subject to screener)"),
          DT::dataTableOutput("dt_countries_level2_control"),
          titlePanel("Risk from travelers from Level 3 countries (outside our bubble; all travelers quarantined)"),
          DT::dataTableOutput("dt_countries_quarantine")
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
         textOutput("graph7header"),
         leafletOutput("graph7"),
         textOutput("graph8header"),
         leafletOutput("graph8")
       )
     )
   )),
  tabPanel(
    "Simulation settings",
    fluidPage(
      titlePanel("Simulation Settings"),
      mainPanel(
        numericInput("intsim_quarantine_failure_odds",
                     "If someone who arrives in NZ with COVID19 and is quarantined,\nand they exit quarantine, the odds they are still contagious are 1 in ",
                     min=5,
                     max=10000,step=10,
                     value = default_quarantine_failure_odds),
        textOutput("ifr_explanation"),
        numericInput("simsettings_ifr",
                     "Assumed Infection Fatality Rate (%):",
                     min=0,
                     max=5,step=0.1,
                     value = default_assumed_ifr_percent),
        numericInput("simsettings_ifr",
                     "Aircraft infection rate (expected number of cases acquired 'in-flight' per pre-existing case):",
                     min=0,
                     max=10,step=0.001,
                     value = default_aircraft_infection_rate),
        numericInput("simsettings_ifr",
                     "Reduction in aircraft infections from mask use (%):",
                     min=0,
                     max=100,step=1,
                     value = default_aircraft_mask_effectiveness_percent)
        
      )
    )
  )


)

# Run the application 
shinyApp(ui = ui, server = server)

