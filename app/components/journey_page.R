journey_data_collection <- list()

get_journey_page_under_construction_tabPanel <- function(){
  return(
    tabPanel("Journey design",
             titlePanel("Under construction")
             )
  )
}
get_journey_page_tabPanel <- function(){
  return(
    tabPanel(
      "Journey design",
      sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
          conditionalPanel(condition = "input.simsettings_mode=='Simple'",
                           checkboxInput(inputId="journey_preflight_test_toggle",
                                         "Run Pre-flight PCR test",
                                         value = TRUE
                                         ),
                           sliderInput(inputId="journey_aircraft_mask_usage",
                                       label = "Journey Aircraft Mask Usage (%)",
                                       min=0,max=100,step=1,
                                       value = 20
                                       ),
                           checkboxInput(inputId="journey_miq_health_check_toggle",
                                         "Run daily health check in quarantine",
                                         value = TRUE
                           )
          ),
          conditionalPanel(condition = "input.simsettings_mode=='Advanced'",
          selectInput("journey_preflight_test_dates",
                      "Preflight PCR on ONE randomly selected date of the following:",
                      choices = c(1,2,3,4,5,6,7),
                      selected=c(2,3),
                      multiple=TRUE),
          selectInput("journey_postflight_test_dates",
                      "Quarantine PCR on ALL of the following days:",
                      choices = 0:15,
                      selected=c(3,12),
                      multiple=TRUE)),
          # checkboxInput("journey_preflight_tempsymptoms",
          #               "Do preflight combined symptom and temperature screening",
          #               value=TRUE),
          selectInput("journey_quar_length",
                      "Quarantine length:",
                      choices = 0:15,
                      selected =14,
                      multiple=FALSE),
          actionButton("journey_reset",
                       "Reset"),
          actionButton("journey_update", "Update View",
                       class="btn btn-primary")
        ),
        mainPanel(
          plotOutput("journey_infectious_by_day"),
          plotOutput("journey_sum_total_vis"),
          conditionalPanel(condition = "input.simsettings_mode=='Advanced'",
            plotOutput("journey_vis")
          )
        )
      )
    )
  )
}
render_journey_page <- function(input,output){
  
  # populate_journey_data <- reactive({
  #   
  # })
  journey_input <- eventReactive(input$journey_update, {
    
    # if(input$journey_preflight_tempsymptoms){
    #   ctss<-c(0)
    # }else{
      ctss<-c()
    # }
      #we are disabling this altogether. the medical panel do not have confidence in this data.
      
      #in Advanced mode, quarantine tests are manually set:
    quarantine_release_day <- as.numeric(input$journey_quar_length)
    print("simsettings_mode:")
    print(input$simsettings_mode)
    print("quarantine_release_day:")
    print(quarantine_release_day)
    
    if(input$simsettings_mode=="Advanced"){
      pcr_test_to_remain_in_quarantine <- c(as.numeric(input$journey_postflight_test_dates))
      preflight_test_dates <- -as.numeric(input$journey_preflight_test_dates)
    }else if (input$simsettings_mode=="Simple"){
      if(quarantine_release_day>=14){
        pcr_test_to_remain_in_quarantine <- c(3,12) +quarantine_release_day - 14 #spacing for quarantines 14 days or more
      }else{
        pcr_test_to_remain_in_quarantine <- quarantine_release_day -1 #spacing for quarantines less than 14 days.
      }
      if(input$journey_preflight_test_toggle){
        preflight_test_dates<-c(-3,-2)
      }else{
        preflight_test_dates<-vector(mode="numeric",length = 0L)
      }
      
      
      
      
    }else{
      stop("Unknown simsettings mode.")
    }
    
    
    if(input$journey_miq_health_check_toggle){
      run_health_check <- TRUE
    }else{
      run_health_check <- FALSE
    }
    
    
    
    
    aircraft_spread_at_20pc_mask_use<-0.0043
    mask_spread_reduction <- 0.85 #reduction in mask use from 0% mask use to 100% mask use
    transmission_with_mask_use <- 1-mask_spread_reduction
    aircraft_spread_at_0pc_use <-aircraft_spread_at_20pc_mask_use/(0.8+0.2*transmission_with_mask_use)

    aircraft_spread_at_x_pct_mask_use <- function(mask_use_prop){
      aircraft_spread_at_0pc_use*(1-((1-transmission_with_mask_use)*mask_use_prop))
    }
    #check: aircraft_spread_at_x_pct_mask_use(0.2)
    
    print("pcr_test_to_remain_in_quarantine:")
    print(pcr_test_to_remain_in_quarantine)
    res<-run_sim(
      pcr_test_list_to_avoid_boarding = list(preflight_test_dates),
      pcr_test_to_remain_in_quarantine = pcr_test_to_remain_in_quarantine,
      quarantine_release_day = quarantine_release_day,
      quarantine_contacts_per_day=2.5,
      quarantine_health_check=run_health_check,
      p_flight_infection_risk_per_case = aircraft_spread_at_x_pct_mask_use(input$journey_aircraft_mask_usage/100) #, #with mask wearing
      #temp_and_symptoms_test_to_avoid_boarding=ctss #with temperature check
    )
    
    
    
    #create the label by which this particular sim will be known
    data_series_label<-paste0(
      ifelse(length(preflight_test_dates)>0,paste0("preflight PCR: ",paste0(preflight_test_dates,collapse = ","), "; "),""),
      "aircraft mask use: ", input$journey_aircraft_mask_usage, "%; ",
      "postflight PCR: ",paste0(pcr_test_to_remain_in_quarantine,collapse=","), "; ",
      "quarantine length: ",as.character(quarantine_release_day), 
      ifelse(run_health_check," with daily health checks",""),
      "; "
    )
    res$data_by_infection_source$label<-data_series_label
    res$data_by_day$label<-data_series_label
    
    #mark the key milestones along the journey
    journey_key_milestones <- data.frame(
      "label"=data_series_label,
      "event_name"=c(
        #rep("Preflight PCR",length(input$journey_preflight_test_dates)),
        #"Preflight PCR",
        rep("Postflight PCR",length(pcr_test_to_remain_in_quarantine)),
        "Quarantine release"
        ),
      "event_day"=c(
        #max(-as.integer(preflight_test_dates)),
        as.integer(pcr_test_to_remain_in_quarantine), 
        quarantine_release_day
      )
    )
    if(length(preflight_test_dates)>0){
      journey_key_milestones <- rbind(
        journey_key_milestones,
        data.frame("label" = data_series_label,
                   "event_name" = "Preflight PCR",
                   "event_day" = max(-as.integer(preflight_test_dates)))
        )
    }
    if(!is.null(journey_data_collection[["journey_by_infection_source"]])){
      if(any(journey_data_collection[["journey_by_infection_source"]]$label==res$data_by_infection_source$label[[1]])){
        return(journey_data_collection)
        #don't add this one because we already have it!
        #this is not very efficient because we're running a sim we're not using
        #but it's easy.
      }
    }
    #add this particular sim to the collection of sims
    if (is.null(journey_data_collection[["journey_by_infection_source"]])){
      journey_data_collection[["journey_by_infection_source"]] <<- res$data_by_infection_source
    }else{
      journey_data_collection[["journey_by_infection_source"]] <<- rbind(journey_data_collection[["journey_by_infection_source"]],res$data_by_infection_source)
    }
    if (is.null(journey_data_collection[["journey_by_day"]])){
      journey_data_collection[["journey_by_day"]] <<- res$data_by_day
    }else{
      journey_data_collection[["journey_by_day"]] <<- rbind(journey_data_collection[["journey_by_day"]],res$data_by_day)
    }
    if (is.null(journey_data_collection[["journey_key_milestones"]])){
      journey_data_collection[["journey_key_milestones"]] <<- journey_key_milestones
    }else{
      journey_data_collection[["journey_key_milestones"]] <<- rbind(journey_data_collection[["journey_key_milestones"]],journey_key_milestones)
    }
    
    
    return(journey_data_collection)
  }, ignoreNULL = FALSE)
  
  observeEvent(input$journey_reset,{
    journey_data_collection<<-list()
  }
  )
  
  # Generate a summary of the dataset ----
  output$journey_vis <- renderPlot({
    journey_data<-journey_input()$journey_by_infection_source
    ggplot(journey_data,aes(x=day_of_infection,y=p_remains_infectious_and_in_pipeline,group=label,color=label))+
      guides(color=guide_legend(nrow=length(unique(journey_data$label)),byrow=TRUE))+
      geom_vline(xintercept = 0,linetype="dashed",color="#000000")+
      geom_point()+geom_line()+
      scale_y_continuous(labels=scales::percent_format())+
      annotate(geom="text",x=-0.2,y=0,hjust=1,label="Day of travel")+
      labs(title="Cases intervention fails to catch", subtitle = "By day of infection", y="Remaining infectious at end of journey",x="Day of case's original infection")+
      theme(legend.position="bottom")
  })
  #we're going to generate a new plot, which describes how many cases are "prevented from release" at each point....
  #prevention could be either through detection or recovery.
  #or we can decide the reciprocal "percentage of cases still unidentified and infectious"
  #this could be done a few ways.
  #(1) consider a specific traveller journey with specific interventions and specific lenghts
  #   then consider that a case is "prevented" if it is identified.
  #   maybe display prevented and unprevented cases
  #   this will be a CUMULATIVE percent of ALL cases that will appear in the journey
  #   (the alternative would be cumulative percent of cases identified out of those that have ALREADY appeared. that would have counterintuitive effects)
  #   so the graph would jump at day 2 on the pre-test
  #   then be gradually declining...
  
  
  output$journey_sum_total_vis <- renderPlot({
    
    journey_data<-journey_input()$journey_by_day
    totals <-journey_data %>% group_by(label) %>% summarise(total_community_exposure_per_case=sum(p_community_exposure_by_day))
    
    ggplot(totals,aes(x=label,y=total_community_exposure_per_case,group=label,fill=label))+
      geom_bar(stat="identity")+
      theme(legend.position="bottom",axis.text.x = element_text(angle = 20))+
      geom_label(aes(label=scales::percent(total_community_exposure_per_case,accuracy = 0.1)))+
      scale_y_continuous(labels=scales::percent_format())+
      labs(title="Percentage of cases exiting quarantine with each intervention",x="Percent of cases exiting quarantine")
  })
  
  output$journey_infectious_by_day <- renderPlot({
    journey_data<-journey_input()$journey_by_day
    journey_key_milestones<-journey_input()$journey_key_milestones
    
    
    ggplot(journey_data,aes(x=days_past_flight,y=p_infectious_in_pipeline_by_day,color=label,group=label))+
      guides(color=guide_legend(nrow=length(unique(journey_data$label)),byrow=TRUE))+
      annotate(geom="text",x=-0.2,y=0,hjust=1,label="Day of travel")+
      geom_vline(xintercept = 0,linetype="dashed",color="#000000")+
      geom_vline(data=journey_key_milestones,aes(xintercept=event_day,color=label,group=label),linetype="dashed")+
      geom_text_repel(data=journey_key_milestones,aes(x=event_day,color=label,group=label,label=event_name),y=0.9,size=3,fontface="bold")+
      geom_point(alpha=0.5,size=2)+geom_line(alpha=0.5,size=2)+
      scale_y_continuous(labels=scales::percent_format())+
      coord_cartesian(xlim=c(-3,max(journey_data$days_past_flight)))+
      labs(title="Cases remaining infectious and undetected by day of travel journey", y="Percent remaining infectious and undetected",x="Day through travel journey")+
      theme(legend.position="bottom")#+
      #facet_grid(label~.)
    
    
  })
  
  
}
