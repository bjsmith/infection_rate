journey_graph_collection_df <- NULL

get_journey_page_tabPanel <- function(){
  return(
    tabPanel(
      "Journey design",
      sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
          selectInput("journey_preflight_test_dates",
                      "Preflight PCR on ONE randomly selected date of the following:",
                      choices = c(1,2,3,4,5,6,7),
                      selected=c(2,3),
                      multiple=TRUE),
          selectInput("journey_postflight_test_dates",
                      "Quarantine PCR on ALL of the following days:",
                      choices = 0:15,
                      selected=c(3,12),
                      multiple=TRUE),
          checkboxInput("journey_preflight_tempsymptoms",
                        "Do preflight combined symptom and temperature screening",
                        value=TRUE),
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
          plotOutput("journey_vis"),
          plotOutput("journey_sum_total_vis")
        )
      )
    )
  )
}
render_journey_page <- function(input,output){
  journey_input <- eventReactive(input$journey_update, {
    if(input$journey_preflight_tempsymptoms){
      ctss<-c(0)
    }else{
      ctss<-c()
    }
    
    res<-run_sim(
      pcr_test_list_to_avoid_boarding = list(-as.numeric(input$journey_preflight_test_dates)),
      pcr_test_to_remain_in_quarantine = c(as.numeric(input$journey_postflight_test_dates)),
      quarantine_release_day = as.numeric(input$journey_quar_length),
      quarantine_contacts_per_day=2.5,
      p_flight_infection_risk_per_case_contact = 0.005*.15, #with mask wearing
      temp_and_symptoms_test_to_avoid_boarding=ctss #with temperature check
    )
    res$data_by_infection_source$label<-paste0(
      "preflight PCR: ",paste0(input$journey_preflight_test_dates,collapse = ","), "; ",
      "postflight PCR: ",paste0(input$journey_postflight_test_dates,collapse=","), "; ",
      ifelse(input$journey_preflight_tempsymptoms,"preflight temp check;",""),
      "quarantine length: ",input$journey_quar_length, "; "
    )
    
    if (is.null(journey_graph_collection_df)){
      journey_graph_collection_df <<- res$data_by_infection_source
    }else{
      journey_graph_collection_df <<- rbind(journey_graph_collection_df,res$data_by_infection_source)
    }
    return(journey_graph_collection_df)
    
  }, ignoreNULL = FALSE)
  
  observeEvent(input$journey_reset,{
    journey_graph_collection_df<<-NULL
  }
  )
  
  # Generate a summary of the dataset ----
  output$journey_vis <- renderPlot({
    journey_data<-journey_input()
    ggplot(journey_data,aes(x=day_of_infection,y=p_remains_infectious_and_in_pipeline,group=label,color=label))+geom_point()+geom_line()+
      guides(color=guide_legend(nrow=length(unique(journey_data$label)),byrow=TRUE))+
      labs(y="cases remaining infectious")+
      theme(legend.position="bottom")
  })
  
  output$journey_sum_total_vis <- renderPlot({
    journey_data<-journey_input()
    totals <-journey_data %>% group_by(label) %>% summarise(remains_infectious_and_in_pipeline=sum(p_remains_infectious_and_in_pipeline))
    
    ggplot(totals,aes(x=label,y=remains_infectious_and_in_pipeline,group=label,fill=label))+geom_bar(stat="identity")+
      theme(legend.position="bottom",axis.text.x = element_text(angle = 20))+geom_label(aes(label=scales::percent(remains_infectious_and_in_pipeline,accuracy = 0.01)))+
      scale_y_continuous(labels=scales::percent_format())
  })
  
  
}
