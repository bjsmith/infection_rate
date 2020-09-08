
get_intsim_tabPanel <- function(default_simulation_data,countries_to_choose_from){
  return(tabPanel(
    "Intervention simulation",
    fluidPage(
      titlePanel("Intervention simulation"),
      sidebarLayout(
        sidebarPanel(
          actionButton("intsim_20countries",
                       "Set to 20 country reference list",
                       class="btn btn-primary"),
          checkboxInput(inputId = "intsim_universalPCR",
                        label="Include universal pre-departure PCR for Level 4 travellers in intervention",
                        value = FALSE),
          get_simJourneyPanel_from_level_id(0,choices= countries_to_choose_from,selected = 
                                              default_simulation_data %>% filter(Location %in% key_interest_countries & PrevalenceRating %in% "COVID-free") %>% .$Location),
          get_simJourneyPanel_from_level_id(1,choices= countries_to_choose_from,
                                            selected = default_simulation_data %>% filter(Location %in% key_interest_countries & PrevalenceRating %in% "Low") %>% .$Location),
          get_simJourneyPanel_from_level_id(2,choices= countries_to_choose_from,
                                            selected = default_simulation_data %>% filter(Location %in% key_interest_countries & PrevalenceRating %in% "Moderate") %>% .$Location),
          get_simJourneyPanel_from_level_id(3,choices= countries_to_choose_from,
                                            selected=default_simulation_data %>% filter(Location %in% key_interest_countries & PrevalenceRating %in% "High") %>% .$Location),
          get_simJourneyPanel_from_level_id(4,choices= c("(all other countries)"),
                                            selected= c("(all other countries)")),
          uiOutput("intsim_notes"),
          selectInput(inputId = "intsim_mode",
                      label="Panel mode",
                      choices = c("Simple","Advanced"),
                      selected = "Simple"
          )
        ),
        mainPanel(
          titlePanel("Total risk per month"),
          uiOutput("intsim_totalrisk"),
          #plotOutput("cases_at_border_graph"),
          plotOutput("intsim_AreaPlot"),
          plotOutput("total_risk_graph"),
          titlePanel("Level 0 countries (no risk)"),
          DT::dataTableOutput("dt_countries_level0"),
          titlePanel("Level 1 countries (low risk)"),
          DT::dataTableOutput("dt_countries_level1"),
          titlePanel("Level 2 countries (medium risk)"),
          DT::dataTableOutput("dt_countries_level2"),
          titlePanel("Level 3 countries (high risk)"),
          DT::dataTableOutput("dt_countries_level3")
        )
      )
    )
  ))
}

render_intsim_page <- function(input, output,sim_world_with_covid_data_statusquo){
  
  output$intsim_level0_header <- simJourneyPanelHeader(0)
  output$intsim_level1_header <- simJourneyPanelHeader(1)
  output$intsim_level2_header <- simJourneyPanelHeader(2)
  output$intsim_level3_header <- simJourneyPanelHeader(3)
  output$intsim_level4_header <- simJourneyPanelHeader(4)
  
  
  output$intsim_notes<-
    renderUI({
      withMathJax(HTML(paste0("
          <h4>Notes:</h4>
          <br /><br />
          Refer to the 'Simulation settings' tab for more options.
          ")))})
  
  output$intsim_level0_description <- renderUI({
    paste0(get_countries_allocated_to_leveln(input,0,sim_world_with_covid_data_statusquo),collapse = ", ")
  })
  output$intsim_level1_description <- renderUI({
    paste0(get_countries_allocated_to_leveln(input,1,sim_world_with_covid_data_statusquo),collapse = ", ")
  })
  output$intsim_level2_description <- renderUI({
    paste0(get_countries_allocated_to_leveln(input,2,sim_world_with_covid_data_statusquo),collapse = ", ")
  })
  output$intsim_level3_description <- renderUI({
    paste0(get_countries_allocated_to_leveln(input,3,sim_world_with_covid_data_statusquo),collapse = ", ")
  })
}

render_total_risk_text <- function(
  world_with_covid_data_level0,
  countries_allocated_to_levels0to3,
  status_quo_risk,
  intervention_risk
  ){reactive({
    countries_excluded_due_to_data<-
      world_with_covid_data_level0 %>%
      data.frame %>%
      filter(Location %in% countries_allocated_to_levels0to3) %>%
      filter(LifeExp<life_exp_thresh)
    
    
    # intervention_risk <- get_intervention_risk()
    # status_quo_risk <- get_status_quo_risk()
    increased_risk <- sum(intervention_risk$ExpectedNumberOfCasesInCommunity,na.rm = TRUE)/sum(status_quo_risk$ExpectedNumberOfCasesInCommunity,na.rm = TRUE)-1
    
    #now...
    #how do we combine these? 
    #it is not as simple as adding each up. 
    #I think we have to multiply the complements then get the complement again.
    #total_risk_prop<-1-prod(1-na.exclude(c(countries_in_bubble_risks,countries_out_of_bubble_risks,untrusted_countries_risks)))
    
    #now we need to add a warning for excluded countries.
    #total_risk_prop
    textout<-paste0(
      "In the status quo where only NZ residents are allowed can enter, we assume ",
      sum(status_quo_risk$MonthlyArrivalsWeighted,na.rm = TRUE),
      " travellers will arrive at the border per month. Consequently, we estimate ",
      signif(sum(status_quo_risk$ExpectedCasesAtBorderUnderLockdown,na.rm = TRUE),2),
      " positive cases per month will arrive at the border, of which ",
      signif(sum(status_quo_risk$ExpectedNumberOfCasesInCommunity,na.rm = TRUE),2),
      " will be exposed to the community.",
      "This represents ",
      signif(sum(status_quo_risk$ExpectedCasesAtBorder,na.rm=TRUE)/sum(status_quo_risk$StatusQuoMonthlyArrivals,na.rm = TRUE)*10^5,4),
      " cases per 100k travellers at the border, of which ",
      signif(sum(status_quo_risk$ExpectedNumberOfCasesInCommunity,na.rm = TRUE)/sum(status_quo_risk$StatusQuoMonthlyArrivals,na.rm = TRUE)*10^5,2),
      " will be exposed to the community.",
      "<br /> <br />",
      "In the specified intervention, we estimate ",
      round(sum(intervention_risk$MonthlyArrivalsWeighted,na.rm=TRUE),0),
      " travellers will arrive at the border per month. Consequently, we estimate ",
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
    
    
    
    textout<-paste0(textout, 
                    "<br /> <br /><em>Cook Islands</em> and <em>Western Samoa</em> are not explicitly modeled. International sources do not track these locations and they are currently rated zero-risk, due to their COVID-free status and low or zero inbound travel. The situation should be continually monitored for any changes to their current zero-risk status.",
                    " Their 100% August 2019 combined travel volumes were 21,952.")
    
    return(textout)
  })
}


renderCasesAtBorderGraph <- function(status_quo_risk){
  return(renderPlot({
    #status_quo_risk <- get_status_quo_risk()
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
    
    
  }))
}


get_total_risk_graph <- function(status_quo_risk,intervention_risk,countries_allocated_to_levels0to3){

    #nz_resident_risk_label <- nz_resident_risk_df$Location[[1]]
    nz_res_only_label <- "Status quo restricted locations"
    # bubble_other_label <- "Other Bubble Locations"
    # quarantine_other_label <- "Other Quarantine Locations"
    # l2_screener_other_label <- "Other Screened Locations"
    other_label <- "Other Locations"
    
    #we'll pool risk from all countries that haven't been selected, for both status quo and intervention categories.
    selected_locations <- unique(c(
      # input$intsim_countries_level0,
      # input$intsim_countries_level1,
      # input$intsim_countries_level2,
      # input$intsim_countries_level3
      countries_allocated_to_levels0to3
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
                         minor_breaks = NULL, 
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
}

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


get_countries_allocated_to_leveln <- function(input,level_n,world_with_covid_data_statusquo){
  
  if(input$intsim_mode=="Advanced"){
    return(input[[paste0("intsim_countries_level",as.character(level_n))]])
  }else if (input$intsim_mode=="Simple"){
    #obtain the current list (probably via the "statusquo" variable)
    #filter it by the lower-bound of the level below level_n if level_n is not zero
    #and filter it by this level's limit
    #then return the countries.
    max_prev <- input[[paste0("intsim_level",as.character(level_n),"_max_prevalence")]]
    locations <- world_with_covid_data_statusquo %>% 
      filter(Location %in% key_interest_countries) %>%
      filter(InferredActiveCaseTravelerRate<=max_prev/10^5)
      
      
    
    if(level_n>0){
      min_prev <- input[[paste0("intsim_level",as.character(level_n-1),"_max_prevalence")]]
      locations <- locations %>% filter(InferredActiveCaseTravelerRate>min_prev/10^5)
    }
    
    return(locations$Location)
  }else{
    stop("Unknown intsim_mode.")
  }
}
