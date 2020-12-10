source("simulation.R")

#intervention_labels <- list("0"="Covid-free","1"="Level 1","2"="Level 2","3"="Level 3")


get_intsim_tabPanel <- function(default_simulation_data,countries_to_choose_from,default_adjust_for_imported_cases){
  return(tabPanel(
    "Proposed System",
    fluidPage(
      titlePanel("Proposed System"),
      sidebarLayout(
        sidebarPanel(
          actionButton("intsim_20countries",
                       "Set to 20 country reference list",
                       class="btn btn-primary"),
          checkboxInput(inputId = "intsim_universalPCR",
                        label="Include universal pre-departure PCR for Level 4 travellers in intervention",
                        value = TRUE),
          checkboxInput(inputId = "intsim_MIQ_adjust",
                        label="Exclude imported cases isolated in MIQ at border where possible (applies on all tabs)",
                        value = default_adjust_for_imported_cases),
          get_simJourneyPanel_from_level_id(0,choices= countries_to_choose_from,selected = 
                                              default_simulation_data %>% filter(Location %in% key_interest_countries & PrevalenceRating %in% "Covid-free") %>% .$Location
          ),
          get_simJourneyPanel_from_level_id(1,choices= countries_to_choose_from,
                                            selected = default_simulation_data %>% filter(Location %in% key_interest_countries & PrevalenceRating %in% "Level 1") %>% .$Location),
          get_simJourneyPanel_from_level_id(2,choices= countries_to_choose_from,
                                            selected = default_simulation_data %>% filter(Location %in% key_interest_countries & PrevalenceRating %in% "Level 2") %>% .$Location),
          get_simJourneyPanel_from_level_id(3,choices= countries_to_choose_from,
                                            selected=default_simulation_data %>% filter(Location %in% key_interest_countries & PrevalenceRating %in% "Level 3") %>% .$Location),
          get_simJourneyPanel_from_level_id(4,choices= c("(all other countries)"),
                                            selected= c("(all other countries)")),
          uiOutput("intsim_notes")
        ),
        mainPanel(
          #titlePanel("Total risk per month"),
          uiOutput("intsim_totalrisk"),
          plotOutput("intsim_AreaPlotAll"),
          checkboxInput(inputId = "intsim_AreaPlotAll_includeAll",
                        label="Include countries where fatality data reliability is questioned.",
                        value = FALSE),
          uiOutput("intsim_AreaPlotDescription"),
          plotOutput("intsim_AreaPlotSelected"),
          checkboxInput(inputId = "intsim_AreaPlotSelected_includeAll",
                        label="Include countries where fatality data reliability is questioned.",
                        value = FALSE),
          uiOutput("intsim_description2"),
          plotOutput("total_risk_graph"),
          uiOutput("intsim_description3"),
          plotOutput("cumulative_risk_plot"),
          checkboxInput(inputId = "intsim_assume_100pc_tvolume",
                        label="Assume travel reverts back to 100% of 2019 levels for all locations",
                        value = FALSE),
          uiOutput("intsim_description4"),
          actionButton("intsim_btn_go_to_risk_matrix",
                       "View Risk Matrix",class="btn btn-primary"),
          uiOutput("intsim_description5"),
          titlePanel("Covid-free countries (no risk)"),
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

render_intsim_page <- function(input, output,session,
                               sim_world_with_covid_data_statusquo,
                               get_status_quo_risk,
                               get_intervention_risk,
                               get_countries_allocated_to_levels0to3,
                               countries_level0_df,
                               countries_level1_df,
                               countries_level2_df,
                               countries_level3_df
                               
){
  
  output$intsim_level0_header <- simJourneyPanelHeader(0)
  output$intsim_level1_header <- simJourneyPanelHeader(1)
  output$intsim_level2_header <- simJourneyPanelHeader(2)
  output$intsim_level3_header <- simJourneyPanelHeader(3)
  output$intsim_level4_header <- simJourneyPanelHeader(4)
  

  
  
  output$intsim_AreaPlotAll <- renderPlot({
    generate_areaPlot(get_intervention_risk(),
                      title="COVID cases expected to enter the traveller journey from each location by volume of travel from each location",
                      include_unreliable_locations = input$intsim_AreaPlotAll_includeAll
                      )
  })
  
  output$intsim_AreaPlotDescription <-renderUI({
    level_1_max <- input$intsim_level1_max_prevalence
    level_2_max <- input$intsim_level2_max_prevalence
    level_3_max <- input$intsim_level3_max_prevalence
    withMathJax(HTML(paste0(
      "In the graph above, 
      the horizontal axis describes the cumulative number of travellers per month we would expect from each country in 2019, 
      based on Statistics New Zealand data 
      (as an approximation, travellers from Australian states are allocated proportionally according to states’ populations). 
      These are ordered from left to right Covid-free locations (zero reported active local cases), 
      Level 1 (less than ",as.character(level_1_max)," per 100k), 
      Level 2 (",as.character(level_1_max)," to ",as.character(level_2_max)," per 100k), 
      Level 3 (",as.character(level_2_max)," to ",as.character(level_3_max)," per 100k), 
      as well as Level 4 locations (more than ",as.character(level_3_max)," per 100k). 
      The vertical axis describes the cases per 1,000 travelers that we expect from each location. 
      The area of each location’s rectangle therefore represents the volume of Covid-19 cases we would expect 
      entering New Zealand from each country if no border measures were applied and if travel volumes were at 2019 levels. 
      \n \n <br /> <br />
      Travel volumes used here are based on 2019 data.
      \n \n <br /> <br />
      The graph below is the same data, but focused only on Covid-free to high prevalence locations 
      (0 to ",as.character(level_3_max)," expected cases per 100k travellers)."
    )))
  })

  output$intsim_AreaPlotSelected <- renderPlot({
    #intervention_risk <- intervention_risk
    generate_areaPlot(get_intervention_risk(),y_limit_per_thousand = 1,
                      title="COVID cases expected from each location by volume of travel from each location: excluding very high and extreme prevalence locations",
                      include_unreliable_locations = input$intsim_AreaPlotSelected_includeAll
                      
                      )
  })
  
  output$intsim_description2 <-renderUI({
    intervention_risk <- get_intervention_risk()
    withMathJax(HTML(paste0(
      "Under the current system, we expect ",signif(sum(get_status_quo_risk()$ExpectedNumberOfCasesInCommunity,na.rm = TRUE),4)," cases per month exiting MIQ. 
      Under the proposed system, we expect ",signif(sum(intervention_risk$ExpectedNumberOfCasesInCommunity,na.rm = TRUE),4)," cases per month exiting MIQ. 
      The vast majority of these (",
      intervention_risk %>% filter(is.na(InterventionLevel)) %>% .$ExpectedNumberOfCasesInCommunity %>% sum(.,na.rm=TRUE) %>% signif(.,4),
      ") are from New Zealanders and eligible exceptions in locations with prevalences above the “high prevalence” level, 
      i.e., very high or extreme prevalence countries. 
      Only ",intervention_risk %>% filter(!is.na(InterventionLevel)) %>% .$ExpectedNumberOfCasesInCommunity %>% sum(.,na.rm=TRUE) %>% signif(.,4),"
      are from countries where relaxed travel restrictions are proposed.
      \n \n <br /> <br />
      The graph below shows the expected number of cumulative travelers 
      by level in both the current and proposed systems."
    )))
  })
  
  output$total_risk_graph <-(
    renderPlot({
      get_total_risk_graph(get_status_quo_risk(),get_intervention_risk(),get_countries_allocated_to_levels0to3())
    })
  )
  
  output$intsim_description3 <-renderUI({
    status_quo_risk <- get_status_quo_risk()
    intervention_risk <- get_intervention_risk()
    withMathJax(HTML(paste0(
      "In the current system (red line), we expect around ",scale_signif(sum(status_quo_risk$MonthlyArrivalsWeighted,na.rm = TRUE)),
      " travelers and about ",signif(sum(status_quo_risk$ExpectedNumberOfCasesInCommunity,na.rm = TRUE),2)," cases exiting MIQ into the community per month. 
      In the proposed system (blue line), we expect ",
      intervention_risk %>% filter(is.na(InterventionLevel)) %>% .$ExpectedNumberOfCasesInCommunity %>% sum(.,na.rm=TRUE) %>% signif(.,2)
      ,"cases exiting MIQ from New Zealand and Eligible Exception Exceptions from Level 4 locations.
      Their risk can be substantially mitigated by mandatory or encouraged pre-departure Covid-19 testing. 
      From Level 0 to level 3 countries we expect just ",
      intervention_risk %>% filter(!is.na(InterventionLevel)) %>% .$ExpectedNumberOfCasesInCommunity %>% sum(.,na.rm=TRUE) %>% signif(.,2),
      " additional cases per month, due to the low prevalence of Covid-19 in those countries.."
    )))
  })
  
  output$cumulative_risk_plot <-renderPlot({
    status_quo_risk <- get_status_quo_risk()
    input$intsim_100PCTravel
    intervention_risk <- get_intervention_risk()
    
    get_cumulative_risk_plot(status_quo_risk,intervention_risk)
  })
  
  
  output$intsim_description4 <-renderUI({
    withMathJax(HTML(paste0("
    The tables below list statistics about each country 
      included in the Covid-free safe travel zone 
      through to Level 1, Level 2, and Level 3 locations. 
      Click the 'View Risk Matrix' button 
                            to view more detailed information about each country.")))
  })
  
  output$intsim_description5 <-renderUI({
    withMathJax(HTML(paste0("
    For each level, the locations currently categorized in each level are listed in the table. 
    These can be customized using the level threshold adjustment controls in the settings on the left hand side of the screen.
    The columns shown are:
    <ul>
    <li>The name of the location shown</li>
    <li>Under the current system,
      <ul>
        <li>Expected passengers per month based on 2019 data</li>
        <li>expected cases entering the passenger journey from each location</li>
      </ul>
      </li>
      <li>Under the proposed system,
      <ul>
        <li>Expected passengers per month assuming the travel volumes specified  using the controls on the left side of this page</li>
        <li>Expected cases entering the passenger journey from each location as a result</li>
        <li>Expected cases exiting MIQ</li>
      </ul>
      </li>
    
    </ul>
    
    
    
                            ")))
  })
  

  observeEvent(input$intsim_btn_go_to_risk_matrix,{
    #print("method button clicked...")
    updateTabsetPanel(session, "mainNavbarPage",
                      selected = "Risk Matrix")
  } 
  )
  
  output$dt_countries_level0<-DT::renderDataTable(countries_level0_df())
  output$dt_countries_level1<-DT::renderDataTable(countries_level1_df())
  output$dt_countries_level2<-DT::renderDataTable(countries_level2_df())
  output$dt_countries_level3<-DT::renderDataTable(countries_level3_df())
  
  
  output$intsim_notes<-
    renderUI({
      withMathJax(HTML(paste0("
          <h4>Notes:</h4>
          <br /><br />
          Refer to the 'Simulation settings' tab for more options.
          ")))})
  
  
  
  output$intsim_level0_description <- renderUI({
    paste0(get_countries_allocated_to_leveln(input,0,sim_world_with_covid_data_statusquo()),collapse = ", ")
  })
  output$intsim_level1_description <- renderUI({
    paste0(get_countries_allocated_to_leveln(input,1,sim_world_with_covid_data_statusquo()),collapse = ", ")
  })
  output$intsim_level2_description <- renderUI({
    paste0(get_countries_allocated_to_leveln(input,2,sim_world_with_covid_data_statusquo()),collapse = ", ")
  })
  output$intsim_level3_description <- renderUI({
    paste0(get_countries_allocated_to_leveln(input,3,sim_world_with_covid_data_statusquo()),collapse = ", ")
  })
  

  
  
}

render_total_risk_text <- function(
  get_world_with_covid_data_level0,
  get_countries_allocated_to_levels0to3,
  get_status_quo_risk,
  get_intervention_risk
){reactive({
  countries_excluded_due_to_data<-
    get_world_with_covid_data_level0() %>%
    data.frame %>%
    filter(Location %in% get_countries_allocated_to_levels0to3()) %>%
    filter(LifeExp<life_exp_thresh)
  
  
  intervention_risk <- get_intervention_risk()
  status_quo_risk <- get_status_quo_risk()
  increased_risk <- sum(intervention_risk$ExpectedNumberOfCasesInCommunity,na.rm = TRUE)/sum(status_quo_risk$ExpectedNumberOfCasesInCommunity,na.rm = TRUE)-1
  
  #now...
  #how do we combine these? 
  #it is not as simple as adding each up. 
  #I think we have to multiply the complements then get the complement again.
  #total_risk_prop<-1-prod(1-na.exclude(c(countries_in_bubble_risks,countries_out_of_bubble_risks,untrusted_countries_risks)))
  
  # print(status_quo_risk %>% select(Location,ExpectedNumberOfCasesInCommunity,PrevalenceRating,DataReliabilityRating,InterventionLevel) %>% filter(!is.na(ExpectedNumberOfCasesInCommunity)) %>% arrange(-ExpectedNumberOfCasesInCommunity),
  #       n=60)
  # print(intervention_risk %>% select(Location,ExpectedNumberOfCasesInCommunity,PrevalenceRating,DataReliabilityRating,InterventionLevel) %>% filter(!is.na(ExpectedNumberOfCasesInCommunity)) %>% arrange(-ExpectedNumberOfCasesInCommunity),
  #       n=60)
  
  if (increased_risk>=0){
    intervention_change_description <- "increase"
  }else{
    intervention_change_description <- "decrease"
  }
  
  #now we need to add a warning for excluded countries.
  #total_risk_prop
  textout<-paste0(
    "In the current system where only NZ residents are allowed can enter, we assume ",
    scale_dp(sum(status_quo_risk$MonthlyArrivalsWeighted,na.rm = TRUE)),
    " travellers will arrive at the border per month 
    (July 2020, NZ Customs; see <a href='https://www.customs.govt.nz/Covid-19/more-information/passenger-statistics/'>website</a> for updated figures).
    Consequently, we estimate ",
    signif(sum(status_quo_risk$ExpectedCasesAtBorderUnderLockdown,na.rm = TRUE),2),
    " positive cases per month will enter the traveller journey, of which ",
    signif(sum(status_quo_risk$ExpectedNumberOfCasesInCommunity,na.rm = TRUE),5),
    " will exit MIQ into the community",
    "This represents ",
    scale_dp(sum(status_quo_risk$ExpectedCasesAtBorder,na.rm=TRUE)/sum(status_quo_risk$StatusQuoMonthlyArrivals,na.rm = TRUE)*10^5,2),
    " cases per 100k travellers at the border, of which ",
    signif(sum(status_quo_risk$ExpectedNumberOfCasesInCommunity,na.rm = TRUE)/sum(status_quo_risk$StatusQuoMonthlyArrivals,na.rm = TRUE)*10^5,2),
    " will exit MIQ.",
    "<br /> <br />",
    "In the specified intervention, we estimate ",
    scale_dp(sum(intervention_risk$MonthlyArrivalsWeighted,na.rm=TRUE)),
    " travellers will arrive at the border per month. Consequently, we estimate ",
    scale_dp(sum(intervention_risk$ExpectedCasesAtBorder,na.rm = TRUE)),
    " cases per month will arrive at the border, of which ",
    signif(sum(intervention_risk$ExpectedNumberOfCasesInCommunity,na.rm = TRUE),5),
    " will exit MIQ.",
    " The intervention ",intervention_change_description, " the expected amount of cases exiting MIQ by ",
    scales::percent(abs(increased_risk),accuracy = 0.01),
    ".<br /> <br />",
    #"The status quo risk of exposing the community to 1 or more cases over a 1-month period is ",
    #scales::percent(total_risk_prop,accuracy = 0.01),
    #".\n\n"
    "A case exiting MIQ includes any event where a traveller causes a case exiting MIQ into the community. 
    This could include events like a very brief interaction covid-positive person with members of the public (e.g., stopping for directions),
    an infected arrival entering the community undetected, or infecting an airline worker, MIQ worker, another traveller in MIQ, 
    or other non-quarantined individual."
  )
  
  if(length(countries_excluded_due_to_data$Location)>0){
    textout<-paste0(textout,
                    "<br /><br /> Covid-19 Data from the following countries is considered less reliable. 
  NZ resident returnees from these countries are already allowed, but we caution against relying on this data for anything further: " ,
                    paste0(countries_excluded_due_to_data$Location,collapse = ", "))
  }
  
  
  
  textout<-paste0(textout, 
                  "<br /> <br /><em><a href='https://www.health.gov.ck/covid19/'>Cook Islands</a></em> and 
                  <em><a href='https://www.samoagovt.ws/category/novel-coronavirus-Covid-19/'>Samoa</a></em> are not explicitly modeled. 
                  International sources do not track these locations and they are currently rated zero-risk, 
                  due to their COVID-free status and low or zero inbound travel. 
                  The situation should be continually monitored for any changes to their current zero-risk status (As of Nov 19, 2020, one case arriving from New Zealand was detected at the border in Samoa).",
                  " Their 100% August 2019 combined travel volumes were 21,952.")
  
  return(textout)
})
}


renderCasesAtBorderGraph <- function(status_quo_risk){
  return(renderPlot({
    #status_quo_risk <- get_status_quo_risk()
    status_quo_risk$Condition<-"Current System"
    
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
                         limits = c(0,plot_max),
                         n.breaks=10
                         #limits=c(0,20),
                         #position="right"
      )+
      scale_fill_manual(values = color_palette)+
      theme(legend.position = "none",legend.box="vertical",legend.margin=margin(),text=element_text(face = "bold"),
            axis.text = element_text(size=16)
      )+
      geom_label_repel(aes(y=LabelPosition),color="white",fontface="bold")+
      coord_flip()+
      labs(caption="Sources: Johns Hopkins University, Statistics NZ, national and state health authorities; see Method & Approach for more detailed information. \n ")
    
    
  }))
}


get_total_risk_graph <- function(status_quo_risk,intervention_risk,countries_allocated_to_levels0to3){
  
  #nz_resident_risk_label <- nz_resident_risk_df$Location[[1]]
  nz_res_only_label <- "NZer or eligible exception from Level 4 locations"
  # bubble_other_label <- "Other Bubble Locations"
  # quarantine_other_label <- "Other Quarantine Locations"
  # l2_screener_other_label <- "Other Screened Locations"
  other_label <- "Other Level 0 to 3 Locations"
  
  #we'll pool risk from all countries that haven't been selected, for both status quo and intervention categories.
  selected_locations <- unique(c(
    # input$intsim_countries_level0,
    # input$intsim_countries_level1,
    # input$intsim_countries_level2,
    # input$intsim_countries_level3
    countries_allocated_to_levels0to3
  ))
  
  intervention_risk$Condition<-"Proposed System"
  
  status_quo_risk$Condition<-"Current System"
  
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
                       minor_breaks = NULL#, 
                       #limits = c(0,plot_max)#,
                       #limits=c(0,0.6)
                       #position="right"
    )+
    #scale_fill_brewer(palette="Set3")+
    scale_fill_manual(values = color_palette)+
    theme(legend.position = "none",legend.box="vertical",legend.margin=margin(),text=element_text(face = "bold"),
          axis.text = element_text(size=16)
    )+
    #coord_cartesian(ylim=c(0,0.65))+
    #guides(fill=guide_legend(nrow=2,byrow=TRUE))+
    geom_label_repel(aes(y=LabelPosition),color="white",fontface="bold")+
    coord_flip()+labs(
      title="Current System vs. Proposed System: Expected Cases per month",
      caption = "Sources: Johns Hopkins University, Statistics NZ, national and state health authorities; see Method & Approach for more detailed information \n 'Current system' travel volumes based on current travel volumes. 'Proposed system' travel volumes based on those simulated and specified on the left hand tab.")
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
  # dt_colnames <- c("Location",
  #                  "Status quo: pax per month",
  #                  "Status quo: Expected Infections At Border",
  #                  "Intervention: pax per month",
  #                  "Intervention: Expected Infections At Border",
  #                  "Intervention: Expected infections still infectious when reaching community")
  
  dt_colheader = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'Location'),
        th(colspan = 2, 'Current system'),
        th(colspan = 3, 'Proposed system')
      ),
      tr(
        lapply(rep(c('pax per month', 'Expected Infections At Border'), 2), th),
        th("Expected infections still infectious when reaching community")
      )
    )
  ))
  
  
  df_to_return<- filtered_df %>%
    #mutate(LocationHyper = paste0("<a href='www.google.com'>",Location,"</a>")) %>% 
    #here's how we'd finish this off: https://stackoverflow.com/questions/57973357/how-to-use-downloadhandler-for-the-download-button-create-inside-shiny-datatable/57978298#57978298
  select(Location,#Hyper,
         StatusQuoMonthlyArrivals,ExpectedCasesAtBorderUnderLockdown,
         MonthlyArrivalsWeighted,ExpectedCasesAtBorder,ExpectedNumberOfCasesInCommunity
  ) 
  
  
  return(DT::datatable(df_to_return,#colnames=dt_colnames,
                       container=dt_colheader,
                       rownames=FALSE
                       #escape=FALSE
                       )%>%
           #formatPercentage(percentage_cols,3) %>%
           formatRound(rounding_cols,2) %>%
           formatRound(rounding_0_cols,0)
  )
}


get_countries_allocated_to_leveln <- function(input,level_n,world_with_covid_data_statusquo){
  
  if(input$simsettings_mode=="Advanced"){
    #print_elapsed_time("Advanced:")
    #print_elapsed_time(input[[paste0("intsim_countries_level",as.character(level_n))]])
    return(input[[paste0("intsim_countries_level",as.character(level_n))]])
  }else if (input$simsettings_mode=="Simple"){
    #obtain the current list (probably via the "statusquo" variable)
    #filter it by the lower-bound of the level below level_n if level_n is not zero
    #and filter it by this level's limit
    #then return the countries.
    max_prev <- input[[paste0("intsim_level",as.character(level_n),"_max_prevalence")]]
    locations <- world_with_covid_data_statusquo %>% 
      filter(Location %in% key_interest_countries) %>%
      filter(InferredActiveCaseTravelerRate<=max_prev/10^5 & DataReliabilityRating=="trustworthy")
    
    
    
    if(level_n>0){
      min_prev <- input[[paste0("intsim_level",as.character(level_n-1),"_max_prevalence")]]
      locations <- locations %>% filter(InferredActiveCaseTravelerRate>min_prev/10^5)
    }
    #print_elapsed_time("Simple:")
    #print_elapsed_time(locations$Location)
    
    return(locations$Location)
  }else{
    stop("Unknown simsettings_mode.")
  }
}

abbreviate_location_names <- function(relevant_data){
  relevant_data <- relevant_data %>% 
    mutate(Location=str_replace(Location,"Australian Capital Territory","ACT")) %>%
    mutate(Location=str_replace(Location,"Northern Territory","NT")) %>%
    mutate(Location=str_replace(Location,"Western Australia","WA")) %>% 
    mutate(Location=str_replace(Location,"Victoria","VIC")) %>%
    mutate(Location=str_replace(Location,"Queensland","QLD")) %>% 
    mutate(Location=str_replace(Location,"Tasmania","TAS")) %>% 
    mutate(Location=str_replace(Location,"New South Wales","NSW")) %>% 
    mutate(Location=str_replace(Location,"United Kingdom","UK")) %>% 
    mutate(Location=str_replace(Location,"United States","USA")) %>% 
    mutate(Location=str_replace(Location,"China (mainland)","China")) %>% 
    mutate(Location=str_replace(Location,"Hong Kong, China","HK, China")) %>% 
    mutate(Location=str_replace(Location,"South Korea","S. Korea")) %>% 
    mutate(Location=str_replace(Location,"South Australia","SA"))
  
  return(relevant_data)
  
}

get_cumulative_risk_plot <- function(status_quo_risk,intervention_risk){
  #TO DO
  #1. Status quo: remains in red.
  #2. Rename status quo and intervention to current and proposed
  #3. Try to do the proposed line all in one colour or slightly different colours.
  #4. Group by levels not by country.
  #5. Try to list country names under level headings
  #6. Label levels at the END of their appropriate line.
  print_elapsed_time("preparing cumulative risk plot data...")
  ##compile data
  data_tidy <- function(covid_data){
    covid_data <- 
      covid_data %>% 
      filter(!is.na(ExpectedNumberOfCasesInCommunity) & !is.na(MonthlyArrivalsWeighted)) %>%
      mutate(InterventionApplied =
               if_else(!is.na(InterventionLevel) &
                         Location %in% key_interest_countries
                       ,TRUE,FALSE)
      )
  }
  status_quo_risk <-data_tidy(status_quo_risk)
  intervention_risk <-data_tidy(intervention_risk)
  
  
  cumulative_intervention_graph <- intervention_risk %>% 
    group_by(InterventionLabel) %>% 
    abbreviate_location_names %>%
    summarise(ExpectedNumberOfCasesInCommunity = sum(ExpectedNumberOfCasesInCommunity),
              MonthlyArrivalsWeighted = sum(MonthlyArrivalsWeighted),
              InterventionApplied = any(InterventionApplied),
              GroupMaxPrevalence = max(InferredActiveCaseTravelerRate),
              PrevalenceRating = paste0(unique(PrevalenceRating),collapse=","),
              CountryList = paste0(Location,collapse="\n")
    )%>%
    arrange(InterventionApplied,GroupMaxPrevalence) %>% 
    mutate(CumulativeExpectedCases=cumsum(ExpectedNumberOfCasesInCommunity),
           CumulativeExpectedTravellers=cumsum(MonthlyArrivalsWeighted),
           ExpectedTravellersXMark=if_else(InterventionApplied,CumulativeExpectedTravellers,as.numeric(NA)),
           PointLabel=if_else(InterventionApplied,
                              PrevalenceRating,
                              #paste0(InterventionLabel, ":\n",CountryList),
                              as.character(NA)),
           Scenario="Intervention"
    ) %>%
    select(-ExpectedNumberOfCasesInCommunity,-MonthlyArrivalsWeighted)
  
  #we might want to add the status quo....
  status_quo_cumulative_graph <- status_quo_risk %>%
    group_by(InterventionLabel) %>% 
    summarise(ExpectedNumberOfCasesInCommunityUnderLockdown = sum(ExpectedNumberOfCasesInCommunityUnderLockdown),
              StatusQuoMonthlyArrivals = sum(StatusQuoMonthlyArrivals),
              InterventionApplied = any(InterventionApplied),
              GroupMaxPrevalence = max(InferredActiveCaseTravelerRate),
              PrevalenceRating = paste0(unique(PrevalenceRating),collapse=",")
    )%>%
    select(InterventionLabel,ExpectedNumberOfCasesInCommunityUnderLockdown,StatusQuoMonthlyArrivals,
           GroupMaxPrevalence,PrevalenceRating
    ) %>%
    mutate(CumulativeExpectedCases=cumsum(ExpectedNumberOfCasesInCommunityUnderLockdown),
           CumulativeExpectedTravellers=cumsum(StatusQuoMonthlyArrivals),
           CountryList="(All)",
           ExpectedTravellersXMark = as.numeric(NA),
           PointLabel=as.character(NA),
           Scenario="StatusQuo",
           InterventionApplied=FALSE
    ) %>%
    select(-ExpectedNumberOfCasesInCommunityUnderLockdown,-StatusQuoMonthlyArrivals)
  
  point_of_origin_Intervention = data.frame(InterventionLabel="NA","InterventionApplied"=TRUE,GroupMaxPrevalence=0,PrevalenceRating=NA,CountryList="",CumulativeExpectedTravellers=0,CumulativeExpectedCases=0,ExpectedTravellersXMark=NA,PointLabel=NA,Scenario="Intervention")
  point_of_origin_StatusQuo <- point_of_origin_Intervention %>% mutate(InterventionApplied=FALSE,Scenario="StatusQuo")
  cumulative_graph <- rbind(point_of_origin_Intervention,cumulative_intervention_graph,point_of_origin_StatusQuo,status_quo_cumulative_graph)
  cumulative_graph[cumulative_graph$InterventionLabel=="None" & (cumulative_graph$Scenario=="Intervention"),"CountryList"] <- "NZ Returning\n and Eligible Exceptions\n from Level 4 Countries"
  cumulative_graph[cumulative_graph$InterventionLabel=="None" & (cumulative_graph$Scenario=="StatusQuo"),"CountryList"] <- ""
  #position the level labels
  current_system_risk <- max(cumulative_graph$CumulativeExpectedCases)
  status_quo_risk_level <-max((cumulative_graph %>% filter(Scenario=="StatusQuo"))$CumulativeExpectedCases)
  cumulative_graph <- cumulative_graph %>% group_by(Scenario) %>% mutate(LabelPosition = (rowid(Scenario))/(n()+1)*(max(cumulative_graph$CumulativeExpectedCases)))
  
  #find the crossover point where intervention risk becomes larger than status quo risk.
  last_intervention_under_limit_n = max(which(cumulative_intervention_graph$CumulativeExpectedCases<=status_quo_risk_level))
  if(last_intervention_under_limit_n==nrow(cumulative_intervention_graph)){
    number_of_travellers_at_intercept <- max(cumulative_intervention_graph$CumulativeExpectedTravellers)
    show_intercept <- FALSE
  }else{
    last_intervention_under_limit_cumrisk <- cumulative_intervention_graph[last_intervention_under_limit_n,"CumulativeExpectedCases"]
    last_intervention_under_limit_cumtravellers <- cumulative_intervention_graph[last_intervention_under_limit_n,"CumulativeExpectedTravellers"]
    diff_cases_between_last_intervention_and_next <- cumulative_intervention_graph[last_intervention_under_limit_n+1 ,"CumulativeExpectedCases"]- last_intervention_under_limit_cumrisk
    diff_travellers_between_last_intervention_and_next <- cumulative_intervention_graph[last_intervention_under_limit_n+1 ,"CumulativeExpectedTravellers"]- last_intervention_under_limit_cumtravellers
    prop_along_next_level <- (status_quo_risk_level - last_intervention_under_limit_cumrisk)/diff_cases_between_last_intervention_and_next
    number_of_travellers_at_intercept <- floor(diff_travellers_between_last_intervention_and_next * prop_along_next_level + last_intervention_under_limit_cumtravellers)
    
    show_intercept <- TRUE
  }
   
  ## Graph
  
  ggout<- ggplot(cumulative_graph,
         aes(x=CumulativeExpectedTravellers,y=CumulativeExpectedCases,label=PointLabel,color=interaction(InterventionApplied,Scenario),group=Scenario))+
    geom_vline(aes(xintercept=ExpectedTravellersXMark),linetype="dotted",color="#aaaaaa")+
    geom_line()+geom_point()+
    scale_color_manual(values=c("#2222AA","#000055","red"))+
    scale_x_continuous(name="Cumulative expected travellers per month\n under the current and proposed system", labels=scales::comma_format())+
    geom_hline(yintercept = status_quo_risk_level,color="#cc0000",linetype="dotted")
  if(show_intercept){
    ggout <- ggout + 
      geom_segment(mapping=NULL,
                   x = number_of_travellers_at_intercept[[1]], y = 0, xend = number_of_travellers_at_intercept[[1]], yend = status_quo_risk_level[[1]],
                   color="#cc0000",linetype="dotted")+
      annotate(geom="text",fontface="italic",
               x=number_of_travellers_at_intercept[[1]],y=0, vjust=1,hjust=1,color="#000055",size=4,#nudge_y=unit(0.25, "lines"),
               label=paste0("Proposed system could allow up to ",scales::comma(floor(number_of_travellers_at_intercept[[1]]))," travellers at no more risk than the current system."))
      
  }
  ggout <- ggout + 
    annotate(geom="text", x=max(cumulative_graph$CumulativeExpectedTravellers),y=status_quo_risk_level,vjust=1,hjust=1,color="#cc0000",
             label=paste0("Current system total traveller risk:\n",
                          as.character(round(current_system_risk,2)), " expected cases exiting MIQ per month"),size=3)+
    annotate(geom="text", 
             x=max(cumulative_graph %>% filter(Scenario=="StatusQuo") %>% .$CumulativeExpectedTravellers,na.rm=TRUE),
             y=max(cumulative_graph %>% filter(Scenario=="StatusQuo") %>% .$CumulativeExpectedCases,na.rm=TRUE)*0.8,
             vjust=0,hjust=0,color="red",
             label="Current system \n cumulative cases\n",size=3)+
    annotate(geom="text",
             x=max(cumulative_graph %>% filter(Scenario!="StatusQuo" & !InterventionApplied) %>% .$CumulativeExpectedTravellers,na.rm=TRUE)*2/5,
             y=max(cumulative_graph %>% filter(Scenario!="StatusQuo" & !InterventionApplied) %>% .$CumulativeExpectedCases,na.rm=TRUE)/2,
             vjust=0,hjust=0,color="#000055",
             label="NZ Returning\n and Eligible Exceptions\n from Level 4 Countries",size=3)+

    geom_text_repel(aes(y=LabelPosition), size=4,hjust=0)+
    geom_text_repel(aes(x=CumulativeExpectedTravellers,y=0,label=paste0(CountryList,"\n")),
                    size=3,hjust=1,vjust=0,fontface="bold"
                    ,direction="y",box.padding=0.1
                    )+
    guides(color="none")+
    scale_y_continuous(
      #scale_y_log10(
      name="Cumulative expected cases exiting MIQ per month",
      labels=function(x){as.character(x)},
      minor_breaks = NULL)+
    theme_minimal()+
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text = element_text(face="bold"),
          plot.margin = margin(12,12,12,12)
    )+labs(caption = "Sources: Johns Hopkins University, Statistics NZ, national and state health authorities; see Method & Approach for more detailed information")
  #ggsave(ggout,filename = "../../../data/imgout.png")
  return(ggout)
}

generate_areaPlot <- function(covid_data,show_horizontal_lines=TRUE,y_limit_per_thousand=NA,title="",include_unreliable_locations=TRUE){
  print_elapsed_time("preparing areaPlot data...")
  caption_text = ""
  #get initial dataset
  current_risk_per_thousand<-sum(covid_data$ExpectedCasesAtBorderUnderLockdown,na.rm=TRUE)/sum(covid_data$MonthlyArrivalsLockdownScaled2,na.rm=TRUE)*1000
  
  

  
  #we draw an area plot that needs coordinates
  #ordered by InferredActiveCaseTravelerRate from lowest to highest
  #coloured by PrevalenceRating
  #each country needs two data points in our area plot - the start and the end of it.
  #and we need a cumulative graph
  all_relevant_data <- covid_data %>% 
    filter(Location %in% key_interest_countries) %>%
    arrange(InferredActiveCaseTravelerRate) %>% 
    mutate(Arrivals=Total2019MonthlyArrivals)
  
  if(include_unreliable_locations==FALSE){
    unreliable_countries <- all_relevant_data %>% filter((DataReliabilityRating!="trustworthy") & !is.na(Location)) 
    all_relevant_data <- all_relevant_data %>% filter(DataReliabilityRating=="trustworthy")
    if(nrow(unreliable_countries)>0){
      caption_text <- paste0("The following locations have been excluded due to low data reliablity: ",paste(unreliable_countries$Location,collapse=", "), "\n")
    }
    
  }
  all_relevant_data <- all_relevant_data %>%
    select(Location,Arrivals,InferredActiveCaseTravelerRate,PrevalenceRating,InterventionLevel) %>%
    mutate(CumulativeTravelExclusive=cumsum(Arrivals)-Arrivals,
           CumulativeTravelInclusive=cumsum(Arrivals)
    )
  
  
  
  
  if(!is.na(y_limit_per_thousand)){
    relevant_data <- all_relevant_data %>% 
      filter(InferredActiveCaseTravelerRate<(y_limit_per_thousand/1000))
    excluded_locations <- all_relevant_data %>% 
      filter(InferredActiveCaseTravelerRate>=(y_limit_per_thousand/1000))
    
    if(nrow(excluded_locations)>0){
      excluded_list <- paste0(excluded_locations$Location,collapse = ", ")
      caption_text <- paste0(caption_text, "The following locations have been excluded because prevalence is too high to display: ", excluded_list)
    }
  }else{
    relevant_data <- all_relevant_data
  }
  
  caption_text <- paste0(caption_text, "\n Travel volumes based on 100% of 2019 data.")
  
  relevant_data<-abbreviate_location_names(relevant_data)
  
  
  #shape into what we want
  
  all_rects <-
    relevant_data %>% 
    mutate(CasesPerThousand=InferredActiveCaseTravelerRate*10^3) %>%
    mutate(bottom=0,
           RectLabel=Location,
           top=CasesPerThousand,
           left=CumulativeTravelExclusive,
           right=CumulativeTravelInclusive,
           label_xpos=CumulativeTravelExclusive + Arrivals/2,
           label_ypos=0)
  
  all_rects <- all_rects %>%
    mutate(RectId=1:nrow(all_rects)#,
           #GroupId=as.numeric(NA)
    )
  
  
  
  #re-arrange labels
  #higher threshold is more grouping
  threshold <- max(all_rects$CumulativeTravelInclusive)/50
  for (rect_i in 2:nrow(all_rects)){
    #now for each row, set the rectgroup to the minimum rectID where the row label_xpos has less than a certain difference.
    current_rectID<-all_rects[[rect_i,"RectId"]]
    current_xpos <- all_rects[[rect_i,"label_xpos"]]
    group_base_rect_candidates <- all_rects %>% filter(label_xpos > (current_xpos - threshold) & RectId<current_rectID)
    if(nrow(group_base_rect_candidates)>0){
      group_base_rect<-group_base_rect_candidates[1,]
      #print(paste0(group_base_rect$Location, " taking ", all_rects[[rect_i,"Location"]]))
      group_base_rect_id <- group_base_rect$RectId
      #and move the xpos back to reflect the change.
      all_rects[[rect_i,"label_xpos"]] <- group_base_rect[["label_xpos"]]
      #now merge the labels
      #print(group_base_rect_id)
      all_rects <- all_rects %>% 
        mutate(RectLabel=ifelse(RectId==group_base_rect_id,
                                paste0(RectLabel,"\n",all_rects[[rect_i,"RectLabel"]]),
                                RectLabel
        ))
      
      all_rects[[rect_i,"RectLabel"]] = ""
    }
  }
  #set up colours for levels
  
  level_rects <- all_rects %>% 
    filter(PrevalenceRating!="Unknown") %>%
    group_by(PrevalenceRating, InterventionLevel) %>% 
    summarise(
      left = min(CumulativeTravelExclusive,na.rm = TRUE),
      right = max(CumulativeTravelInclusive,na.rm = TRUE),
      top=max(all_rects$CasesPerThousand,na.rm = TRUE)*2,
      bottom=-max(all_rects$CasesPerThousand,na.rm = TRUE),
      max_prevalence=max(InferredActiveCaseTravelerRate,na.rm = TRUE)
    ) %>% 
    arrange(max_prevalence)
  
  #level_rects$InterventionLevelLabel <- sapply(as.character(level_rects$InterventionLevel),function(x){intervention_labels[[x]]})
  
  print_elapsed_time("generating areaPlot...")
  #let's scale it to a maximum of 20 points
  #what does that look like?
  y_by_options = c(0.1,0.2,0.5,1,2,4,5,10,20,40,50,100,200,400,500) #shouldn't really go above 4 or 5...
  max_cpt = max(all_rects$CasesPerThousand,na.rm=TRUE)
  print(max_cpt)
  print((max_cpt/y_by_options))
  y_by <- min(y_by_options[(max_cpt/y_by_options)<=20])
  y_scale_sequence = seq(0,max(all_rects$CasesPerThousand,na.rm=TRUE)+y_by,y_by)
  
  plot <-     ggplot(all_rects,aes(xmin=left, xmax=right, ymin=bottom,ymax=top))+
    #draw the levels
    geom_rect(data=level_rects,mapping=aes(xmin=left, xmax=right, ymin=bottom,ymax=top,fill=PrevalenceRating),
              alpha=0.5,
              fill=brewer.pal(n = nrow(level_rects), name = "Blues"))+
    geom_text_repel(data=level_rects,mapping=aes(left, label=PrevalenceRating),segment.color = "transparent",
                    y=max(all_rects$CasesPerThousand,na.rm=TRUE)*2/3,
                    hjust=1,
                    size=3,
                    fontface="bold",
                    color="black",
                    fill=brewer.pal(n = nrow(level_rects), name = "Blues"))+
    #draw the countries
    geom_vline(aes(xintercept=CumulativeTravelInclusive),linetype="dotted",color="#aaaaaa")+
    geom_rect(color="#9999ff",fill="#000055",alpha=0.8)+
    geom_text(aes(label_xpos,y=label_ypos,label=RectLabel),
              angle=50,size=3,hjust=1,vjust=1,lineheight = 0.7,fontface="bold")+
    #draw the current average prevalence
    geom_hline(yintercept = current_risk_per_thousand,color="#cc0000")+
    annotate(geom="text",x=0,y=current_risk_per_thousand,vjust=0,hjust=0,color="#cc0000",label="Status quo aggregate traveller risk\n",size=3)+
    #set the scales and style
    geom_vline(xintercept=0,color="#aaaaaa")+
    scale_x_continuous(name="Cumulative expected travellers per month", labels=scales::comma_format(),)+
    scale_y_continuous(name="Cases per 1,000 travellers",breaks = y_scale_sequence,minor_breaks = NULL)+
    coord_cartesian(ylim=c(-max(all_rects$CasesPerThousand,na.rm=TRUE)/4,max(all_rects$CasesPerThousand,na.rm=TRUE)))+
    theme_minimal()
  
  if (show_horizontal_lines){
    plot <- plot +       theme(panel.grid.major.x = element_blank(),
                               panel.grid.minor.x = element_blank()
    )
  }else{
    plot <- plot +       theme(panel.grid.major.x = element_blank(),
                               panel.grid.minor.x = element_blank(),
                               panel.grid.major.y=element_blank()
    )
    
  }
  plot <- plot + labs(title=title,
                      caption = paste0(caption_text,"\n","Sources: Johns Hopkins University, Statistics NZ, national and state health authorities; see Method & Approach for more detailed information."))
  
  return(plot)
  }

