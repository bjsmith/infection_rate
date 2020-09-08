get_Proposal_tabPanel <- function(default_simulation_data,countries_to_choose_from){
  return(tabPanel(
    "Proposal",
    fluidPage(
      titlePanel("Proposal"),
      sidebarLayout(
        sidebarPanel(
          
          
          actionButton("intsim_20countries",
                       "Set to 20 country reference list",
                       class="btn btn-primary"),
          checkboxInput(inputId = "intsim_universalPCR",
                        label="Include universal pre-departure PCR for Level 4 travellers in intervention",
                        value = FALSE),
          #list locations, don't allow them to be chosen.
          get_simSimplifiedJourneyPanel_from_level_id(0,
                                                      choices= countries_to_choose_from,
                                                      selected = default_simulation_data %>% filter(Location %in% key_interest_countries & PrevalenceRating %in% "COVID-free") %>% .$Location),
          get_simSimplifiedJourneyPanel_from_level_id(1,choices= countries_to_choose_from,
                                            selected = default_simulation_data %>% filter(Location %in% key_interest_countries & PrevalenceRating %in% "Low") %>% .$Location),
          get_simSimplifiedJourneyPanel_from_level_id(2,choices= countries_to_choose_from,
                                            selected = default_simulation_data %>% filter(Location %in% key_interest_countries & PrevalenceRating %in% "Moderate") %>% .$Location),
          get_simSimplifiedJourneyPanel_from_level_id(3,choices= countries_to_choose_from,
                                            selected=default_simulation_data %>% filter(Location %in% key_interest_countries & PrevalenceRating %in% "High") %>% .$Location),
          get_simSimplifiedJourneyPanel_from_level_id(4,choices= c("(all other countries)"),
                                            selected= c("(all other countries)")),
          uiOutput("intsim_notes")
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