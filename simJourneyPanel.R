require(readr)
require(dplyr)

simPanelDf <- read_csv("data/sim_panels.csv")

get_generated_button <- function(){
  return(          actionButton("intsim_20countries1",
                                "testbutton",
                                class="btn btn-primary"))
}

get_intervention_name <-function(level_id){
  simPanelDf_row <- simPanelDf %>% filter(level==level_id)
  return(simPanelDf_row  %>% .$label)
}
get_simJourneyPanel_from_level_id <- function(level_id, choices,selected= c()){
  simPanelDf_row <- simPanelDf %>% filter(level==level_id)
  
  return(simJourneyPanel(level_id,
                         choices = choices,
                         selected = selected,
                         default_effectiveness = simPanelDf_row %>% .$default_effectiveness,
                         default_extra_spread = simPanelDf_row %>% .$default_extra_spread,
                         default_volume = simPanelDf_row %>% .$default_volume
                         ))
}

simJourneyPanelHeader <- function(level_id){
  return(
    renderUI({HTML(
      paste0("<h5>",get_intervention_name(level_id),"</h5>")
    )})
    )
}



simJourneyPanel <- function(level_id,choices, selected,default_effectiveness, default_extra_spread, default_volume){
  return(
    wellPanel(
      uiOutput(paste0("intsim_level",level_id,"_header")),
      selectInput(paste0("intsim_countries_level",level_id),
                  paste0("Locations:"),
                  choices = choices,
                  selected = selected,
                  multiple=TRUE),
      numericInput(paste0("intsim_percent_tvolume_level",level_id),
                   "Traveler volume (% of 2019 levels, adjusted for lockdown levels):",
                   min=1,
                   max=100,step=1,
                   value = default_volume),
      numericInput(paste0("intsim_effectiveness_level",level_id),
                   "Effectiveness (%):",
                   min=0,
                   max=100,step=1,
                   value = default_effectiveness*100),
      numericInput(paste0("intsim_extraspread_level",level_id),
                   "Extra spread (%):",
                   min=0,
                   max=100,step=0.1,
                   value = default_extra_spread*100),
      textOutput(paste0("intsim_level",level_id,"_notes"))
    )
  )
}