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
                         default_volume = simPanelDf_row %>% .$default_volume,
                         default_max_prevalence = simPanelDf_row %>% .$default_max_prevalence_p_m
                         ))
}

simJourneyPanelHeader <- function(level_id){
  return(
    renderUI({HTML(
      paste0("<h5>",get_intervention_name(level_id),"</h5>")
    )})
    )
}



simJourneyPanel <- function(
  level_id,choices, selected,
  default_effectiveness, default_extra_spread, default_volume,default_max_prevalence
  ){
  max_prev_per_100k <- default_max_prevalence/10
  
  if(!is.na(max_prev_per_100k)){
    prevalence_panel <- 
      conditionalPanel(condition = "input.intsim_mode=='Simple'",
                       sliderInput(paste0("intsim_level",level_id,"_max_prevalence"),
                                   "Maximum prevalence (per 100k) for country to be in this level:",
                                   min=max_prev_per_100k/10,max=max(max_prev_per_100k*10,1),
                                   step=max(max_prev_per_100k/10,0.1),
                                   value=max_prev_per_100k),
                       htmlOutput(paste0("intsim_level",level_id,"_description"))
                       )
  }else{
    prevalence_panel <- 
      conditionalPanel(condition = "input.intsim_mode=='Simple'",
                       htmlOutput(paste0("intsim_level",level_id,"_description"))
      )
  }
  
  return(
    wellPanel(
      uiOutput(paste0("intsim_level",level_id,"_header")),
      prevalence_panel,
      conditionalPanel(condition = "input.intsim_mode=='Advanced'",
      selectInput(paste0("intsim_countries_level",level_id),
                  paste0("Locations:"),
                  choices = choices,
                  selected = selected,
                  multiple=TRUE)
      ),
      numericInput(paste0("intsim_percent_tvolume_level",level_id),
                   "Traveler volume (% of 2019 levels, adjusted for lockdown levels):",
                   min=1,
                   max=100,step=1,
                   value = default_volume),
      conditionalPanel(condition = "input.intsim_mode=='Advanced'",
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
  )
}


get_simSimplifiedJourneyPanel_from_level_id <- function(level_id, choices,selected= c()){
  simPanelDf_row <- simPanelDf %>% filter(level==level_id)
  
  return(simSimplifiedJourneyPanel(level_id,
                         choices = choices,
                         selected = selected,
                         default_effectiveness = simPanelDf_row %>% .$default_effectiveness,
                         default_extra_spread = simPanelDf_row %>% .$default_extra_spread,
                         default_volume = simPanelDf_row %>% .$default_volume,
                         default_max_prevalence = simPanelDf_row %>% .$default_max_prevalence_p_m
                         
  ))
}
