library(shiny)
#summary_map.R

get_summary_map_tabPanel <- function(default_run_date){
  return(
    tabPanel(
      "Prevalence map",
      fluidPage(
        fluidRow(
          column(3,
                 titlePanel("Prevalence map")),
          column(2,
                 h4("View settings")),
          column(3,
                 dateInput("summary_map_run_date",
                           "Run date:",
                           value = default_run_date,max = Sys.Date())
                 
          ),
          column(4,
                 selectInput(
                   "summary_map_DisplayFilter",
                   "Countries to display",choices = c("Key Destinations and Travel Partners","All"),
                   selected = "Key Destinations and Travel Partners"
                   )
          )
        ),
        hr(),
        fluidRow(
          column(12,
                 leafletOutput("summary_map_prevalence_map"),
          )
        )
        # ,fluidRow(
        #   column(12,
        #          uiOutput("summary_map_notes")
        #   )
        # )
      )
    )
  )
}
render_summary_map <- function(input,output,filtered_mapped_world_with_covid_data,month_name){
  
  #write code to synchronize the date picker on this page with the date picker on the settings page.
    
  output$summary_map_prevalence_map<-renderLeaflet({
    #filter based on the run filter
    
    show_leaflet(data_to_show = filtered_mapped_world_with_covid_data %>% filter(!is.na(InfActiveCasesPerMillion)),
                 primary_col = "InfActiveCasesPerMillion",
                 rounding_func = function(x){scales::comma(signif(x,2))},
                 legend_title =  "Inferred active cases per million people <br /> (likely to underestimate in countries<br /> with poor death recording)",
                 quant_grades = 4,
                 pal_reverse = FALSE
    )
  })
  
}
