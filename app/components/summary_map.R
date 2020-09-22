library(shiny)
#summary_map.R
#https://stackoverflow.com/questions/64001136/setting-up-two-rshiny-input-values-for-the-same-value-across-different-tab-panel/64001953#64001953
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
          # column(3,
          #        dateInput("summary_map_run_date",
          #                  "Run date:",
          #                  value = default_run_date,max = Sys.Date())
          #        
          # ),
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
render_summary_map <- function(input,output,session,filtered_mapped_world_with_covid_data,month_name){
  
  
  #write code to synchronize the date picker on this page with the date picker on the settings page.
    
  output$summary_map_prevalence_map<-renderLeaflet({
    #filter based on the run filter
    data_to_show <- filtered_mapped_world_with_covid_data
    map_filter <- input$summary_map_DisplayFilter
    if (map_filter=="Key Destinations and Travel Partners"){
      #only countries with the reliabilityRating 
      data_to_show <- data_to_show %>% filter(DataReliabilityRating=="trustworthy" & Total2019MonthlyArrivals>=2000)
    }
    
    
    show_leaflet(data_to_show = data_to_show,
                 primary_col = "InferredActiveCaseTravelerRate",
                 rounding_func = function(x){scales::comma(signif(x,2))},
                 legend_title =  "Prevalence per million people",
                 quant_grades = 4,
                 pal_reverse = FALSE
    )
  })
  
}
