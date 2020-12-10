library(shiny)
get_summary_page_tabPanel <- function(){
  return(
    tabPanel(
      "Summary",
      id="summary_page",
      fluidPage(
        titlePanel("Summary"),
        theme="summmary_page.css",
        includeHTML("components/summary_page/summary.html"),
        hr(),
        h2("Method and Approach"),
        includeHTML("components/summary_page/method.html"),
        actionButton("summary_page_method",
                     "Read more",class="btn btn-primary"),
        hr(),
        h2("Recommendations"),
        includeHTML("components/summary_page/recommendations.html"),
        leafletOutput("cases_in_community_map"),
        actionButton("summary_page_recommendations",
                     "Read more",class="btn btn-primary"),
        hr(),
        h2("Team"),
        includeHTML("components/summary_page/team.html")
      )
    )
  )
}

render_summary_page <- function(input, output, session, filtered_mapped_world_with_covid_data){
  
  observeEvent(input$summary_page_method,{
    print("method button clicked...")
    updateTabsetPanel(session, "mainNavbarPage",
                      selected = "Method and Approach")
    
    
  } 
  )
  observeEvent(input$summary_page_recommendations,{
    print("recommenations button clicked...")
    updateTabsetPanel(session, "mainNavbarPage",
                      selected = "Prevalence map")
  } 
  )

  output$cases_in_community_map<-renderLeaflet({
    
      display_data <- filtered_mapped_world_with_covid_data %>% filter(!is.na(ExpectedNumberOfCasesInCommunity))
      col_of_interest <- "ExpectedNumberOfCasesInCommunity"
      
      binpal <- colorBin("YlOrRd",
                         display_data$ExpectedNumberOfCasesInCommunity,
                         domain=c(0,max(display_data$ExpectedNumberOfCasesInCommunity,na.rm = TRUE)),
                         na.color=NA,
                         bins=c(0,0.01,0.1,1,
                                10^c(1:ceiling(log10(
                                  max(display_data$ExpectedNumberOfCasesInCommunity,na.rm = TRUE)))
                                )), pretty = FALSE)
      
      show_leaflet(data_to_show = display_data,
                   primary_col = col_of_interest,
                   rounding_func = function(x){scale_signif(x,2)},
                   legend_title =  "Expected number of\n cases exiting MIQ",
                   custom_palette = binpal
                   
      )
  })


}