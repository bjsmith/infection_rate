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
        actionButton("summary_page_recommendations",
                     "Read more",class="btn btn-primary"),
        hr(),
        h2("Team"),
        includeHTML("components/summary_page/team.html")
      )
    )
  )
}

render_summary_page <- function(input, output, session){
  
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


}