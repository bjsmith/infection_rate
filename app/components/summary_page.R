library(shiny)
get_summary_page_tabPanel <- function(){
  return(
    tabPanel(
      "Summary",
      id="summary_page",
      fluidPage(
        titlePanel("Summary"),
        theme="summmary_page.css",
        uiOutput("summary_page_summary_full_text"),
        hr(),
        h2("Method and Approach"),
        uiOutput("summary_page_method_full_text"),
        actionButton("summary_page_method",
                     "Read more",class="btn btn-primary"),
        hr(),
        h2("Recommendations"),
        uiOutput("summary_page_recommendations_full_text"),
        actionButton("summary_page_recommendations",
                     "Read more",class="btn btn-primary"),
        hr(),
        h2("Team")
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
  
  output$summary_page_summary_full_text <- renderUI({
    withMathJax(HTML(paste0(
      "New Zealand is facing increasing economic recession as a result of its strict border policy which, at
        present, necessitates two weeks’ quarantine for all travellers who enter the country. We developed a
        model to explore the effect of selective border opening to travellers from countries with a low
        prevalence of covid-19 on the rate of undetected cases that pose a risk of spread to the community.
        The risk posed could be mitigated by introducing RT-PCR testing for covid-19 on either side of the
        border, increasing infection control at the border, and by varying times of quarantine on arrival to
        New Zealand. The length of quarantine is proposed to be commensurate with the prevalence in the
        traveller’s country."
    )))})
    
    
    output$summary_page_method_full_text <- renderUI({
      withMathJax(HTML(paste0(
"   The rate of covid-19 positive travellers entering New Zealand was conservatively calculated from the
    product of the prevalence of active covid-19 infection and projected monthly rate of arrivals.
    This monthly rate was then reduced by including a five tiered border policy and quarantine and test
    system which is applied to travellers based on their country of origin. These tiers had varying
    requirements for RT-PCR tests and duration of quarantine, with longer periods for travellers from
    higher prevalence countries, since spacing out PCR tests is likely to improve the overall sensitivity of
    covid-19 detection. 
    The proportion of cases detected was estimated to vary between 70 and 95%
    depending on the length of quarantine. No quarantine or testing is mandated for ‘covid-free’
    countries, whereas one, five and fourteen day quarantines are suggested for travellers from countries
    with increasing prevalence of covid-19. Pre-departure covid-19 tests are also suggested for travellers
    from all but covid-free countries.
    A real-time web app has been developed to estimate the rate of entry of undetected infection posed
    by these suggested selective border policies, and compare this to current rates. The app is flexible
    enough to assess the risk posed if important assumptions or parameters are altered.
"
      )))})
      
      output$summary_page_recommendations_full_text <- renderUI({
        withMathJax(HTML(paste0(
"
      New Zealand could introduce a tiered testing and quarantine system to its border which has the
      potential to dramatically increase the rate of travel to and from this country. Such a policy is
      projected to only modestly increase the risk of spread of covid-19 to the community.
"
        )))})
      

}