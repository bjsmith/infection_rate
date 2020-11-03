get_map_page_tabPanel <- function(){
  
  tabPanel(
    "Method and Approach",
    
    fluidPage(
      # Application title
      titlePanel("COVID-19: Prevalence around the world"),
      
      # Sidebar with a slider input for number of bins 
      sidebarLayout(
        sidebarPanel(
          uiOutput("testt")
          
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          textOutput("graph0header"),
          leafletOutput("graph0"),
          textOutput("graph1header"),
          leafletOutput("graph1"),
          textOutput("graph2header"),
          leafletOutput("graph2"),
          textOutput("graph3header"),
          leafletOutput("graph3"),
          textOutput("graph4header"),
          leafletOutput("graph4"),
          textOutput("graph5header"),
          leafletOutput("graph5"),
          textOutput("graph6header"),
          leafletOutput("graph6"),
          # textOutput("graph7header"),
          # leafletOutput("graph7"),
          textOutput("graph8header"),
          leafletOutput("graph8")
        )
      )
    ))
}

render_map_page <- function(output, filtered_mapped_world_with_covid_data,month_name){
  
  output$testt<-
    renderUI({
      withMathJax(HTML(
        map_page_notes(month_name,life_exp_thresh)
      ))
    })
  
  output$graph0header<-renderText({"Figure 1: Reported active cases now (average past 7 days)"})
  output$graph0 <- renderLeaflet({
    show_leaflet(data_to_show = filtered_mapped_world_with_covid_data %>% filter(!is.na(ActiveCases)),
                 primary_col = "ActiveCases",
                 rounding_func = function(x){scales::comma(round(x,1))},
                 legend_title =  "Observed active cases",
                 quant_grades = 4,
                 pal_reverse = FALSE
    )
    
  })
  output$graph1header<-renderText({"Figure 2: Hit rate or inferred case detection rate"})
  output$graph1 <- renderLeaflet({
    show_leaflet(data_to_show = filtered_mapped_world_with_covid_data %>% filter(!is.na(InferredDetectionRate)),
                 primary_col = "InferredDetectionRate",
                 rounding_func = function(x){scales::percent(x,accuracy = 0.1)},
                 quant_grades = 3,
                 legend_title = "Inferred detection rate <br /> (current deaths over cases three weeks prior; <br /> countries with a life expectancy of 75 or greater)")
    
  })
  output$graph2header<-renderText({"Figure 3: Inferred active cases today"})
  output$graph2 <- renderLeaflet({
    show_leaflet(data_to_show = filtered_mapped_world_with_covid_data %>% filter(!is.na(InferredActiveCases)),
                 primary_col = "InferredActiveCases",
                 rounding_func = function(x){scales::comma(round(x,1))},
                 legend_title =  "Inferred active cases<br /> (likely to underestimate in countries<br /> with poor death recording)",
                 quant_grades = 4,
                 pal_reverse = FALSE
    )
    
  })
  output$graph3header<-renderText({"Figure 4: Active cases per million"})
  output$graph3<-renderLeaflet({
    
    show_leaflet(data_to_show = filtered_mapped_world_with_covid_data %>% filter(!is.na(InfActiveCasesPerMillion)),
                 primary_col = "InfActiveCasesPerMillion",
                 rounding_func = function(x){scales::comma(signif(x,2))},
                 legend_title =  "Inferred active cases per million people <br /> (likely to underestimate in countries<br /> with poor death recording)",
                 quant_grades = 4,
                 pal_reverse = FALSE
    )
  })
  
  output$graph4header<-renderText({"Figure 5: New Zealand arrivals by month (2019 figures)"})
  output$graph4<-renderLeaflet({
    
    show_leaflet(data_to_show = filtered_mapped_world_with_covid_data %>% filter(!is.na(Total2019MonthlyArrivals)),
                 primary_col = "Total2019MonthlyArrivals",
                 rounding_func = function(x){scales::comma(signif(x,3))},
                 legend_title =  "NZ arrivals by month",
                 quant_grades = 5,
                 pal_reverse = FALSE
    )
  })
  output$graph5header<-renderText({paste0(
    "Figure 6: Probability of one or more cases arriving each month from residents of each country, based on arrival figures from this country in ",
    month_name,
    " 2019.")})
  output$graph5<-renderLeaflet({
    
    #get the maximum number of bins we can have, considering the distribution of the data
    display_data <- filtered_mapped_world_with_covid_data %>% filter(!is.na(ProbabilityOfMoreThanZeroCases))
    col_of_interest <-"ProbabilityOfMoreThanZeroCases"
    
    show_leaflet(data_to_show =  display_data ,
                 primary_col = col_of_interest,
                 rounding_func = function(x){scales::percent(x,accuracy = 0.01)},
                 legend_title =  "Probability of more than zero cases arriving to New Zealand",
                 quant_grades = get_max_quantiles(display_data,col_of_interest,max_quantiles=8),
                 pal_reverse = FALSE
    )
  })
  output$graph6header<-renderText({paste0(
    "Figure 7: Expected number of cases to arrive each month from residents of each country, based on arrival figures from this country in ",
    month_name,
    "2019.")})
  output$graph6<-renderLeaflet({
    
    display_data <- filtered_mapped_world_with_covid_data %>% filter(!is.na(ExpectedCasesAtBorder))
    col_of_interest <-"ExpectedCasesAtBorder"
    
    
    show_leaflet(data_to_show = display_data,
                 primary_col = col_of_interest,
                 rounding_func = function(x){scales::comma(signif(x,2))},
                 legend_title =  "Expected number of cases to arrive each month",
                 quant_grades = get_max_quantiles(display_data,col_of_interest,max_quantiles=8),
                 pal_reverse = FALSE
    )
  })
  # output$graph7header<-renderText({paste0(
  #   "Figure 8: Probability of one or more cases arrives and is quarantined but reaches the community, based on arrival figures from this country in ",
  #   month_name,
  #   " 2019.")})
  # output$graph7<-renderLeaflet({
  #   
  #   display_data <- filtered_mapped_world_with_covid_data %>% filter(!is.na(ProbabilityOfMoreThanZeroCommunityCases))
  #   col_of_interest <- "ProbabilityOfMoreThanZeroCommunityCases"
  #   show_leaflet(data_to_show = display_data,
  #                primary_col = col_of_interest,
  #                rounding_func = function(x){scales::percent(x,accuracy = 0.01)},
  #                legend_title =  "insert legend title",
  #                quant_grades = get_max_quantiles(display_data,col_of_interest,max_quantiles=8),
  #                pal_reverse = FALSE
  #   )
  # })
  output$graph8header<-renderText({paste0(
    "Figure 8: Expected number of cases to arrive and be quarantined but still reach the community, based on arrival figures from this country in ",
    month_name,
    "2019.")})
  output$graph8<-renderLeaflet({
    
    display_data <- filtered_mapped_world_with_covid_data %>% filter(!is.na(ExpectedNumberOfCasesInCommunity))
    col_of_interest <- "ExpectedNumberOfCasesInCommunity"
    
    binpal <- colorBin("YlOrRd", 
                       display_data$ExpectedNumberOfCasesInCommunity, 
                       domain=c(0,max(display_data$ExpectedNumberOfCasesInCommunity,na.rm = TRUE)),
                       bins=c(0.001,0.01,0.1,1,10,min(100,max(display_data$ExpectedNumberOfCasesInCommunity,na.rm = TRUE)), pretty = FALSE)
    )
    
    show_leaflet(data_to_show = display_data,
                 primary_col = col_of_interest,
                 rounding_func = function(x){scales::comma(signif(x,2))},
                 legend_title =  "Expected number of cases to arrive each month",
                 custom_palette = binpal
                 
    )
  })
  
  output$ifr_explanation <- renderText({
    paste0(
      "Where there are recent COVID fatalities, this is compared to confirmed cases two weeks prior. ",
      "If the number of cases look too low, then a 'hit rate' is calculated by comparing those cases two weeks prior to fatalities now.",
      "Then, inferred active cases now is caclculted by dividing the confirmed active cases by the hit rate.",
      "To do this we need to assume an infection fatality rate (IFR)."
      
    )
  })
}

map_page_notes <- function(month_name,life_exp_thresh){
  paste0(
  "

This app attempts to display probable <em>current</em> cases per million in each country.

First, take a look at the number of active cases, based on the reported active cases over the last 7 days (Figure 1).
<br />
<h4>Method</h4>

Data is limited but around the world, 0.5% has been estimated as a plausible infection to fatality rate for COVID-19 in normal populations 
<a href='https://science.sciencemag.org/content/early/2020/04/24/science.abb3221'>(Li et al., 2020)</a>.

<br /><br />
1. We can estimate the true infections \\(i\\)  three weeks prior as the number of fatalities \\(f\\) this week, multiplied by 200, if we assume an 0.5% CFR.
<br /><br />
$$i_{estimated, three weeks prior} = \\frac{f_{observed, this week}}{0.005} $$

<br /><br />
2. We can <i>then</i> compare the estimated true infections \\(i\\) three weeks prior with the observed \\(i\\) cases at the same time, and calculate a hit rate \\(h\\) 
(proportion of infections detected as confirmed cases)  (Figure 2). We then assume that the hit rate is constant.
<br /><br />
$$h = \\frac{i_{observed, three weeks prior}}{i_{estimated, three weeks prior}} $$

<br /><br />
3. Then we can estimate the true infections \\(i\\) today (Figure 3) based on detected cases today, and the estimated hit rate \\(h\\) .
$$i_{estimated, today} = \\frac{i_{observed, today}}{h}$$
<br /><br />
4. We can then divide that estimated number of true infections by population size \\(p\\) to get a per capita figure (Figure 4).
$$r = \\frac{i_{estimated, today}}{p} $$
<br /><br />

5. We can get the arrivals from last year based on country of usual residence (Figure 5). 
This isn't perfect data because it records country of usual residence rather than the country each person was last in. 
However, it is actually helpful to use this data because it excludes arrivals by New Zealand residents, who are already allowed back to NZ.

<br /><br />

6. We can then calculate the probability that we'll get one or more cases \\(P(c>0)\\) 
arriving from each source country in a month (Figure 6), 
using population size \\(p\\), 
the estimated number of active cases in that source country \\(i\\), 
and the number of people of that country who arrived in NZ in "
  ,month_name,
  " 2019, \\(a\\) . 
  
  Arrivals are estimated as the number of that countries' residents arriving in NZ this time last year (as in the prevous step), plus 
  New Zealanders returning from overseas where that country was their 'main destination'.
  
  
  If we assume cases are independent, we can use the following formula:

$$P(c>0) = 1-(\\frac{p-i_{estimated, today}}{p})^a$$

<br /><br />

7. Finally we can calculate the <em>expected number</em> of cases from each country (Figure 7). 
This is simply the estimated infection rate multiplied by the number of arrivals.

<br /><br />

8. If we can assume that a certain rate of quarantined arrivals will get out into the community before they are confirmed negative,
we can calculate the probability that at least arrival, if quarantined, will get out into the community. 

A very cautious assumption is that 1 in 50 people will break quarantine, and 1 in 100 people will test negative and be released but actually be positive.

With those assumptions we can calculate the probability of getting an arrival who goes through quarantine and testing, but reaches the community, as (Figure 8) and the expected number of such people (Figure 9).

<br /><br />

An alternative method would make an adjustment based on the estimated current rate of growth. Although it's problematic, we assume that the rate of growth in confirmed cases
reflects the rate of growth in actual infections. This method is not used here.

<br /><br />

In countries where health systems are underdeveloped, it is unlikely that COVID-19 deaths will be accurately recorded. 

Because countries where health systems are underdeveloped may not accurately record deaths, 
the visualization only displays countries with a life expectancy of "
  ,as.character(life_exp_thresh),
  " or higher."
  )
}