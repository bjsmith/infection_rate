method_example_Panel <- function(mainPanel,examplePanel){
  return(sidebarLayout(
    sidebarPanel = examplePanel,
    mainPanel=mainPanel,position = "right"
  ))
}

get_map_page_tabPanel <- function(){
  
  tabPanel(
    "Method and Approach",
    
    fluidPage(title="Method and Approach",
              id="methapp_fixedPage",
      # Application title
      titlePanel("Method and Approach"),
      # Show a plot of the generated distribution
      method_example_Panel(
      examplePanel=sidebarPanel(includeMarkdown('example1.Rmd')),
      mainPanel = mainPanel(
      uiOutput("graph0header"),
      leafletOutput("graph0"))
      ),
      uiOutput("graph1header"),
      leafletOutput("graph1"),
      uiOutput("graph2header"),
      leafletOutput("graph2"),
      uiOutput("graph3header"),
      leafletOutput("graph3"),
      uiOutput("graph4header"),
      leafletOutput("graph4"),
      uiOutput("graph5header"),
      leafletOutput("graph5"),
      uiOutput("graph6header"),
      leafletOutput("graph6"),
      # textOutput("graph7header"),
      # leafletOutput("graph7"),
      uiOutput("graph8header"),
      leafletOutput("graph8"),
      
      uiOutput("paragraph_09")
    ))
}

render_map_page <- function(output, filtered_mapped_world_with_covid_data,month_name){
  
  output$testt<-
    renderUI({
      withMathJax(HTML(
        map_page_notes(month_name,life_exp_thresh)
      ))
    })
  #https://math.meta.stackexchange.com/questions/5020/mathjax-basic-tutorial-and-quick-reference
  output$graph0header<-renderUI({
    withMathJax(HTML(
    "
    To understand the risk to New Zealand of opening up to each country, 
    our basic approach is to multiply the prevalence of covid-19 in each country by the 
    expected number of travellers from that country, 
    and then mitigate that by the level of protection our border security program provides:
    
    $$ \\text{risk} = \\frac{\\text{prevalence} \\times \\text{travel volume}}{\\text{border protection}} $$
    
    To do this we must understand three key factors:
    
    <ol>
    <li>prevalence</li>
    <li>travel volume</li>
    <li>border protection</li>
    </ol>
    
    More detail can be found in a forthcoming academic paper (contact the authors at <a href='mailto:hello@striatum.co.nz'>hello@striatum.co.nz</a> to view an advance copy). 
    The source code for this application, written based on R Shiny, is accessible to anyone and available for download under the Open Source MIT License via <a href='https://github.com/bjsmith/infection_rate'>Github</a>. 
    

    The first data point we need is the active cases in each country (Figure 1).
    These figures are based on data reported by <a href='https://systems.jhu.edu/research/public-health/ncov/'>Dong, Du, and Gardner (2020)</a> 
    at Johns Hopkins University Center for Systems Science and Engineering.
    
    Active cases are confirmed cases minus confirmed fatalities and recoveries.
    

  <br />
  <br /><br />

    
    Figure 1: Reported active cases now
    
    "))})
  output$graph0 <- renderLeaflet({
    show_leaflet(data_to_show = filtered_mapped_world_with_covid_data %>% filter(!is.na(ActiveCases)),
                 primary_col = "ActiveCases",
                 rounding_func = function(x){scales::comma(round(x,1),accuracy=1)},
                 legend_title =  "Observed active cases",
                 quant_grades = 4,
                 pal_reverse = FALSE
    )
    
  })
  output$graph1header<-renderUI({withMathJax(HTML("
  
  
    Where case numbers are low, we have endeavoured to supplement this with 
    <a href='https://docs.google.com/spreadsheets/d/1hkpfinHpxT1KcTI8umh55aaiFug12jKKSMZoae4ttlA/edit?usp=sharing'>
    data gathered directly from national and state Ministries of Health</a>
    in order to exclude cases detected at each location's border and held in managed isolation/quarantine.
  <br /><br />  
  
  We don't want to assume that each country is detecting all of their infections, 
  so we need to work out their likely detection rate and adjust for that. We only ever adjust infections upwards; never downwards.
    
    Data is limited but around the world, experts estimated the true infection fatality rate (IFR) in Wuhan, China at 0.6% 
<a href='https://www.eurosurveillance.org/content/10.2807/1560-7917.ES.2020.25.12.2000256'>(Russell et al., 2020)</a>.

<br /><br />
1. We can estimate the <em>case fatality rate</em> (CFR), the number of reported cases relative to the number of fatalities, by 
comparing the number of reported cases \\(f\\) three weeks' prior with the number of fatalities this week:
<br /><br />
$$ \\text{CFR} = \\frac{f_{\\text{observed, this week}}}{c_{\\text{observed, three weeks ago}}} $$

2. Combined with the internationally reported estimate of the true IFR we can calculate the infection detection rate (IDR) with the formula

$$ \\text{IDR} = \\frac{\\text{CFR}}{0.6\\%}$$

The estimated IDR for each country is shown in Figure 2.

These rely on accurate fatality estimates.  
Because we are not confident about fatality estimates in countries with poor health systems, we exclude any counctires with a life expectancy of under 70.


<br /><br />
    
    Figure 2: Infection detection rate"))})
  output$graph1 <- renderLeaflet({
    show_leaflet(data_to_show = filtered_mapped_world_with_covid_data %>% filter(!is.na(InferredDetectionRate)),
                 primary_col = "InferredDetectionRate",
                 rounding_func = function(x){signif(x,2)},
                 quant_grades = 4,
                 legend_title = "Infection detection rate",
                 pal_reverse = FALSE)
    
  })
  output$graph2header<-renderUI({withMathJax(HTML("
    
<br /><br />
3. We can <i>then</i> estimate the true number of infections in a community now by multiplying the number of confirmed cases by the IDR:
<br /><br />
$$ i_{\\text{estimated, today}} = c_{ \\text{observed, today}} \\times \\text{IDR} $$

If adjusting by the IDR would reduce the number of estimated infections below the confirmed active cases in any particular country, 
then we skip this step for that country.
<br /><br />

    
    
    Figure 3: Estimated true number of active invections"))})
  output$graph2 <- renderLeaflet({
    show_leaflet(data_to_show = filtered_mapped_world_with_covid_data %>% filter(!is.na(InferredActiveCases)),
                 primary_col = "InferredActiveCases",
                 rounding_func = function(x){scales::comma(round(x,0),accuracy=1)},
                 legend_title =  "Estimated active infections",
                 quant_grades = 4,
                 pal_reverse = FALSE
    )
    
  })
  output$graph3header<-renderUI({
    withMathJax(HTML(
  "
    4. We can then divide that estimated number of true infections by population size \\(p\\) to calculate a prevalence per million people (Figure 4).
$$r = \\frac{i_\\text{estimated, today}}{p} $$
<br /><br />

    
    
    Figure 4: Infections per million people"))})
  output$graph3<-renderLeaflet({
    
    show_leaflet(data_to_show = filtered_mapped_world_with_covid_data %>% filter(!is.na(InfActiveCasesPerMillion)),
                 primary_col = "InfActiveCasesPerMillion",
                 rounding_func = function(x){scale_signif(x,2)},
                 legend_title =  "Infections per million",
                 quant_grades = 5,
                 pal_reverse = FALSE
    )
  })
  
  output$graph4header<-renderUI({withMathJax(HTML("
    
    5. Arrivals data are estimated based on historic arrivals data on <a href='http://infoshare.stats.govt.nz/infoshare/'>Statistics New Zealand Infoshare</a>. For each country, 
    they include residents of that country travelling to New Zealand and New Zealand citizens returning to New Zealand from that country (Figure 5).

<br /><br />

    
    
    Figure 5: New Zealand arrivals by month (2019 figures, estimated)"))})
  output$graph4<-renderLeaflet({
    
    show_leaflet(data_to_show = filtered_mapped_world_with_covid_data %>% filter(!is.na(Total2019MonthlyArrivals)),
                 primary_col = "Total2019MonthlyArrivals",
                 rounding_func = function(x){scale_signif(x,2)},
                 legend_title =  "NZ arrivals by month",
                 quant_grades = 6,
                 pal_reverse = FALSE
    )
  })
  output$graph5header<-renderUI({withMathJax(HTML(paste0(
    
    "
    
    6. We can then calculate the probability that we'll get one or more cases \\(P(c>0)\\) 
arriving from each source country in a month (Figure 6) by multiplying the number of travellers by the prevalence of covid-19 in each country.
  
  
  If we assume cases are independent, we can use the following formula:

$$P(c>0) = 1-(\\frac{p-i_\\textrm{estimated, today}}{p})^a$$

<br /><br />

    
    
    Figure 6: Probability of one or more cases arriving each month from residents of each country, based on arrival figures from this country in ",
    month_name,
    " 2019.")))})
  output$graph5<-renderLeaflet({
    
    #get the maximum number of bins we can have, considering the distribution of the data
    display_data <- filtered_mapped_world_with_covid_data %>% filter(!is.na(ProbabilityOfMoreThanZeroCases))
    col_of_interest <-"ProbabilityOfMoreThanZeroCases"
    
    show_leaflet(data_to_show =  display_data ,
                 primary_col = col_of_interest,
                 rounding_func = function(x){scales::percent(x,accuracy = 0.01)},
                 legend_title =  "Probability",
                 quant_grades = get_max_quantiles(display_data,col_of_interest,max_quantiles=6),
                 pal_reverse = FALSE
    )
  })
  output$graph6header<-renderUI({
    withMathJax(HTML(paste0(
    "
    
7. Finally we can calculate the <em>expected number</em> of cases from each country (Figure 7). 
This is simply the estimated infection rate multiplied by the number of arrivals.

<br /><br />
    
    
    
    Figure 7: Expected number of cases to arrive each month from residents of each country, based on arrival figures from this country in ",
    month_name,
    " 2019.")))})
  output$graph6<-renderLeaflet({
    
    display_data <- filtered_mapped_world_with_covid_data %>% filter(!is.na(ExpectedCasesAtBorder))
    col_of_interest <-"ExpectedCasesAtBorder"
    
    
    binpal <- colorBin("YlOrRd",
                       display_data$ExpectedCasesAtBorder,
                       domain=c(0,max(display_data$ExpectedCasesAtBorder,na.rm = TRUE)),
                       na.color=NA,
                       bins=c(0,0.01,0.1,1,
                              10^c(1:ceiling(log10(
                                max(display_data$ExpectedCasesAtBorder,na.rm = TRUE)))
                              )), pretty = FALSE)
    
    
    
    show_leaflet(data_to_show = display_data,
                 primary_col = col_of_interest,
                 rounding_func = function(x){scale_signif(x,3)},
                 legend_title =  "Expected monthly\n cases arriving",
                 custom_palette = binpal
    )
  })

  output$graph8header<-renderUI({withMathJax(HTML(paste0(
    "
    As can be seen, rates vary widely from very, very low rates in East Asia through to very high rates in the United States and Europe.
<br /><br />

8. By assessing the reliability of the MIQ (Managed Isolation and Quarantine) system based on observed success so far, we can estimate the probability that a case, 
once arriving in New Zealand, will exit to the community, or spread to a MIQ worker, or otherwise lead to a case in the community.

We assessed this risk using based on 
<a href='https://www.tepunahamatatini.ac.nz/2020/07/16/effect-of-new-zealand-border-controls-on-covid-19-reincursion-risk/'>prior research</a> 
as well supplemental estimates of MIQ success rate.
<br /><br />
Roughly speaking, based on past experience, around 1 in 50 cases arriving in 14-day MIQ results in one case in the community.
More precise estimates are display on the \"Proposed system\" tab.
<br /><br />

We can then calculate the expected (average) number of arrivals from each location who exits MIQ still infectious.

<br /><br />
    
    
    
    Figure 8: Expected number of cases to arrive and reach the community after MIQ, based on arrival figures in ",
    month_name,
    " 2019.")))})
  output$graph8<-renderLeaflet({
    
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
                 legend_title =  "Expected monthly\n community cases",
                 custom_palette = binpal
                 
    )
  })
  
  output$paragraph_09 <- renderUI({withMathJax(HTML(paste0(
    "

<br /><br />

In countries where health systems are underdeveloped, it is unlikely that COVID-19 deaths will be accurately recorded. 

Because countries where health systems are underdeveloped may not accurately record deaths, 
the visualization only displays countries with a life expectancy of "
    ,as.character(life_exp_thresh),
    " or higher.")))})
  
}
