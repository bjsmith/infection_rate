library(DT)
source("utils.R")
library(ggrepel)

life_exp_thresh <- 70
run_date<-Sys.Date()
month_name <- format(run_date,"%B")

geo_world_basic_data <- get_geomapped_covid_data(life_exp_thresh,run_date)
geo_world_with_covid_data <- get_analysis_covid_data(geo_world_basic_data)
nogeo_world_basic_data <- get_geomapped_covid_data(life_exp_thresh,run_date,separate_aussie_states_and_hk = TRUE,include_geo_data = FALSE)
world_with_covid_data <- get_analysis_covid_data(nogeo_world_basic_data)




### now set up specific datasets for each output


vals_to_include <- (is.finite(geo_world_with_covid_data$InferredDetectionRate) & !is.na(geo_world_with_covid_data$InferredDetectionRate)
                    
                    & geo_world_with_covid_data$LifeExp>=life_exp_thresh
)

inc_data_inf_det_rate<-geo_world_with_covid_data[vals_to_include,]





vals_to_include <- (
  is.finite(geo_world_with_covid_data$ActiveCasesPerThousand) & !is.na(geo_world_with_covid_data$ActiveCasesPerThousand)
  
  & geo_world_with_covid_data$LifeExp>=life_exp_thresh
)

inc_data_inf_cases_per_m<-geo_world_with_covid_data[vals_to_include,]
#pal<-colorQuantile(palette="Blues",domain= inc_data$ActiveCasesPerThousand,n=4)

#chloro_labels <- paste0(inc_data$name_long, ": ", as.character(round(inc_data$ActiveCasesPerThousand,1)))



vals_to_include <- (
  is.finite(geo_world_with_covid_data$ActiveCasesPerThousand) & !is.na(geo_world_with_covid_data$ActiveCasesPerThousand)
  
  & geo_world_with_covid_data$LifeExp>=life_exp_thresh
)

inc_data_cases_per_m<-geo_world_with_covid_data[vals_to_include,]
#pal<-colorQuantile(palette="Blues",domain= inc_data$ActiveCasesPerThousand,n=4)

#chloro_labels <- paste0(inc_data$name_long, ": ", as.character(round(inc_data$ActiveCasesPerThousand,1)))

vals_to_include <- (
  is.finite(geo_world_with_covid_data$InferredActiveCases) & !is.na(geo_world_with_covid_data$InferredActiveCases)
  
  & geo_world_with_covid_data$LifeExp>=life_exp_thresh
)

inc_data_inf_active_cases<-geo_world_with_covid_data[vals_to_include,]
#pal<-colorQuantile(palette="Blues",domain= inc_data$ActiveCasesPerThousand,n=4)

#chloro_labels <- paste0(inc_data$name_long, ": ", as.character(round(inc_data$ActiveCasesPerThousand,1)))

vals_to_include <- (
  is.finite(geo_world_with_covid_data$ActiveCases) & !is.na(geo_world_with_covid_data$ActiveCases)
  
  & geo_world_with_covid_data$LifeExp>=life_exp_thresh
)


inc_data_active_cases<-geo_world_with_covid_data[vals_to_include,]

inc_data_arrivals<-geo_world_with_covid_data
#pal<-colorQuantile(palette="Blues",domain= inc_data$ActiveCasesPerThousand,n=4)


vals_to_include <- (
  
  world_with_covid_data$LifeExp>=life_exp_thresh
)


display_table<-world_with_covid_data[vals_to_include,]
######set up general simulator

countries_to_choose_from<-
  world_with_covid_data$Location %>%
  sort %>%
  .[.!="New Zealand"]

# key_interest_countries <- c("US","China","Fiji","United Kingdom",
#                             "Japan","India","Cook Islands",
#                             "Samoa","Canada","Korea, South",
#                             "Germany","Indonesia","Taiwan*",
#                             "Singapore","Hong Kong SAR",
#                             "Thailand","Philippines",
#                             "Malaysia","France")

dql<-(
  is.finite(world_with_covid_data$ActiveCases) & !is.na(world_with_covid_data$ActiveCases) &
    is.finite(world_with_covid_data$ProbabilityOfMoreThanZeroCases) & !is.na(world_with_covid_data$ProbabilityOfMoreThanZeroCases) &
    world_with_covid_data$LifeExp>=life_exp_thresh
)
dql[is.na(dql)]<-TRUE
world_with_covid_data$DataQualityLow<-dql

get_intsim_dt<-function(country_filter,selected_probs="bubble",world_w_covid_data#,quarantine_odds_override,travel_volume_weighting=1
                        ){
  # world_w_covid_data <- get_analysis_covid_data(
  #   geo_world_basic_data,
  #   quarantine_odds_override=quarantine_odds_override,
  #   travel_volume_weighting=travel_volume_weighting)
  filtered_df <- world_w_covid_data %>%
    data.frame %>%
    filter(LifeExp>=life_exp_thresh) %>%
    filter(Location %in% country_filter) %>%
    arrange(InfActiveCasesPerMillion)
  
  if(selected_probs=="bubble"){
    df_to_return <- filtered_df %>%
      select(Location,ProbabilityOfMoreThanZeroCases,
           ExpectedNumberOfCases
    )
    percentage_cols <-c('ProbabilityOfMoreThanZeroCases')
    rounding_cols <-c('ExpectedNumberOfCases')
    dt_colnames<-c("Territory","Probability of 1 or more cases","Expected cases")
  }
  
  if(selected_probs=="quarantine"){
    df_to_return<- filtered_df %>%
    select(Location,ProbabilityOfMoreThanZeroCases,ProbabilityOfMoreThanZeroCommunityCases,
           ExpectedNumberOfCases,ExpectedNumberOfCasesInCommunity
    )
    percentage_cols <-c('ProbabilityOfMoreThanZeroCases','ProbabilityOfMoreThanZeroCommunityCases')
    rounding_cols <-c('ExpectedNumberOfCases','ExpectedNumberOfCasesInCommunity')
    dt_colnames<-c("Territory","Probability of 1 or more cases\n arriving in quarantine",
                   "Probability of 1 or more cases\n reaching community",
                "Expected cases in quarantine", "Expected cases reaching community")
  }

  return(DT::datatable(df_to_return,colnames=dt_colnames)%>%
           formatPercentage(percentage_cols,3) %>%
           formatRound(rounding_cols,2))
}

###########create data table for table tab

#lose geometry info and convert to datatable. note: not the same as data.table!

display_dt <- DT::datatable(
  display_table %>% 
    data.frame %>%
    arrange(InfActiveCasesPerMillion) %>%
    select(LocationCode, Location, Population, #total_cases,
           ActiveCases,InferredActiveCases,InfActiveCasesPerMillion,
           MonthlyArrivals,ProbabilityOfMoreThanZeroCases,ProbabilityOfMoreThanZeroCommunityCases,
           ExpectedNumberOfCases,ExpectedNumberOfCasesInCommunity
    ))


###########main dashboard.
source("map_page.R")
# Define server logic required to draw a histogram
server <- function(input, output) {
  #country page
  output$country_table<-DT::renderDataTable(
    display_dt %>%
      formatPercentage(c('ProbabilityOfMoreThanZeroCases','ProbabilityOfMoreThanZeroCommunityCases'),3) %>%
      formatRound(c('InferredActiveCases','InfActiveCasesPerMillion'),0,mark=",") %>%
      formatRound(c('ExpectedNumberOfCases','ExpectedNumberOfCasesInCommunity'),2) %>%
      formatRound(c('Population','ActiveCases','MonthlyArrivals'),0,mark=",")
  )
  
  #intervention simulation page
  
  output$intsim_notes<-
    renderUI({
      withMathJax(HTML(paste0("
<h4>Notes:</h4>
<br /><br />
Excludes impact from NZ residents returning.
Thus, current calculations doesn't take into account increased risk from NZ residents returning from countries allowed within our bubble.
Use the 'within our bubble' feature with caution.

")))})
  

  sim_geo_world_with_covid_data_bubble <- reactive({
    world_w_covid_data <- get_analysis_covid_data(
      nogeo_world_basic_data,
      quarantine_odds_override=(1/input$intsim_quarantine_failure_odds),
      travel_volume_weighting=input$intsim_percent_capacity/100)
    return(world_w_covid_data)
  })
  
  sim_geo_world_with_covid_data_quarantine <- reactive({
    world_w_covid_data <- get_analysis_covid_data(
      nogeo_world_basic_data,
      quarantine_odds_override=(1/input$intsim_quarantine_failure_odds),
      travel_volume_weighting=input$intsim_percent_capacity_with_quarantine/100)
    return(world_w_covid_data)
  })
  
  countries_bubble_df <- reactive({
    return(get_intsim_dt(input$intsim_countries_bubble,"bubble",
                         #quarantine_odds_override=(1/input$intsim_quarantine_failure_odds),
                         sim_geo_world_with_covid_data_bubble())
    )
  })

  countries_quarantine_df <- reactive({
    return(get_intsim_dt(input$intsim_countries_quarantine,"quarantine",
                         #quarantine_odds_override=(1/input$intsim_quarantine_failure_odds),
                         sim_geo_world_with_covid_data_quarantine())
    )
  })
  
  
  get_countries_in_bubble_risks <- reactive({
    sim_geo_world_with_covid_data_bubble() %>%
      data.frame %>%
      filter(LifeExp>=life_exp_thresh) %>%
      filter(Location %in% input$intsim_countries_bubble)
  })
  
  get_countries_out_of_bubble_risks <- reactive({
    sim_geo_world_with_covid_data_quarantine() %>%
      data.frame %>%
      filter(LifeExp>=life_exp_thresh) %>%
      filter(Location %in% input$intsim_countries_quarantine)
    #use the lower "community cases" figure here because these are going through quarantine.
  })
  
  get_foreign_risk <- reactive({
    foreign_risk <- rbind(
      get_countries_in_bubble_risks() %>%
        select(Location,
               "ExpectedCases"=ExpectedNumberOfCases),
      get_countries_out_of_bubble_risks() %>%
        select(Location,
               "ExpectedCases"=ExpectedNumberOfCasesInCommunity)
    )
    
  })
  
  get_nz_resident_risk <- reactive({
    data.frame("Location"="Returning NZers",
               "ExpectedCases"=39/input$intsim_quarantine_failure_odds)
  })
  
  
  output$total_risk_graph <- renderPlot({
    foreign_risk <- get_foreign_risk()
    
    
    #nz resident risk is the number of cases that have returned
    #roughly 39 - need to get a better figure
    #divided by our expected ratio of escaped cases
    nz_resident_risk_df <- get_nz_resident_risk()
    
    nz_resident_risk_label <- nz_resident_risk_df$Location[[1]]
    
    foreign_risk$Location <-
      factor(foreign_risk$Location,
             levels = c(unique(foreign_risk$Location),nz_resident_risk_label),
             ordered=TRUE)
    

      
    
    
    
    combined_risk_graph <- rbind(foreign_risk,nz_resident_risk_df)
    
    combined_risk_graph <- arrange(combined_risk_graph,desc(Location))
    combined_risk_graph$LabelPosition<-cumsum(combined_risk_graph$ExpectedCases)-combined_risk_graph$ExpectedCases/2
    combined_risk_graph <- arrange(combined_risk_graph,Location)
    color_palette = c(
      rep(
        c('#1f78b4','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#6a3d9a','#b15928'),
        ceiling(nrow(combined_risk_graph)/8))[1:nrow(foreign_risk)],
      '#222222' #new zealand
      )
    
    ggplot(combined_risk_graph,aes(x=1,y=ExpectedCases,fill=Location,label=Location))+
      geom_bar(stat="identity",alpha=0.8)+
      scale_x_continuous(name="",breaks=NULL)+
      scale_y_continuous(name="Expected cases per month",
                         breaks=0:10,minor_breaks = NULL, 
                         limits = c(0,max(nz_resident_risk_df$ExpectedCases*3,sum(combined_risk_graph$ExpectedCases))),
                         position="right")+
      scale_fill_manual(values = color_palette)+
      theme(legend.position = "none",legend.box="vertical",legend.margin=margin(),text=element_text(face = "bold"),
            axis.text = element_text(size=16)
            )+
      #guides(fill=guide_legend(nrow=2,byrow=TRUE))+
      geom_label_repel(aes(y=LabelPosition),color="white",fontface="bold")+
      coord_flip()
  })
  
  


  
  total_risk_text <- reactive({
    
    countries_excluded_due_to_data<-
      sim_geo_world_with_covid_data_bubble() %>%
      data.frame %>%
      #filter((name_long %in% input$intsim_countries_bubble) | name_long %in% (input$intsim_countries_qurantine)) %>%
      filter(Location %in% c(input$intsim_countries_bubble,input$intsim_countries_quarantine)) %>%
      filter(LifeExp<life_exp_thresh)
    
    
    #for countries that are let in without quarantine, we want to add 
    countries_in_bubble_risks <- get_countries_in_bubble_risks()$ProbabilityOfMoreThanZeroCases
    countries_out_of_bubble_risks <- get_countries_out_of_bubble_risks()$ProbabilityOfMoreThanZeroCommunityCases
    
    foreign_risk <- get_foreign_risk()
    nz_resident_risk <- get_nz_resident_risk()
    pct_nz_risk <- sum(nz_resident_risk$ExpectedCases)/(sum(foreign_risk$ExpectedCases)+sum(nz_resident_risk$ExpectedCases))
    
    #now...
    #how do we combine these? 
    #it is not as simple as adding each up. 
    #I think we have to multiply the complements then get the complement again.
    total_risk_prop<-1-prod(1-c(countries_in_bubble_risks,countries_out_of_bubble_risks))
    
    #now we need to add a warning for excluded countries.
    
    textout<-paste0(
      "Returning NZ Residents make up ",
      scales::percent(pct_nz_risk,accuracy=1),
      " of all expected cases.",
      "<br /> <br />",
      "The total risk from non-New Zealanders of exposing the community to 1 or more cases over a 1-month period is ",
                    scales::percent(total_risk_prop,accuracy = 0.01)
                    ,".\n\n"
                    
                    )
    
    if(length(countries_excluded_due_to_data$Location)>0){
      textout<-paste0(textout,
"<br /><br /> COVID-19 Data from the following countries is considered less reliable. 
Risk from these countries cannot be estimated at this time:" ,
paste0(countries_excluded_due_to_data$Location,collapse = ", "))
    }
    
    
    
    return(textout)
  })
  

  
  output$dt_countries_bubble<-DT::renderDataTable(
    countries_bubble_df() 
  )
  
  
  output$dt_countries_quarantine<-DT::renderDataTable(
    countries_quarantine_df() 
    # %>%
    #   formatPercentage(c('ProbabilityOfMoreThanZeroCases','ProbabilityOfMoreThanZeroCommunityCases'),3) %>%
    #   formatRound(c('ExpectedNumberOfCases','ExpectedNumberOfCasesInCommunity'),2)
  )
  output$intsim_totalrisk<-
    renderUI({
      withMathJax(HTML(paste0(
        total_risk_text()
      )))
    })

  
  #map page
  output$testt<-
    renderUI({
      withMathJax(HTML(
        map_page_notes(month_name,life_exp_thresh)
        ))
    })
  
  output$graph0header<-renderText({"Figure 1: Reported active cases now (average past 7 days)"})
  output$graph0 <- renderLeaflet({
    show_leaflet(data_to_show = inc_data_active_cases %>% filter(!is.na(ActiveCases)),
                 primary_col = "ActiveCases",
                 rounding_func = function(x){scales::comma(round(x,1))},
                 legend_title =  "Observed active cases",
                 quant_grades = 4,
                 pal_reverse = FALSE
    )
    
  })
  output$graph1header<-renderText({"Figure 2: Hit rate or inferred case detection rate"})
  output$graph1 <- renderLeaflet({
    show_leaflet(data_to_show = inc_data_inf_det_rate %>% filter(!is.na(InferredDetectionRate)),
                 primary_col = "InferredDetectionRate",
                 rounding_func = function(x){scales::percent(x,accuracy = 0.1)},
                 quant_grades = 3,
                 legend_title = "Inferred detection rate <br /> (current deaths over cases three weeks prior; <br /> countries with a life expectancy of 75 or greater)")
    
  })
  output$graph2header<-renderText({"Figure 3: Inferred active cases today"})
  output$graph2 <- renderLeaflet({
    show_leaflet(data_to_show = inc_data_inf_active_cases %>% filter(!is.na(InferredActiveCases)),
                 primary_col = "InferredActiveCases",
                 rounding_func = function(x){scales::comma(round(x,1))},
                 legend_title =  "Inferred active cases<br /> (likely to underestimate in countries<br /> with poor death recording)",
                 quant_grades = 4,
                 pal_reverse = FALSE
    )
    
  })
  output$graph3header<-renderText({"Figure 4: Active cases per million"})
  output$graph3<-renderLeaflet({
    
    show_leaflet(data_to_show = inc_data_inf_cases_per_m %>% filter(!is.na(InfActiveCasesPerMillion)),
                 primary_col = "InfActiveCasesPerMillion",
                 rounding_func = function(x){scales::comma(signif(x,2))},
                 legend_title =  "Inferred active cases per million people <br /> (likely to underestimate in countries<br /> with poor death recording)",
                 quant_grades = 4,
                 pal_reverse = FALSE
    )
  })
  
  output$graph4header<-renderText({"Figure 5: New Zealand visitor arrivals by month"})
  output$graph4<-renderLeaflet({
    
    show_leaflet(data_to_show = inc_data_arrivals %>% filter(!is.na(MonthlyArrivals)),
                 primary_col = "MonthlyArrivals",
                 rounding_func = function(x){scales::comma(signif(x,3))},
                 legend_title =  "NZ Visitor arrivals by month",
                 quant_grades = 5,
                 pal_reverse = FALSE
    )
  })
  output$graph5header<-renderText({paste0(
    "Figure 6: Probability of one or more cases arriving each month from residents of each country, based on arrival figures from this country in ",
    month_name,
    " 2019 by country of usual residence.")})
  output$graph5<-renderLeaflet({
    
    show_leaflet(data_to_show = inc_data_active_cases %>% filter(!is.na(ProbabilityOfMoreThanZeroCases)),
                 primary_col = "ProbabilityOfMoreThanZeroCases",
                 rounding_func = function(x){scales::percent(x,accuracy = 0.01)},
                 legend_title =  "insert legend title",
                 quant_grades = 5,
                 pal_reverse = FALSE
    )
  })
  output$graph6header<-renderText({paste0(
    "Figure 7: Expected number of cases to arrive each month from residents of each country, based on arrival figures from this country in ",
    month_name,
    "2019 by country of usual residence.")})
  output$graph6<-renderLeaflet({
    
    show_leaflet(data_to_show = inc_data_active_cases %>% filter(!is.na(ExpectedNumberOfCases)),
                 primary_col = "ExpectedNumberOfCases",
                 rounding_func = function(x){scales::comma(signif(x,2))},
                 legend_title =  "Expected number of cases to arrive each month",
                 quant_grades = 6,
                 pal_reverse = FALSE
    )
  })
  output$graph7header<-renderText({paste0(
    "Figure 8: Probability of one or more cases arrives and is quarantined but reaches the community, based on arrival figures from this country in ",
    month_name,
    " 2019 by country of usual residence.")})
  output$graph7<-renderLeaflet({
    
    show_leaflet(data_to_show = inc_data_active_cases %>% filter(!is.na(ProbabilityOfMoreThanZeroCommunityCases)),
                 primary_col = "ProbabilityOfMoreThanZeroCommunityCases",
                 rounding_func = function(x){scales::percent(x,accuracy = 0.01)},
                 legend_title =  "insert legend title",
                 quant_grades = 5,
                 pal_reverse = FALSE
    )
  })
  output$graph8header<-renderText({paste0(
    "Figure 9: Expected number of cases to arrive and be quarantined but still reach the community, based on arrival figures from this country in ",
    month_name,
    "2019 by country of usual residence.")})
  output$graph8<-renderLeaflet({
    
    show_leaflet(data_to_show = inc_data_active_cases %>% filter(!is.na(ExpectedNumberOfCasesInCommunity)),
                 primary_col = "ExpectedNumberOfCasesInCommunity",
                 rounding_func = function(x){scales::comma(signif(x,2))},
                 legend_title =  "Expected number of cases to arrive each month",
                 quant_grades = 6,
                 pal_reverse = FALSE
    )
  })
  
  
}

ui <- navbarPage(
  "Opening the border: What's the risk?",
  # tabPanel(
  #   "Intervention simulation 2",
  #   fluidPage(
  #     titlePanel("Intervention simulation 2"),
  #     sidebarLayout(
  #       sidebarPanel(
  #         selectInput("intsim2_countries_l2",
  #                     "Level 2",
  #                     choices = countries_to_choose_from,
  #                     selected = c("Queensland, Australia","Tasmania, Australia",
  #                                  "Australian Capital Territory, Australia",
  #                                  "Western Australia, Australia","South Australia, Australia",
  #                                  "Northern Territory, Australia",
  #                                  "Vietnam","Taiwan*","Thailand"
  #                                  #"Malaysia","Cambodia","Sri Lanka"
  #                                  ),
  #                     multiple=TRUE),
  #         numericInput("intsim2_percent_capacity_l2",
  #                      "Level 2 - expected travel volume (% of 2019)",
  #                      min=1,
  #                      max=100,step=1,
  #                      value = 80),
  #         selectInput("intsim2_countries_l3",
  #                     "Level 3",
  #                     choices = countries_to_choose_from,
  #                     multiple=TRUE),
  #         numericInput("intsim2_percent_capacity_l3",
  #                      "Level 3 - expected travel volume (% of 2019)",
  #                      min=1,
  #                      max=100,step=1,
  #                      value = 60),
  #         selectInput("intsim2_countries_l4",
  #                     "Level 4",
  #                     choices = countries_to_choose_from,
  #                     multiple=TRUE),
  #         numericInput("intsim2_percent_capacity_l4",
  #                      "Level 4 - expected travel volume (% of 2019)",
  #                      min=1,
  #                      max=100,step=1,
  #                      value = 60),
  #         uiOutput("intsim_notes")
  #         
  #       ),
  #     mainPanel(
  #       titlePanel("Total risk per month"),
  #       uiOutput("intsim2_totalrisk"),
  #       plotOutput("intsim2_total_risk_graph"),
  #       titlePanel("Risk from travelers from countries in our bubble"),
  #       DT::dataTableOutput("intsim2_dt_countries_bubble"),
  #       titlePanel("Risk from travelers from countries outside our bubble"),
  #       DT::dataTableOutput("intsim2_dt_countries_quarantine")
  #     )
  #     )
  #   )
  # ),
  # tabPanel(
  #   "Location Profiles",
  #   fluidPage(
  #     titlePanel("Location Profiles"),
  #     sidebarLayout(
  #       sidebarPanel(
  #         sidebarPanel(
  #           selectInput("Location",
  #                       "Select a location to profile:",
  #                       choices = countries_to_choose_from,
  #                       selected = c("Queensland, Australia","Tasmania, Australia",
  #                                    "Australian Capital Territory, Australia",
  #                                    "Western Australia, Australia","South Australia, Australia",
  #                                    "Northern Territory, Australia",
  #                                    "Vietnam","Taiwan*","Thailand"
  #                                    #"Malaysia","Cambodia","Sri Lanka"
  #                       ),
  #                       multiple=TRUE),
  #         )
  #         
  #   )
  # ),
  tabPanel(
    "Intervention simulation",
    fluidPage(
      titlePanel("Intervention simulation"),
      sidebarLayout(
        sidebarPanel(
          selectInput("intsim_countries_bubble",
                      "Select countries to enter our bubble (no quarantine):",
                      choices = countries_to_choose_from,
                      selected = c("Queensland, Australia","Tasmania, Australia",
                                   "Australian Capital Territory, Australia",
                                   "Western Australia, Australia","South Australia, Australia",
                                   "Northern Territory, Australia",
                                   "Vietnam","Taiwan*","Thailand"
                                   #"Malaysia","Cambodia","Sri Lanka"
                      ),
                      multiple=TRUE),
          numericInput("intsim_percent_capacity",
                       "When a country enters our bubble (no quarantine),
                       incoming travelers arrive at what percent of full capacity?",
                       min=1,
                       max=100,step=1,
                       value = 80),
          selectInput("intsim_countries_quarantine",
                      "Select countries to allow travelers from, under quarantine:",
                      choices = countries_to_choose_from,
                      multiple=TRUE,
                      selected = c("Korea, South")),
          numericInput("intsim_percent_capacity_with_quarantine",
                       "When residents from a particular country are allowed to enter NZ, passing through quarantine first,
                       incoming travelers arrive at what percent of full capacity?",
                       min=1,
                       max=100,step=1,
                       value = 40),
          numericInput("intsim_quarantine_failure_odds",
                       "If someone who arrives in NZ with COVID19 and is quarantined,\nand they exit quarantine, the odds they are still contagious are 1 in ",
                       min=5,
                       max=10000,step=10,
                       value = 12),
          
          uiOutput("intsim_notes")
          
        ),
        mainPanel(
          titlePanel("Total risk per month"),
          uiOutput("intsim_totalrisk"),
          plotOutput("total_risk_graph"),
          titlePanel("Risk from travelers from countries in our bubble"),
          DT::dataTableOutput("dt_countries_bubble"),
          titlePanel("Risk from travelers from countries outside our bubble"),
          DT::dataTableOutput("dt_countries_quarantine")
        )
      )
    )
  ),

    tabPanel(
    "Country List",
    fluidPage(
      titlePanel("COVID-19: List of countries and locations"),
      mainPanel(
        DT::dataTableOutput("country_table")
      )
    )
  ),
  tabPanel(
   "Method and assumptions",
   
   fluidPage(
     # Application title
     titlePanel("COVID-19: Prevalence around the world"),
     
     # Sidebar with a slider input for number of bins 
     sidebarLayout(
       sidebarPanel(
         # sliderInput("bins",
         #             "Number of bins:",
         #             min = 1,
         #             max = 50,
         #             value = 30),
         uiOutput("testt")
         #textOutput("")
         
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
         textOutput("graph7header"),
         leafletOutput("graph7"),
         textOutput("graph8header"),
         leafletOutput("graph8")
       )
     )
   ))


)

# Run the application 
shinyApp(ui = ui, server = server)

