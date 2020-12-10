library(shiny)
#summary_map.R
#https://stackoverflow.com/questions/64001136/setting-up-two-rshiny-input-values-for-the-same-value-across-different-tab-panel/64001953#64001953
get_simsettings_tabPanel <- function(defaults, last_jh_date){
  return(
######### BEGIN TABPANEL
    tabPanel(
      "Simulation settings",
      fluidPage(
        useShinyjs(),#have to put this somewhere; I've arbitrarily put it in the simSettings page.
        titlePanel("Simulation Settings"),
        mainPanel(
          fixedPage(
            # fixedRow(
            #   column(4,"sidebar"),
            #   column(8, "main")
            # ),
            fixedRow(
              column(4,
                     numericInput("simsettings_ifr",
                                  "Assumed Infection Fatality Rate (%):",
                                  min=0,
                                  max=5,step=0.1,
                                  value = defaults[["default_assumed_ifr_percent"]])
              ),
              column(8,textOutput("ifr_explanation"))
            ),hr(),
            fixedRow(
              column(4,
                     numericInput("simsettings_traveler_relative_prevalence",
                                  "Prevalence of COVID-19 in travellers relative to the population:",
                                  min=0,
                                  max=10,step=0.1,
                                  value = defaults[["default_traveler_relative_prevalence"]])
              ),
              column(8,uiOutput("simsettings_traveler_relative_prevalence_description"))
            ),hr(),
            fixedRow(
              column(4,
                     numericInput("simsettings_current_lockdown_passenger_volume",
                                  "Current monthly incoming passenger volume:",
                                  min=0,
                                  max=10^5,step=1000,
                                  value = defaults[["default_current_lockdown_passenger_volume"]])
                     
              ),
              column(8,uiOutput("simsettings_current_lockdown_passenger_volume_description"))
            ),hr(),
            fixedRow(
              column(4,
                     dateInput("simsettings_run_date",
                               "Run date:",
                               value = defaults[["default_run_date"]],max = last_jh_date)
                     
              ),
              column(8,uiOutput("simsettings_run_date_description"))
            ),hr(),
            fixedRow(
              column(4,
                     selectInput(inputId = "simsettings_mode",
                                 label="Simulation mode",
                                 choices = c("Simple","Advanced"),
                                 selected = "Simple"
                     )
              ),
              column(8,uiOutput("simsettings_mode_description"))
            ),hr(),
            fixedRow(
              column(4,
                     passwordInput(inputId="simsettings_cache_reset_password",
                                   label="To clear cache, enter the correct password first:"),
                     actionButton(inputId="simsettings_clear_cache",
                                  "Clear Cache"
                     )
                     ),
              column(8,
                     uiOutput("simsettings_reset_description"))
            )
            )
          )
        )
      )
    )
######### END TABPANEL
    
}
render_simsettings <- function(input,output,session){
  
  output$ifr_explanation <- renderText({
    paste0(
      "Where there are recent COVID fatalities, this is compared to confirmed cases two weeks prior. ",
      "If the number of cases look too low, then a 'hit rate' is calculated by comparing those cases two weeks prior to fatalities now.",
      "Then, inferred active cases now is calculated by dividing the confirmed active cases by the hit rate.",
      "To do this we need to assume an infection fatality rate (IFR)."
      
    )
  })
  
  output$simsettings_traveler_relative_prevalence_description <- renderUI({HTML(
    "Travellers may be assumed to have different prevalences of Covid than the general population. For instance:
    (a) Travellers may be more mobile people with a higher number of contacts and consequently a higher prevalence. 
    On the other hand (b) travellers might 'self-screen' by voluntarily opting not to fly if they get sick, 
    and thus the prevalence of remaining travellers could also be lower than the general population.
    
    This setting is set to 1 by default, but can be adjusted to test the result under varying assumptions.
    "
  )})
  
  output$simsettings_current_lockdown_passenger_volume_description <- renderUI({HTML(
    "
    This determines the volume of passengers. 
    We allow the volume of passengers coming each month to be set independently from the proportion of passengers coming from each source country.
    This is because NZ Customs release up-to-date information about the volumes of passengers.
    Users may wish to retrieve up to date volume information from NZ Customs and input it here.
    The default setting is the level of travellers entering New Zealand in July 2020.
    Statistics New Zealand only release information about the proportion of passengers coming from each source location with a 2-3 month delay.
    "
  )})
    
  output$simsettings_run_date_description <- renderUI({HTML(
    "
    The Johns Hopkins University dataset active cases is updated daily. 
    Set the date here to view data based on this date. 
    The default run date is set to the last date for which we have manually updated data about local and imported cases.
    <br /><br />
    It is possible to set this setting to any other date and view the applicable prevalences and likelihoods of importing cases on those dates.
    However, manually updated data about local and imported cases will only be available for the default date, 
    so adjusting for local and imported cases should be disabled via the 'Proposed System' tab when viewing other dates.
    <br /><br />
    This value also determines the month in which passenger volumes are estimated.
    'Normal' levels are estimated from the corresponding calendar 2019 month. 
    Lockdown levels are estimated from the selected month, or if that month is not available, the latest available calendar month of data.
    Typically there is about a 2-month lag for Statistics NZ to release this data.
    "
  )})
    
  output$simsettings_mode_description <- renderUI({HTML(
    "
    This determines the tabs and settings that are available. 'Simple' mode is the default and provides a wide range of settings to adjust the app.
    'Advanced' mode provides even more customization options. 
    Specifically, countries can be individually selected for different passenger journeys.
    It also includes a 'validation' tab for comparing the model's prediction of cases arriving in NZ 
    against the actual number of cases in New Zealand.
    "
  )})
  
  output$simsettings_reset_description <- renderUI({HTML(
    "
    To reset the app cache, enter the admin password and click the reset button. This will also reset your session.
    "
  )})


}
