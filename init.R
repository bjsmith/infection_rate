

app_packages <- c(
  #shiny
  "shiny",
  "shinyjs",
  "dplyr",
  "shinybusy",
  
  #data
  "DT",
  "data.table",
  "dplyr",
  "lubridate",
  "readr",
  "stringr",
  "magrittr",
  "googlesheets4",
  "zoo",
  "tidyr",
  
  #visual
  "ggplot2", # tidyverse vis package
  "ggrepel",
  # "sf",
  "spData",
  "leaflet",
  # "rnaturalearth",
  # "rnaturalearthdata",
  "RColorBrewer"
)


install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}
invisible(sapply(app_packages, install_if_missing))

