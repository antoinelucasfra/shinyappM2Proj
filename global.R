## global.R
# Load libraries (assumes you've run setup.R if packages were missing)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(leaflet)
library(plotly)
library(tidyverse)
if (requireNamespace("sf", quietly = TRUE)) {
  library(sf)
} else {
  message(
    "Package 'sf' not available; polygon maps will be disabled. Install 'sf' for full functionality."
  )
}
library(mapview)

# Load data and perform parsing/validation (from data_management.R)
source("data_management.R")

# Make sure the country choices are deterministic and available for UI construction
country_choices <- c("Monde", sort(unique(as.character(suicide$country))))
