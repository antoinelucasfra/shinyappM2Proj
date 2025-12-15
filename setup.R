## setup.R
# Install required packages for the Shiny app (run once)
packages <- c(
  "shiny",
  "shinythemes",
  "shinyWidgets",
  "shinydashboard",
  "tidyverse",
  "leaflet",
  "plotly",
  "sf",
  "mapview",
  "readxl"
)

## No test packages are installed by default. Add testing packages manually if needed.

inst <- packages[!packages %in% rownames(installed.packages())]
if (length(inst) > 0) {
  message("Installing packages: ", paste(inst, collapse = ", "))
  install.packages(inst)
} else {
  message("All packages already installed")
}

message(
  "Note: 'sf' may require system-level libraries (proj, gdal). If 'sf' fails to install, follow https://r-spatial.org/r/2021/04/27/r-spatial.html#installation for platform-specific help."
)
