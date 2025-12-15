## Small app launcher
# Load helper / data
if (file.exists("global.R")) {
  source("global.R")
}

# Source UI and server
source("ui.R")
source("server.R")

# Launch app
shiny::shinyApp(ui = ui, server = server)
