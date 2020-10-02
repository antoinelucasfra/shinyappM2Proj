#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyServer(function(input, output, session) {
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      # addMarkers(data = points()) 
      addCircleMarkers(
        lng = suicide_country_cumul$Longitude[suicide_country_cumul$year == input$idYear],
        lat =  suicide_country_cumul$Latitude[suicide_country_cumul$year == input$idYear],
        radius = suicide_country_cumul$total_suicide[suicide_country_cumul$year == input$idYear]/ suicide_country_cumul$population[suicide_country_cumul$year == input$idYear]*50000,
        color = "red",
        label = paste("Pays :", suicide_country_cumul$country[suicide_country_cumul$year == input$idYear],"; Nombre de suicide :", suicide_country_cumul$total_suicide[suicide_country_cumul$year == input$idYear]),
        labelOptions = labelOptions(textsize = "15px", direction = "auto")
        )
      })
  
  })
  
  

