# Install package if needed 

if(!require(shiny)) install.packages("shiny")
if(!require(shinydashboard)) install.packages("shinydashboard")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(shinyWidgets)) install.packages("shinyWidgets")
if(!require(plotly)) install.packages("plotly")
if(!require(readxl)) install.packages("readxl")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(FactoMineR)) install.packages("FactoMineR")
if(!require(Factoshiny)) install.packages("Factoshiny")
if(!require(glue)) install.packages("glue")
if(!require(rgdal)) install.packages("rgdal")
if(!require(sf)) install.packages("sf")
if(!require(shinythemes)) install.packages("shinythemes")
if(!require(mapview)) install.packages("mapview")

# Load necessary packages 

library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(shinyWidgets)
library(plotly)
library(readxl)
library(tidyverse)
library(FactoMineR)
library(Factoshiny)
library(glue)
library(rgdal)
library(sf)
library(shinythemes)
library(mapview)

# Data management

suicide = read.csv("data/suicide_coord.csv", sep = ";", header = T)

suicide$country = as.factor(suicide$country)
suicide$sex = as.factor(suicide$sex)
suicide$age = as.factor( suicide$age)
suicide$age <- factor(suicide$age,levels = c("5-14 years","15-24 years",
                                             "25-34 years","35-54 years","55-74 years","75+ years"))
suicide$generation = as.factor(suicide$generation)
suicide$Capital.Major.City = as.factor(suicide$Capital.Major.City)
suicide$Latitude = as.integer(suicide$Latitude)
suicide$Longitude = as.integer(suicide$Longitude)

# Borders importation

world <- read_sf("data/world")

# Please setup your working directory 
# setwd("")

source("data_management.r")
# source("./ui.R")

server <- function(input,output,session)
{
   ### Map panel
   
   # Reactive dataframe for leaflet output
   
   suicide_map <- reactive({
      
      suicide_year_cumul <- suicide %>% group_by(country,year,
                                                 Capital.Major.City,Latitude,Longitude) %>%
         summarise(total_suicide = sum(suicides_no), population = sum(population)) %>% 
         filter(year %in% input$idYear) %>% 
         mutate(ratio = total_suicide / population * 100000)
      
      world <- world %>%
         filter(NAME %in% suicide_year_cumul$country)
      
      suicide_year_cumul <- suicide_year_cumul %>%
         mutate(country = as.character(country)) %>% 
         filter(country %in% world$NAME)
      
      merge(suicide_year_cumul, world, by.x = "country", by.y = "NAME") %>% st_sf()
      
   })
   
   # Reactive text for leaflet output
   
   mytext <- reactive({
      
      paste(suicide_map()$country," : ", round(suicide_map()$ratio, digits = 1),
            "suicides per 100k habs in ", input$idYear)
      
   })
   
   # Leaflet output
   
   map <- reactive({
      
      leaflet(suicide_map()) %>%
         addTiles() %>%
         setView(lat=46.2,lng=2.2,zoom=1.5) %>%
         addPolygons(fillColor = ~colorNumeric(palette="Reds", domain = ratio, na.color = "transparent")(ratio),
                     stroke=FALSE,
                     fillOpacity = 1,
                     label = mytext(),
                     highlight = highlightOptions(weight = 5, color = "white",
                                                  bringToFront = TRUE)) %>%
         addLegend(pal = colorNumeric(palette="Reds", domain = suicide_map()$ratio), 
                   values = ~ratio, opacity = 0.7, title = "Suicides per 100k inhabitants",
                   position = "bottomright")
   })
   
   output$mymap <- renderLeaflet({
      
      map()
      
   })
   
   user_map <- reactive({
      
      map() %>% 
         # Store the view based on UI
         setView(lng = input$mymap_center$lng
                 , lat = input$mymap_center$lat
                 , zoom = input$mymap_zoom)
      
   })
   
   output$map_dl <- downloadHandler(
      
      filename = "leafletmap.pdf",
      content = function(file) {
         mapshot( x = user_map()
                  , file = file
                  , cliprect = "viewport" # The clipping rectangle matches the height & width from the viewing port
                  , selfcontained = FALSE) # When this was not specified, the function for produced a PDF of two pages: one of the leaflet map, the other a blank page
      }
   )
   
   ### Plot panel
   
   # Global plot 
   
   output$plot_global <- renderPlotly({
      
      if (input$country_select == "Monde"){
         
         suicide %>% group_by(year) %>% 
            summarise(suicide_100k = sum(suicides_no) / sum(population) * 100000) %>% 
            ggplot(aes(x = year, y = suicide_100k)) + geom_line(size = 2) +
            ggtitle("Evolution of the number of suicide per 100k habs") +
            scale_color_viridis_d() +
            labs(y = "Number of suicide per 100k habs") +
            theme_bw() 
      }
      
      else { 
         
         suicide %>% group_by(country, year) %>% 
            filter(country == input$country_select) %>%
            summarise(suicide_100k = sum(suicides_no) / sum(population) * 100000) %>% 
            ggplot(aes(x = year, y = suicide_100k)) + geom_line(size = 2) +
            ggtitle("Evolution of the number of suicide per 100k habs") +
            scale_color_viridis_d() +
            labs(y = "Number of suicide per 100k habs") +
            theme_bw() 
      }
      
   })
   
   # Sex plot
   
   output$plot_selected_sex <- renderPlotly({
      
      if (input$country_select == "Monde"){
         suicide %>% group_by(year, sex) %>% 
            summarise(suicide_100k = sum(suicides_no) / sum(population) * 100000) %>% 
            ggplot(aes(x = year, y = suicide_100k, color = sex)) + geom_line(size = 2) +
            ggtitle("Evolution of the number of suicide per 100k habs between genders.") +
            scale_color_viridis_d() +
            labs(y = "Number of suicide per 100k habs") +
            theme_bw() 
      }
      
      else {
         
         suicide %>% group_by(country,year,sex) %>% 
            filter(country == input$country_select) %>%
            summarise(suicide_100k = sum(suicides_no) / sum(population) * 100000) %>% 
            ggplot(aes(x = year, y = suicide_100k, color = sex)) + geom_line(size = 2) +
            ggtitle("Evolution of the number of suicide per 100k habs between genders.") +
            scale_color_viridis_d() +
            labs(y = "Number of suicide per 100k habs") +
            theme_bw() 
      }
      
   })
   
   # Age plot
   
   output$plot_selected_age <- renderPlotly({
      
      if (input$country_select == "Monde"){
         
         suicide %>% group_by(year,age) %>% 
            summarise(suicide_100k = sum(suicides_no) / sum(population) * 100000) %>% 
            ggplot(aes(x = year, y = suicide_100k, color = age)) + geom_line(size = 2) +
            ggtitle("Evolution of the number of suicide per 100k habs between age categories.") +
            scale_color_viridis_d() +
            labs(y = "Number of suicide per 100k habs") +
            theme_bw() 
      }
      
      else {
         
         suicide %>% group_by(country,year,age) %>% 
            filter(country == input$country_select) %>%
            summarise(suicide_100k = sum(suicides_no) / sum(population) * 100000) %>% 
            ggplot(aes(x=year,y=suicide_100k, color = age)) + geom_line(size = 2) +
            ggtitle("Evolution of the number of suicide per 100k habs between age categories.") +
            scale_color_viridis_d() +
            labs(y = "Number of suicide per 100k habs") +
            theme_bw() 
      }
      
   })
   
   # Generation plot
   
   output$plot_selected_generation <- renderPlotly({
      
      if (input$country_select == "Monde"){
         
         suicide %>% group_by(year,generation) %>% 
            summarise(suicide_100k = sum(suicides_no) / sum(population) * 100000) %>% 
            ggplot(aes(x = year, y = suicide_100k, color = generation)) + geom_line(size = 2) +
            ggtitle("Evolution of the number of suicide per 100k habs between generations.") +
            scale_color_viridis_d() +
            labs(y = "Number of suicide per 100k habs") +
            theme_bw() 
      }
      
      else {
         
         suicide %>% group_by(country,year,generation) %>% 
            filter(country == input$country_select) %>%
            summarise(suicide_100k = sum(suicides_no) / sum(population) * 100000) %>% 
            ggplot(aes(x=year,y=suicide_100k, color = generation)) + geom_line(size = 2) +
            ggtitle("Evolution of the number of suicide per 100k habs between generations") +
            scale_color_viridis_d() +
            labs(y = "Number of suicide per 100k habs") +
            theme_bw() 
      }
      
   })
   
   ### Country ranking panel
   
   # Reactive definition to select top countries
   
   # Lowest
   
   low <- reactive({
      
      if(input$indicator_select == "suicide rates per 100k habs"){
         
         suicide %>% group_by(year,country) %>% 
            filter(year >= input$date_length_select[1] & year <= input$date_length_select[2]) %>%
            summarise(total_suicide100k = sum(suicides_no) / sum(population) * 100000) %>% ungroup() %>% 
            group_by(country) %>% 
            summarise(suicide_100k = mean(total_suicide100k)) %>% 
            dplyr::arrange(suicide_100k) %>%
            slice(1:input$country_number_select)
      }
      
      else{
         
         suicide %>% group_by(year,country) %>% 
            filter(year >= input$date_length_select[1] & year <= input$date_length_select[2]) %>%
            summarise(total_suicide = sum(suicides_no)) %>% ungroup() %>% 
            group_by(country) %>% 
            summarise(suicide_total = mean(total_suicide)) %>% 
            dplyr::arrange(suicide_total) %>%
            slice(1:input$country_number_select)
      }
      
   })
   
   # Highest
   
   high <- reactive({
      
      if(input$indicator_select == "suicide rates per 100k habs"){
         suicide %>% group_by(year,country) %>% 
            filter(year >= input$date_length_select[1] & year <= input$date_length_select[2]) %>%
            summarise(total_suicide100k = sum(suicides_no) / sum(population) * 100000) %>% ungroup() %>% 
            group_by(country) %>% 
            summarise(suicide_100k = mean(total_suicide100k)) %>% 
            dplyr::arrange(desc(suicide_100k)) %>%
            slice(1:input$country_number_select)
      }
      
      else{
         suicide %>% group_by(year,country) %>% 
            filter(year >= input$date_length_select[1] & year <= input$date_length_select[2]) %>%
            summarise(total_suicide = sum(suicides_no)) %>% ungroup() %>% 
            group_by(country) %>% 
            summarise(suicide_total = mean(total_suicide)) %>% 
            dplyr::arrange(desc(suicide_total)) %>%
            slice(1:input$country_number_select)
      }
      
   })
   
   # Output definition for country rankings
   
   output$high_rank<- renderText({
      
      paste("Top ",
            input$country_number_select,
            "country with the highest",input$indicator_select,"between",
            input$date_length_select[1],"and",input$date_length_select[2],".")
      
   })
   
   output$table_high <- renderTable({
      
      high()
      
   })
   
   output$low_rank <- renderText({
      
      paste("Top ",
            input$country_number_select,
            "country with the lowest",input$indicator_select,"between",
            input$date_length_select[1],"and",input$date_length_select[2],".")
      
   })
   
   output$table_low <- renderTable({
      
      low()
      
   })
   
   # Add a download button
   
   output$download_data <- downloadHandler(
      
      filename = function(){
         paste("top",input$country_number_select,"country_ranking.csv")
      },
      
      content = function(file){
         
         # Create the file to download
         
         low_add <- low() %>% mutate(rank_categ = rep("low",nrow(low())))
         high_add <- high() %>% mutate(rank_categ = rep("high",nrow(high())))
         all_add <- rbind(low_add,high_add)
         
         # Write the csv file
         
         write.csv(all_add, file, row.names = FALSE)
      }
   )
   
   ### Data panel
   
   output$dataTable_raw <- renderDataTable({
      
      suicide
      
   })
   
   ### About panel
   
   # Add the link for data informations
   
   link <- a("Link for the raw data and brief explanation",
             href="https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016")
   
   output$link_data <- renderUI({
      
      tagList(link)
      
   })
   
   # Add the link for github repo for source code
   
   url <- a("Github repo for the source code of the app",
            href="https://github.com/antoinelucasfra/shinyappM2Proj")
   
   output$git_repo <- renderUI({
      
      tagList(url)
      
   })
}
