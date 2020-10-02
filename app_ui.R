# TODO LIST
#'régler problème énonciation des graphs
#'régler prblm add code source qq part
#'régler problème bornes slider
#'
#'
#'
#'
#'

source("./data_management")

#library 

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
library(sf)

#this is the script for the ui part of the app

ui <- shinyUI(
  navbarPage("Shiny_SuicideR",
             
             tabPanel("Suicide mapper",
                      selectInput(inputId = "idYear", label = "Annees", 
                                  choices = seq(1986,2016,1)),
                      leafletOutput("mymap")
                      
                      #add a legend for suicide amount (country colored with gradient of tiles/polygons to add)
                      
                      
                      
                      
             ),
             
             tabPanel("Region plots",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          pickerInput("country_select", "Country:",
                                      choices = levels(suicide$country),
                                      multiple = FALSE),
                          
                          # checkboxGroupButtons("sex_select","Sex:",
                          #                      choices = levels(suicide$sex)),
                          # 
                          # checkboxGroupButtons("age_select","Age:",
                          #                      choices = levels(suicide$age)),
                          # 
                          # checkboxGroupButtons("generation_select","Generation:",
                          #                      choices = levels(suicide$generation)),
                          
                          
                          "Select the country of your interest to have a clue on suicide evolution through time between different classes"
                          
                        ),
                        mainPanel(
                            plotlyOutput("plot_selected_sex"),
                            plotlyOutput("plot_selected_age"),
                            plotlyOutput("plot_selected_generation")
                          
                          
                          
                        )
                      )
                      
             ),
             
             tabPanel("Country Ranking",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          sliderInput("country_number_select",
                                      "Select the number of country you want in the top:",
                                      min = 1, max = length(levels(suicide$country)),value = 5),
                          
                          sliderInput("date_length_select",
                                      "Select the interval of time:",
                                      min = 1985, max = 2016, value = c(1985,2016))
                            ),
                        
                        mainPanel(
                          plotlyOutput("evolution_increase"),
                          plotlyOutput("evolution_decrease"),
                          tableOutput("table_increase"),
                          tableOutput("table_decrease")
                        )
                      )
             ),
             
             
             tabPanel("Raw data",
                      
                      h2("Dataframe to explore raw data"),
                      dataTableOutput("dataTable_raw")
             ),
             
             tabPanel("About",
                      
                      h3("This application was built for a project during a course of Data Science specialisation in Agrocampus Ouest"),
                      HTML("<br>"),
                      hr(),
                      HTML("<br>"),
                      hr(),
                      h5("If you want to consult the source code, please refer to the following link :"),
                      uiOutput("tab")
                      
                      
                      
                      )
             
             )
  )




