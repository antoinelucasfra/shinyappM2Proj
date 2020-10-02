# TODO LIST
#'-régler problème reactive et rond gris
#'régler problème énonciation des graphs
#'régler prblm add code source qq part
#'
#'
#'
#'
#'

source("./suicide.r")

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

#this is the script for the ui part of the app

ui <- shinyUI(
  navbarPage("SuicideRY",
             
             tabPanel("Suicide mapper",
                      leafletOutput("mymap"),
                        
                        selectInput(inputId = "idYear", label = "Annees", 
                                    choices = seq(1986,2016,1))
                      

                      #add a legend for suicide amount :
                      
                      
                      
             ),
             
             tabPanel("Region plots",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          pickerInput("country_select", "Country:",
                                      choices = levels(suicide$country),
                                      multiple = FALSE), 
                          
                          checkboxGroupButtons("sex_select","Sex:",
                                               choices = levels(suicide$sex)),
                          
                          checkboxGroupButtons("age_select","Age:",
                                               choices = levels(suicide$age)),
                          
                          checkboxGroupButtons("generation_select","Generation:",
                                               choices = levels(suicide$generation)),
                          
                          
                          "Select the country, sex, age class and generation of your interest."
                          
                        ),
                        mainPanel(
                          plotOutput("plot_selected")
                        )
                      )
                      
             ),
             
             tabPanel("Factorial Analysis",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          checkboxGroupButtons("sex_select","Sex:",
                                               choices = levels(suicide$sex)),
                          
                          checkboxGroupButtons("age_select","Age:",
                                               choices = levels(suicide$age)),
                          
                          checkboxGroupButtons("generation_select","Generation:",
                                               choices = levels(suicide$generation)),
                          
                          
                          "Select the country, sex, age class and generation of your interest."
                          
                        ),
                        mainPanel(
                          plotOutput("plot_selected")
                        )
                      )
                      
             ),
             
             tabPanel("Data",
                      dataTableOutput("dataTable")
             ),
             
             tabPanel("About this app",
                      
                      h2("The author are : Zoe Wante, Chloé Tellier, Antoine Lucas"),
                      h3("This app was built in the context of a R programmation course")
                      )
  )
)




