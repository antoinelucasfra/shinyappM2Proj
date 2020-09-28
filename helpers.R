#library 

library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(shinyWidgets)
library(plotly)
library(readxl)

#data processing and management

suicide <- readxl::read_excel("./suicide_example.xlsx")

suicide_global <- read_excel('./suicide_coord.xlsx')

#functions to use in the app


### MAP FUNCTIONS ###

# function to plot suicide by year

#country,age,sex,generations,total_suicide
iris %>% filter(Species == "virginica")

#function to plot for a specified sex

sex_plot <- function(df,sex){
  
  g <- df %>% filter(Sex == sex)
    ggplot(aes(x=date,y=suicide, color = Sex)) + geom_line() +
    ylab("Amount of suicide per year") + 
    theme_bw() 
  
  return(g)
}

#function to plot for a specified Age class

sex_plot <- function(df,age_class){
  
  g <- df %>% filter(Age == age_class)
  ggplot(aes(x=date,y=suicide, color = Age)) + geom_line() +
    ylab("Amount of suicide per year") + 
    theme_bw() 
  
  return(g)
}

#function to plot for a specified generation

sex_plot <- function(df,generation){
  
  g <- df %>% filter(Generation == generation)
  ggplot(aes(x=date,y=suicide, color = Generation)) + geom_line() +
    ylab("Amount of suicide per year") + 
    theme_bw() 
  
  return(g)
}










