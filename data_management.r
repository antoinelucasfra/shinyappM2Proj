#please setup your working directory 
# setwd("")

##install package if needed 

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


#load necessary packages 

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

# data management

suicide = read.csv("./data/suicide_coord.csv", sep = ";", header = T)

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
