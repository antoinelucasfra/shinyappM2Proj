# https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016

# Library 

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

# Cleanage des donnees

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

# Define cum of suicide to plot on the map 

suicide_country_cumul = suicide %>% group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
  summarise(total_suicide = sum(suicides_no),
            population = sum(population))
dim(suicide_country_cumul)
summary(suicide_country_cumul)

# Importation des borders
world <- read_sf("data/world")
