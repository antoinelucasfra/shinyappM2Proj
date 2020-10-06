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


# ### Identifier les pays qui n'ont pas de donnees
# 
# total_countries = read.csv("Countries-of-the-world.csv", sep =";", header = TRUE)
# total_countries_vector = as.vector(total_countries[,1])
# 
# year = 1990
# 
# dispo_countries_vector = as.vector(unique(suicide[suicide$year == year,]$country))
# 
# countries_dispo_or_not = total_countries_vector %in% dispo_countries_vector
# 
# dispo_countries = as.data.frame(dispo_countries_vector)
# dispo_countries$latitude = total_countries$Latitude[countries_dispo_or_not]
# dispo_countries$longitude = total_countries$Longitude[countries_dispo_or_not]
# 
# missing_countries_vector = total_countries_vector[!countries_dispo_or_not]
# missing_countries = as.data.frame(missing_countries_vector)
# missing_countries$latitude = total_countries$Latitude[!countries_dispo_or_not]
# missing_countries$longitude = total_countries$Longitude[!countries_dispo_or_not]
