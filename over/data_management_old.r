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

### Cleanage des donnees

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

#define cum of suicide to plot on the map 
suicide_country_cumul = suicide %>% group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
  summarise(total_suicide = sum(suicides_no),
            population = sum(population))


# Ajouter les taux de suicide pour 100 000 habitants (pour pouvoir comparer des pays qui n'ont pas le mm nb d'habs)


# suicide %>% group_by(country,year,Capital.Major.City,Latitude,Longitude) %>% 
#   summarise(total_suicide = sum(suicides_no),population=sum(population))
# 
# country = "Albania"
# 
# suicide_country = suicide[suicide$country == country,]


# # Sexe
# 
# suicide_country_male = suicide_country[suicide_country$sex == "male",]
# suicide_country_male_cumul = suicide_country_male %>% group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
#   summarise(total_suicide = sum(suicides_no),
#             population = sum(population),
#             suicides_per_100k_habs = total_suicide / population * 100000)
# 
# suicide_country_female = suicide_country[suicide_country$sex == "female",]
# suicide_country_female_cumul = suicide_country_female %>% group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
#   summarise(total_suicide = sum(suicides_no),
#             population = sum(population),
#             suicides_per_100k_habs = total_suicide / population * 100000)
# 
# # Age
# missing_countries$latitude = total_countries$Latitude[!countries_dispo_or_not]
# missing_countries$longit
# suicide_country_5_14 = suicide_country[suicide_country$age == "5-14 years",]
# suicide_country_5_14_cumul = suicide_country_5_14 %>% group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
#   summarise(total_suicide = sum(suicides_no),
#             population = sum(population),
#             suicides_per_100k_habs = total_suicide / population * 100000)
# 
# suicide_country_15_24 = suicide_country[suicide_country$age == "15-24 years",]
# suicide_country_15_24_cumul = suicide_country_15_24 %>% group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
#   summarise(total_suicide = sum(suicides_no),
#             population = sum(population),
#             suicides_per_100k_habs = total_suicide / population * 100000)
# 
# suicide_country_25_34 = suicide_country[suicide_country$age == "25-34 years",]
# suicide_country_25_34_cumul = suicide_country_25_34 %>% group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
#   summarise(total_suicide = sum(suicides_no),
#             population = sum(population),
#             suicides_per_100k_habs = total_suicide / population * 100000)
# 
# suicide_country_35_54 = suicide_country[suicide_country$age == "35-54 years",]
# suicide_country_35_54_cumul = suicide_country_35_54 %>% group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
#   summarise(total_suicide = sum(suicides_no),
#             population = sum(population),
#             suicides_per_100k_habs = total_suicide / population * 100000)
# 
# suicide_country_55_74 = suicide_country[suicide_country$age == "55-74 years",]
# suicide_country_55_74_cumul = suicide_country_55_74 %>% group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
#   summarise(total_suicide = sum(suicides_no),
#             population = sum(population),
#             suicides_per_100k_habs = total_suicide / population * 100000)
# 
# suicide_country_75 = suicide_country[suicide_country$age == "75+ years",]
# suicide_country_75_cumul = suicide_country_75 %>% group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
#   summarise(total_suicide = sum(suicides_no),
#             population = sum(population),
#             suicides_per_100k_habs = total_suicide / population * 100000)
# 
# # G?n?ration
# 
# suicide_country_boomers = suicide_country[suicide_country$generation == "Boomers",]
# suicide_country_boomers_cumul = suicide_country_boomers %>% group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
#   summarise(total_suicide = sum(suicides_no),
#             population = sum(population),
#             suicides_per_100k_habs = total_suicide / population * 100000)
# 
# suicide_country_gi = suicide_country[suicide_country$generation == "G.I. Generation",]
# suicide_country_gi_cumul = suicide_country_gi %>% group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
#   summarise(total_suicide = sum(suicides_no),
#             population = sum(population),
#             suicides_per_100k_habs = total_suicide / population * 100000)
# 
# suicide_country_x = suicide_country[suicide_country$generation == "Generation X",]
# suicide_country_x_cumul = suicide_country_x %>% group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
#   summarise(total_suicide = sum(suicides_no),
#             population = sum(population),
#             suicides_per_100k_habs = total_suicide / population * 100000)
# 
# suicide_country_z = suicide_country[suicide_country$generation == "Generation Z",]
# suicide_country_z_cumul = suicide_country_z %>% group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
#   summarise(total_suicide = sum(suicides_no),
#             population = sum(population),
#             suicides_per_100k_habs = total_suicide / population * 100000)
# 
# suicide_country_millenials = suicide_country[suicide_country$generation == "Millenials",]
# suicide_country_millenials_cumul = suicide_country_millenials %>% group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
#   summarise(total_suicide = sum(suicides_no),
#             population = sum(population),
#             suicides_per_100k_habs = total_suicide / population * 100000)
# 
# suicide_country_silent = suicide_country[suicide_country$generation == "Silent",]
# suicide_country_silent_cumul = suicide_country_silent %>% group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
#   summarise(total_suicide = sum(suicides_no),
#             population = sum(population),
#             suicides_per_100k_habs = total_suicide / population * 100000)

### Tableaux par annee

# suicide_year = suicide[suicide$year == year,]
# 
# suicide_year_cumul = suicide_year %>% group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
#   summarise(total_suicide = sum(suicides_no),
#             population = sum(population))


### Identifier les pays qui n'ont pas de donnees

total_countries = read.csv("Countries-of-the-world.csv", sep =";", header = TRUE)
total_countries_vector = as.vector(total_countries[,1])

year = 1990

dispo_countries_vector = as.vector(unique(suicide[suicide$year == year,]$country))

countries_dispo_or_not = total_countries_vector %in% dispo_countries_vector

dispo_countries = as.data.frame(dispo_countries_vector)
dispo_countries$latitude = total_countries$Latitude[countries_dispo_or_not]
dispo_countries$longitude = total_countries$Longitude[countries_dispo_or_not]

missing_countries_vector = total_countries_vector[!countries_dispo_or_not]
missing_countries = as.data.frame(missing_countries_vector)
missing_countries$latitude = total_countries$Latitude[!countries_dispo_or_not]
missing_countries$longitude = total_countries$Longitude[!countries_dispo_or_not]

