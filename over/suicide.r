setwd("C:/Users/telli/Documents/ACO/3A/Divers - Programmation avec R/Projet")

library(tidyverse)
library(FactoMineR)


## Jeu de données 3 : les suicides
# https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016


### Cleanage des données

suicide = read.csv("suicide_coord.csv", sep = ";", header = T)
str(suicide)

suicide$country = as.factor(suicide$country)
suicide$year = as.factor(suicide$year)
suicide$sex = as.factor(suicide$sex)
suicide$age = as.factor(suicide$age)
suicide$generation = as.factor(suicide$generation)
suicide$Capital.Major.City = as.factor(suicide$Capital.Major.City)
suicide$Latitude = as.integer(suicide$Latitude)

str(suicide)


### Graphes par pays

# Ajouter les taux de suicide pour 100 000 habitants (pour pouvoir comparer des pays qui n'ont pas le mm nb d'habs)

country = "Albania"

suicide_country = suicide[suicide$country == country,]

suicide_country_cumul = suicide_country %>% group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
  summarise(total_suicide = sum(suicides_no),
            population = sum(population))

# Sexe

suicide_country_male = suicide_country[suicide_country$sex == "male",]
suicide_country_male_cumul = suicide_country_male %>% group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
  summarise(total_suicide = sum(suicides_no),
            population = sum(population))

suicide_country_female = suicide_country[suicide_country$sex == "female",]
suicide_country_female_cumul = suicide_country_female %>% group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
  summarise(total_suicide = sum(suicides_no),
            population = sum(population))

# Age

suicide_country_5_14 = suicide_country[suicide_country$age == "5-14 years",]
suicide_country_5_14_cumul = suicide_country_5_14 %>% group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
  summarise(total_suicide = sum(suicides_no),
            population = sum(population))

suicide_country_15_24 = suicide_country[suicide_country$age == "15-24 years",]
suicide_country_15_24_cumul = suicide_country_15_24 %>% group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
  summarise(total_suicide = sum(suicides_no),
            population = sum(population))

suicide_country_25_34 = suicide_country[suicide_country$age == "25-34 years",]
suicide_country_25_34_cumul = suicide_country_25_34 %>% group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
  summarise(total_suicide = sum(suicides_no),
            population = sum(population))

suicide_country_35_54 = suicide_country[suicide_country$age == "35-54 years",]
suicide_country_35_54_cumul = suicide_country_35_54 %>% group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
  summarise(total_suicide = sum(suicides_no),
            population = sum(population))

suicide_country_55_74 = suicide_country[suicide_country$age == "55-74 years",]
suicide_country_55_74_cumul = suicide_country_55_74 %>% group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
  summarise(total_suicide = sum(suicides_no),
            population = sum(population))

suicide_country_75 = suicide_country[suicide_country$age == "75+ years",]
suicide_country_75_cumul = suicide_country_75 %>% group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
  summarise(total_suicide = sum(suicides_no),
            population = sum(population))

# Génération

suicide_country_boomers = suicide_country[suicide_country$generation == "Boomers",]
suicide_country_boomers_cumul = suicide_country_boomers %>% group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
  summarise(total_suicide = sum(suicides_no),
            population = sum(population))

suicide_country_gi = suicide_country[suicide_country$generation == "G.I. Generation",]
suicide_country_gi_cumul = suicide_country_gi %>% group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
  summarise(total_suicide = sum(suicides_no),
            population = sum(population))

suicide_country_x = suicide_country[suicide_country$generation == "Generation X",]
suicide_country_x_cumul = suicide_country_x %>% group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
  summarise(total_suicide = sum(suicides_no),
            population = sum(population))

suicide_country_z = suicide_country[suicide_country$generation == "Generation Z",]
suicide_country_z_cumul = suicide_country_z %>% group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
  summarise(total_suicide = sum(suicides_no),
            population = sum(population))

suicide_country_millenials = suicide_country[suicide_country$generation == "Millenials",]
suicide_country_millenials_cumul = suicide_country_millenials %>% group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
  summarise(total_suicide = sum(suicides_no),
            population = sum(population))

suicide_country_silent = suicide_country[suicide_country$generation == "Silent",]
suicide_country_silent_cumul = suicide_country_silent %>% group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
  summarise(total_suicide = sum(suicides_no),
            population = sum(population))

# Graphiques --> à revoir

plot(suicide_country_cumul$year, suicide_country_cumul$total_suicide,
     type="l", cex.lab=1.25, cex.axis=1.25, cex.main=1.25,
     main = paste("Nombre de suicides par an en", country),
     xlab = "Year",
     ylab = "Number of suicides")

plot(suicide_country_male_cumul$year, suicide_country_male_cumul$total_suicide,
     type="l", cex.lab=1.25, cex.axis=1.25, cex.main=1.25,
     main = paste("Nombre de suicides par an en", country, ", chez les hommes"),
     xlab = "Year",
     ylab = "Number of suicides")


### Tableaux par année

# Ajouter les taux de suicide pour 100 000 habitants (pour pouvoir comparer des pays qui n'ont pas le mm nb d'habs)

year = 2010

suicide_year = suicide[suicide$year == year,]

suicide_year_cumul = suicide_year %>% group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
  summarise(total_suicide = sum(suicides_no),
            population = sum(population))
