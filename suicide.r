setwd("C:/Users/telli/Documents/ACO/3A/Divers - Programmation avec R/Projet")

library(tidyverse)
library(FactoMineR)


## Jeu de données 3 : les suicides
# https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016

# Cleanage des données

suicide = read.csv("suicide.csv", header = T)
str(suicide)

colnames(suicide)[1] = "country"
colnames(suicide)[11] = "gdp_per_capita"

suicide$country = as.factor(suicide$country)
suicide$year = as.factor(suicide$year)
suicide$sex = as.factor(suicide$sex)
suicide$age = as.factor(suicide$age)
suicide$country.year = as.factor(suicide$country.year)
suicide$generation = as.factor(suicide$generation)

suicide = suicide[,-9]
suicide = suicide[,-9]

str(suicide)


# Graphes par pays

suicideAlbania = suicide[suicide$country == "Albania",]

suicideAlbaniacumul = suicideAlbania %>% group_by(country, gdp_per_capita, year) %>%
  summarise(total_suicide = sum(suicides_no),
            population = sum(population))

suicideAlbania_male = suicideAlbania[suicideAlbania$sex == "male",]
suicideAlbania_malecumul = suicideAlbania_male %>% group_by(country, gdp_per_capita, year) %>%
  summarise(total_suicide = sum(suicides_no),
            population = sum(population))

plot(suicideAlbaniacumul$year, suicideAlbaniacumul$total_suicide,
     type="l", cex.lab=1.25, cex.axis=1.25, cex.main=1.25,
     main="Nombre de suicides par an en Albanie",
     xlab="Year",
     ylab="Number of suicides")

plot(suicideAlbania_malecumul$year, suicideAlbania_malecumul$total_suicide,
     type="l", cex.lab=1.25, cex.axis=1.25, cex.main=1.25,
     main="Nombre de suicides par an en Albanie, chez les hommes",
     xlab="Year",
     ylab="Number of suicides")

# A faire : pour les femmes, pour les différentes catégories d'age, pour les différentes générations