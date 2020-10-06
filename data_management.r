# setwd("C:/Users/telli/Documents/ACO/3A/Divers - Programmation avec R/Projet")

## Jeu de donnes 3 : les suicides
# https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016

### Cleanage des donnees

suicide = read.csv("./data/suicide_coord.csv", sep = ";", header = T)
str(suicide)

suicide$country = as.factor(suicide$country)
suicide$sex = as.factor(suicide$sex)
suicide$age = as.factor( suicide$age)
suicide$age <- factor(suicide$age,levels = c("5-14 years","15-24 years",
                                             "25-34 years","35-54 years","55-74 years","75+ years"))
suicide$generation = as.factor(suicide$generation)
suicide$Capital.Major.City = as.factor(suicide$Capital.Major.City)
suicide$Latitude = as.integer(suicide$Latitude)
suicide$Longitude = as.integer(suicide$Longitude)

str(suicide)



### Graphes par pays

#fonctions de graphs par pays

#le jeu de données fixe qui donnera la courbe totale de suicide pour chaque pays
total_df <- suicide %>% 
  group_by(year,country) %>% 
  filter(country == "Albania" ) %>%
  summarise(total_suicide = sum(suicides_no)) %>% 
  ggplot(aes(x=year,y=total_suicide)) +
  geom_point() 

#ensuite on veux ajouter sur le même graphique une SEULE courbe selon les modalitées de sélection des inputs :
#sex,age,generation et avec un changement dynamique de la seconde courbe selon les inputs sélectionnés

#courbe avec les valeurs par sex 

sex_df <- suicide %>% 
  group_by(year,country,sex) %>%
  filter(country == "Albania") %>% 
  summarise(total_suicide = sum(suicides_no))

total_df %>% ggplot(aes(x = year, y = total_suicide)) + 
  geom_line(data = total_df) + 
  # geom_line(data = sex_df) +
  theme_bw()

#courbe avec les valeurs par âge 

suicide %>% 
  group_by(year,country,age) %>% 
  filter(country == "Albania") %>%
  summarise(total_suicide = sum(suicides_no)) %>% 
  ggplot(aes(x=year,y=total_suicide)) +
  geom_point() +
  theme_bw()

# Ajouter les taux de suicide pour 100 000 habitants (pour pouvoir comparer des pays qui n'ont pas le mm nb d'habs)

suicide_country_cumul = suicide %>% group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
  summarise(total_suicide = sum(suicides_no),
            population = sum(population))

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


# travail pour les polygones 

world <-  read_sf("./data/world")
world$NAME[209] <- c("United States of America")
# Comparer les noms des pays à la main pour en récupérér le plus possible

### Merge des données
world <- world %>%
  filter(world$NAME %in% dta$Country)
dta <- dta %>% 
  filter(dta$Country %in% world$NAME)
dta_final <- merge(dta, world, by.x = "Country", by.y = "NAME")
dta_final2 <- st_sf(dta_final)















