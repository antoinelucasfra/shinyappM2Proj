
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
