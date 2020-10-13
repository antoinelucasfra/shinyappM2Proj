dta <- read.csv("empreinte_carbone.csv", header = TRUE, sep = ";", fileEncoding = "latin1")
dim(dta)
summary(dta)

### Importation des borders

world <-  read_sf("data/world")
world$NAME[209] <- c("United States of America")


### Merge des données
world <- world %>%
  filter(world$NAME %in% dta$Country)

dta <- dta %>% 
  filter(dta$Country %in% world$NAME)

dta_final <- merge(dta, world, by.x = "Country", by.y = "NAME")
dta_final2 <- st_sf(dta_final)


### Palettes de couleurs
# Faire des palettes par variables
mypalette <- colorNumeric(palette="Accent", domain = dta$Total.Ecological.Footprint, na.color="transparent")
mypalette(c(45,43))

### Les Maps

mytext <- paste(dta$Country, dta$Total.Ecological.Footprint) 

map <- leaflet(dta_final2) %>%
  addTiles() %>%
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons(fillColor = ~mypalette(Total.Ecological.Footprint), 
              stroke=FALSE, 
              fillOpacity = 1,
              label = mytext, 
              highlight = highlightOptions(weight = 5, color = "white",
                                           bringToFront = TRUE)) %>% 
  addLegend(pal = mypalette, values = ~Total.Ecological.Footprint, opacity = 0.7, title = "Total.Ecological.Footprint",
            position = "bottomright") # Regler la taille de la légende
map
