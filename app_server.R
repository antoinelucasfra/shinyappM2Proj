

server <- function(input,output,session){
   
   #map panel
   
   #define points to print on the map
   points <- eventReactive(input$recalc, {
      cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
   }, ignoreNULL = FALSE)
   
   # reactive({ year <- input$idYear})
   # reactive({dispo_countries_vector <- as.vector(unique(suicide[suicide$year == year,]$country))})
   # reactive({countries_dispo_or_not <- total_countries_vector %in% dispo_countries_vector})
   # reactive({missing_countries_vector <- total_countries_vector[!countries_dispo_or_not]})
   # reactive({missing_countries <- as.data.frame(missing_countries_vector)})
   # reactive({colnames(missing_countries)[1] <- "country"})
   # reactive({missing_countries$Latitude <- total_countries$Latitude[!countries_dispo_or_not]})
   # reactive({missing_countries$Longitude <- total_countries$Longitude[!countries_dispo_or_not]})
   
   output$mymap <- renderLeaflet({
      leaflet() %>%
         addTiles() %>%
         # addMarkers(data = points()) 
         addCircleMarkers(
            lng = suicide_country_cumul$Longitude[suicide_country_cumul$year == input$idYear],
            lat =  suicide_country_cumul$Latitude[suicide_country_cumul$year == input$idYear],
            radius = suicide_country_cumul$total_suicide[suicide_country_cumul$year == input$idYear]/ suicide_country_cumul$population[suicide_country_cumul$year == input$idYear]*50000,
            color = "red",
            fillOpacity = 1,
            label = paste("Pays :", suicide_country_cumul$country[suicide_country_cumul$year == input$idYear],"; Nombre de suicide :", suicide_country_cumul$total_suicide[suicide_country_cumul$year == input$idYear]),
            labelOptions = labelOptions(textsize = "15px", direction = "auto")
         )
      #les cercles que l'on veux implémenter sur la carte 
      # %>%
      #   addCircles(
      #     lng = missing_countries$Longitude,
      #     lat = missing_countries$Latitude,
      #     radius = 50,
      #     opacity = 1,
      #     color = "grey",
      #     label = paste("Pas de données pour :", missing_countries$country)
      # )
   })
   
   # output$legend  ???
   
   #plot panel
   
   output$plot_selected <- renderPlot({
      
      # global_plot <- suicide %>% group_by(country,year,Capital.Major.City,Latitude,Longitude) %>% 
      #    # filter(country == input$country_select) %>% 
      #    summarise(total_suicide = sum(suicides_no), population_sum = sum(population),
      #              suicide_per100k_habs = total_suicide/population * 100000) %>% 
      #    ggplot() + geom_line(aes(x=year,y=total_suicide))
      
      # suicide %>% group_by(country,year,Capital.Major.City,Latitude,Longitude) %>% 
      #    filter(country == input$country_select) %>%
      #    filter(sex == input$sex_select) %>%
      #    summarise(total_suicide = sum(suicides_no), population_sum = sum(population)) %>% 
      #    ggplot() + geom_line(aes(x=year,y=total_suicide,color="red")) 
      
      suicide %>% group_by(country,year,Capital.Major.City,Latitude,Longitude) %>% 
         filter(country == input$country_select) %>%
         filter(sex == input$sex_select) %>% 
         filter(age == input$age_select) %>% 
         filter(generation == input$generation_select) %>% 
         summarise(total_suicide = sum(suicides_no), population_sum = sum(population)) %>% 
         ggplot(aes(x=year,y=total_suicide)) + geom_line() +
         theme_bw()
      
      # suicide %>% group_by(country,year,Capital.Major.City,Latitude,Longitude) %>% 
      #    filter(country == input$country_select) %>%
      #    summarise(total_suicide = sum(suicides_no), population_sum = sum(population)) %>% 
      #    ggplot(aes(x=year,y=total_suicide)) + geom_line() + theme_bw()
   })
   
   #data panel
   
   output$dataTable <- renderDataTable({
     suicide
     })
   
   
   #about panel
   
}
 
 
shinyApp(ui,server)



