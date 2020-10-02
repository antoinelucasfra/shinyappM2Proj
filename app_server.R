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
            radius = suicide_country_cumul$total_suicide[suicide_country_cumul$year == input$idYear]/500,
            # radius = suicide_country_cumul$total_suicide[suicide_country_cumul$year == input$idYear]/ suicide_country_cumul$population[suicide_country_cumul$year == input$idYear]*50000,
            color = "red",
            fillOpacity = 1,
            label = paste("Pays :", suicide_country_cumul$country[suicide_country_cumul$year == input$idYear],"; Nombre de suicide :", suicide_country_cumul$total_suicide[suicide_country_cumul$year == input$idYear]),
            labelOptions = labelOptions(textsize = "15px", direction = "auto")
         ) 
         # addLegend(position = "bottomright",pal = colorNumeric(),values = ,
         #           title = "Number of Deaths per country")
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
   
   output$plot_selected_sex <- renderPlotly({
      
      suicide %>% group_by(country,year,sex) %>% 
         filter(country == input$country_select) %>%
         summarise(suicide_100k = sum(suicides.100k.pop)) %>% 
         ggplot(aes(x=year,y=suicide_100k, color = sex)) + geom_line() +
         ggtitle("Evolution of the number of suicide per 100k habs between genders.") +
         scale_color_viridis_d() +
         labs(y = "Number of suicide per 100k habs") +
         theme_bw()
         
      })
   
   output$plot_selected_age <- renderPlotly({
      
      suicide %>% group_by(country,year,age) %>% 
         filter(country == input$country_select) %>%
         summarise(suicide_100k = sum(suicides.100k.pop)) %>% 
         ggplot(aes(x=year,y=suicide_100k, color = age)) + geom_line() +
         ggtitle("Evolution of the number of suicide per 100k habs between age categories.") +
         scale_color_viridis_d() +
         labs(y = "Number of suicide per 100k habs") +
         theme_bw()
      
   })
   
   output$plot_selected_generation <- renderPlotly({
      
      suicide %>% group_by(country,year,generation) %>% 
         filter(country == input$country_select) %>%
         summarise(suicide_100k = sum(suicides.100k.pop)) %>% 
         ggplot(aes(x=year,y=suicide_100k, color = generation)) + geom_line() +
         ggtitle("Evolution of the number of suicide per 100k habs between generations") +
         scale_color_viridis_d() +
         labs(y = "Number of suicide per 100k habs") +
         theme_bw()
      
   })
   
   #country ranking panel
   
   output$table_increase <- renderTable({
      
      suicide
      # suicide %>% group_by(year,country) %>%
      #    filter(year >= input$date_length_select$min & year <= input$date_length_select$max) %>%
      #    summarise(total_suicide100k = sum(suicides.100k.pop)) %>%
      #    dplyr::arrange(desc(total_suicide100k)) %>%
      #    ungroup() %>%
      #    slice(1:input$country_number_select)
      
   })
   
   output$table_decrease <- renderTable({
      
      suicide %>% group_by(year,country) %>% 
         # REUSSIR A PRENDRE MIN ET MAX SLIDER POUR 
         filter(year >= input$date_length_select & year <= input$date_length_select) %>%
         summarise(total_suicide100k = sum(suicides.100k.pop)) %>%
         dplyr::arrange(total_suicide100k) %>% 
         ungroup() %>%
         slice(1:input$country_number_select)
   })
   
   
   #data panel
   
   output$dataTable_raw <- renderDataTable({
     suicide
     })
   
   
   #about panel
   
   #add the url in the text
   url <- a("Github repo for the source code of the app",
            href="https://github.com/antoinelucasfra/shinyappM2Proj")
   output$tab <- renderUI({
      tagList(url)
   })
   
}
 
 
shinyApp(ui,server)



