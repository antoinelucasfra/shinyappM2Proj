server <- function(input,output,session)
{
   
   ### Map panel
   
   # Define points to print on the map
   points <- eventReactive(input$recalc, {
      cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
   }, ignoreNULL = FALSE)
   
   output$mymap <- renderLeaflet({
      leaflet() %>%
         addTiles() %>%
         
         # addMarkers(data = points()) 
         addCircleMarkers(
            lng = suicide_country_cumul$Longitude[suicide_country_cumul$year == input$idYear],
            lat =  suicide_country_cumul$Latitude[suicide_country_cumul$year == input$idYear],
            radius = suicide_country_cumul$total_suicide[suicide_country_cumul$year == input$idYear]/500,
            color = "red",
            fillOpacity = 1,
            label = paste("Pays :", suicide_country_cumul$country[suicide_country_cumul$year == input$idYear],"; Nombre de suicide :", suicide_country_cumul$total_suicide[suicide_country_cumul$year == input$idYear]),
            labelOptions = labelOptions(textsize = "15px", direction = "auto")
         ) 
   })
   
   ### Plot panel
   
   output$plot_global <- renderPlotly({
      if (input$country_select == "Monde"){
         suicide %>% group_by(year) %>% 
            summarise(suicide_100k = sum(suicides.100k.pop)) %>% 
            ggplot(aes(x=year,y=suicide_100k)) + geom_line(size = 1.5) +
            ggtitle("Evolution of the number of suicide per 100k habs in total") +
            scale_color_viridis_d() +
            labs(y = "Number of suicide per 100k habs") +
            theme_bw()
      }
      else { 
         suicide %>% group_by(country, year) %>% 
            filter(country == input$country_select) %>%
            summarise(suicide_100k = sum(suicides.100k.pop)) %>% 
            ggplot(aes(x=year,y=suicide_100k)) + geom_line(size = 1.5) +
            ggtitle("Evolution of the number of suicide per 100k habs in total") +
            scale_color_viridis_d() +
            labs(y = "Number of suicide per 100k habs") +
            theme_bw()
         }
      })
   
   observeEvent(input$sex, {output$plot_selected_sex <- renderPlotly({
      
      if (input$country_select == "Monde"){
         suicide %>% group_by(year,sex) %>% 
            summarise(suicide_100k = sum(suicides.100k.pop)) %>% 
            ggplot(aes(x=year,y=suicide_100k, color = sex)) + geom_line(size = 1.5) +
            ggtitle("Evolution of the number of suicide per 100k habs between genders.") +
            scale_color_viridis_d() +
            labs(y = "Number of suicide per 100k habs") +
            theme_bw()
      }
      else {
         suicide %>% group_by(country,year,sex) %>% 
            filter(country == input$country_select) %>%
            summarise(suicide_100k = sum(suicides.100k.pop)) %>% 
            ggplot(aes(x=year,y=suicide_100k, color = sex)) + geom_line(size = 1.5) +
            ggtitle("Evolution of the number of suicide per 100k habs between genders.") +
            scale_color_viridis_d() +
            labs(y = "Number of suicide per 100k habs") +
            theme_bw()
      }
   })})
   
   
   observeEvent(input$age, {output$plot_selected_age <- renderPlotly({
      
      if (input$country_select == "Monde"){
         suicide %>% group_by(year,age) %>% 
            summarise(suicide_100k = sum(suicides.100k.pop)) %>% 
            ggplot(aes(x=year,y=suicide_100k, color = age)) + geom_line(size = 1.5) +
            ggtitle("Evolution of the number of suicide per 100k habs between age categories.") +
            scale_color_viridis_d() +
            labs(y = "Number of suicide per 100k habs") +
            theme_bw()
      }
      
      else {
         suicide %>% group_by(country,year,age) %>% 
            filter(country == input$country_select) %>%
            summarise(suicide_100k = sum(suicides.100k.pop)) %>% 
            ggplot(aes(x=year,y=suicide_100k, color = age)) + geom_line(size = 1.5) +
            ggtitle("Evolution of the number of suicide per 100k habs between age categories.") +
            scale_color_viridis_d() +
            labs(y = "Number of suicide per 100k habs") +
            theme_bw()
      }
      
   })})
   
   observeEvent(input$generation, {output$plot_selected_generation <- renderPlotly({
      
      if (input$country_select == "Monde"){
         suicide %>% group_by(year,generation) %>% 
            summarise(suicide_100k = sum(suicides.100k.pop)) %>% 
            ggplot(aes(x=year,y=suicide_100k, color = generation)) + geom_line(size = 1.5) +
            ggtitle("Evolution of the number of suicide per 100k habs between generations.") +
            scale_color_viridis_d() +
            labs(y = "Number of suicide per 100k habs") +
            theme_bw()
      }
      
      else {
         suicide %>% group_by(country,year,generation) %>% 
            filter(country == input$country_select) %>%
            summarise(suicide_100k = sum(suicides.100k.pop)) %>% 
            ggplot(aes(x=year,y=suicide_100k, color = generation)) + geom_line(size = 1.5) +
            ggtitle("Evolution of the number of suicide per 100k habs between generations") +
            scale_color_viridis_d() +
            labs(y = "Number of suicide per 100k habs") +
            theme_bw()
      }
      
   })}) 
   
   ### Country ranking panel
   
   #reactive definition
   
   low <- reactive({
      suicide %>% group_by(year,country) %>% 
         filter(year >= input$date_length_select[1] & year <= input$date_length_select[2]) %>%
         summarise(total_suicide100k = sum(suicides.100k.pop)) %>% ungroup() %>% 
         group_by(country) %>% 
         summarise(mean_year = mean(total_suicide100k)) %>% 
         dplyr::arrange(mean_year) %>%
         slice(1:input$country_number_select)
   })
   
   high <- reactive({
      suicide %>% group_by(year,country) %>% 
         filter(year >= input$date_length_select[1] & year <= input$date_length_select[2]) %>%
         summarise(total_suicide100k = sum(suicides.100k.pop)) %>% ungroup() %>% 
         group_by(country) %>% 
         summarise(mean_year = mean(total_suicide100k)) %>% 
         dplyr::arrange(desc(mean_year)) %>%
         slice(1:input$country_number_select)
   })
   
   #output definition
   
   output$high_rank<- renderText({
      paste("This is the top ",
            input$country_number_select,
            "country with the highest suicide rates (per 100k habs) between",
            input$date_length_select[1],"and",input$date_length_select[2])
   })
   
   output$table_high <- renderTable({
      high()
   })
      
    output$low_rank <- renderText({
       paste("This is the top ",
             input$country_number_select,
             "country with the lowest suicide rates (per 100k habs) between",
             input$date_length_select[1],"and",input$date_length_select[2])
    })
    
    output$table_low <- renderTable({
       low()
    })
    
    output$download_data <- downloadHandler(
       filename = function(){
          paste("top",input$country_number_select,"country_ranking.csv")
          },
       content = function(file){
          #create the file to download
          low_add <- low() %>% mutate(rank_categ = rep("low",nrow(low())))
          high_add <- high() %>% mutate(rank_categ = rep("high",nrow(high())))
          all_add <- rbind(low_add,high_add)
          #write the csv file
          write.csv(all_add, file, row.names = FALSE)
       }
    )
    
    ### Prediction panel
    
    ### Data panel
    
    output$dataTable_raw <- renderDataTable({
       suicide
    })
    
    ### About panel
    
    link <- a("Link for the raw data and brief explanation",
              href="https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016")
    output$link_data <- renderUI({
       tagList(link)
    })
    
    url <- a("Github repo for the source code of the app",
             href="https://github.com/antoinelucasfra/shinyappM2Proj")
    output$git_repo <- renderUI({
       tagList(url)
    })
}

shinyApp(ui,server)

