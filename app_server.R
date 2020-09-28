 server <- function(input,output){
   
   #map panel
   
   output$mymap <- renderLeaflet({
     leaflet(quakes) %>% addTiles()
   })
   
   # output$legend  ???
   
   #plot panel
   
   output$country_select 
   
   output$plot_selected <- renderPlot({
     
     iris %>% 
       ggplot(aes(x= Petal.Length,y = Sepal.Length, color = Species)) + geom_line()
     
     
   })
     
   #data panel
   
   output$dataTable <- renderDataTable({
     iris
     })
   
   
   #about panel
   
}
 
 
shinyApp(ui,server)



