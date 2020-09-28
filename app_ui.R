#this is the script for the ui part of the app

ui <- shinyUI(
  navbarPage("Suicide map",
             
             tabPanel("Suicide mapper",
                      sidebarPanel(
                        selectInput(inputId = "idYear", label = "Year Selection", choices = seq(1986,2016,1))
                        
                      ),
                      
                      leafletOutput("mymap"),
                      
                      #add a legend for suicide amount :
                      
                      
                      
             ),
             
             tabPanel("Region plots",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          pickerInput("country_select", "Country:",
                                      choices = "albania",
                                      multiple = TRUE), 
                          
                          pickerInput("variable_select",
                                      "Variable:",
                                      choices = c("Sex","Age","Generation"),
                                      selected = "Sex"
                          ),
                          
                          "Select country, year, category and age class"
                          
                        ),
                        mainPanel(
                          plotlyOutput("plot_selected")
                          
                        )
                      )
                      
             ),
             
             tabPanel("Data",
                      dataTableOutput("dataTable")
             ),
             
             tabPanel("About this app",
                      
                      h2("The author are : Zoe Wante, Chloé Tellier, Antoine Lucas"),
                      h3("This app was built in the context of a R programmation course")
                      )
  )
)










