# This is the script for the ui part of the app

ui <- fluidPage(
  
  # Define a theme for the app
  
  theme = shinytheme("united"),
  
  # Creation of the different panels 
  
  navbarPage("R_Shiny_Suicide",
             
             # Map panel
             
             tabPanel("Suicide mapper",
                      
                      sidebarLayout(
                        sidebarPanel(
                          h4("Select the year of your interest."),
                          hr(),
                          pickerInput("idYear", "Year:",
                                      choices = seq(1990,2014,1),width = '60%'),
                          hr(),
                          "Press below to download a pdf output of the map:",
                          downloadButton("map_dl"),
                          
                        ),
                        
                        mainPanel(
                          
                          h3("Worldwide map for suicide data"),
                          leafletOutput("mymap")

                        )
                        
                      )
             ),
             
             # Region plot panel
             
             tabPanel("Region plots",
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          h4("Select the country of your interest to have 
                           a clue on suicide evolution through time between different factors."),
                          hr(),
                          pickerInput("country_select", "Country:",
                                      choices = c("Monde",levels(suicide$country)),
                                      multiple = FALSE),
                          
                        ),
                        
                        mainPanel(
                          
                          tabsetPanel(id = "main panel",
                                      
                                      tabPanel("Global Vision", 
                                               plotlyOutput("plot_global")),
                                      
                                      tabPanel("By sex",
                                               plotlyOutput("plot_selected_sex")),
                                      
                                      tabPanel("By age",
                                               plotlyOutput("plot_selected_age")),
                                      
                                      tabPanel("By generation",
                                               plotlyOutput("plot_selected_generation"))
                          )
                        )
                      )
             ),
             
             # Country ranking panel
             
             tabPanel("Country Ranking",
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          h4("Select the options to have an idea on the suicide country rankings."),
                          hr(),
                          numericInput("country_number_select",
                                       "Select the number of country you want in the top:",
                                       value = 5, min = 1, max = length(unique(suicide$country)),
                                       step = NA),
                          
                          sliderInput("date_length_select",
                                      "Select the interval of time:",
                                      min = 1990, max = 2014, value = c(1990,2014)),
                          
                          radioButtons("indicator_select",
                                       "Indicator to rank countries :",
                                       c("suicide rates per 100k habs","average number of suicide")),
                          hr(),
                          "Press below to download selected data:",
                          downloadButton(outputId = "download_data", 
                                         label = "Download Selected Data")
                          
                        ),
                        
                        mainPanel(
                          
                          textOutput("high_rank"),
                          
                          tableOutput("table_high"),
                          
                          hr(""),
                          
                          textOutput("low_rank"),
                          
                          tableOutput("table_low")
                        )
                      )
             ),
             
             # Raw data panel
             
             tabPanel("Raw data",
                      
                      h3("Raw data exploration, feel free to explore it!"),
                      
                      dataTableOutput("dataTable_raw")
             ),
             
             # About panel
             
             tabPanel("About",
                      
                      h3("Analysis and critic"),
                      
                      h5("Our dataset is not ideal:"),
                      
                      h5("- Firstly, there is a lot of missing data for several countries."),
                      
                      h5("- Moreover, it would be useful to have more indicators about each country, about economy, social or health system for instance
                      in order to make a prediction model for the evolution of the number of suicides in the world."),
                      
                      h3("Explanations about the application"),
                      
                      h5("This application was built by Zoe Wante, Antoine Lucas and Chloe Tellier for a project during a course of Data Science specialisation in Agrocampus Ouest (Rennes, France)
                         under the tutorship of Benoit Thieurmel and Francois Husson."),
                      
                      h5("The data are available at the following link:"),
                      
                      uiOutput("link_data"),
                      
                      h5("If you want to consult the source code, please refer to the following link:"),
                      
                      uiOutput("git_repo")
             )
  )
)
