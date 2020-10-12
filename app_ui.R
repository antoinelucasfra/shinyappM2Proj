# This is the script for the ui part of the app

ui <- shinyUI(
  navbarPage("Shiny_SuicideR",
             
             tabPanel("Suicide mapper",
                      selectInput(inputId = "idYear", label = "Annees", 
                                  choices = seq(1986,2016,1)),
                      leafletOutput("mymap")
             ),
             
             tabPanel("Region plots",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          pickerInput("country_select", "Country:",
                                      choices = levels(suicide$country),
                                      multiple = FALSE),
                          
                          "Select the country of your interest to have a clue on suicide evolution through time between different classes"
                        ),
                        
                        mainPanel(
                          plotlyOutput("plot_selected_sex"),
                          plotlyOutput("plot_selected_age"),
                          plotlyOutput("plot_selected_generation")
                        )
                      )
             ),
             
             tabPanel("Country Ranking",
                      
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("country_number_select",
                                      "Select the number of country you want in the top:",
                                      min = 1, max = length(levels(suicide$country)),value = 5),
                          
                          sliderInput("date_length_select",
                                      "Select the interval of time:",
                                      min = 1985, max = 2016, value = c(1985,2016))
                        ),
                        
                        mainPanel(
                          textOutput("high_rank"),
                          tableOutput("table_high"),
                          textOutput("low_rank"),
                          tableOutput("table_low"),
                          downloadButton(outputId = "download_data", 
                                         label = "Download Selected Data")
                        )
                      )
             ),
             
             tabPanel("Prediction",
                      h3("This is a tool for approximative prediction of the amount of suicide in the next year."),
                      tableOutput("predict_table"),
                      plotOutput("predict_plot")
                      
             ),
             
             tabPanel("Raw data",
                      h2("Raw data exploration"),
                      dataTableOutput("dataTable_raw")
             ),
             
             tabPanel("About",
                      h4("This application was built for a project during a course of Data Science specialisation in Agrocampus Ouest"),
                      h4("The data are available at the following link:"),
                      uiOutput("link_data"),
                      h4("If you want to consult the source code, please refer to the following link :"),
                      uiOutput("git_repo")
             )
             
  )
)
