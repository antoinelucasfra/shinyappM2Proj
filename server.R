server <- function(input, output, session) {
  ### Map panel

  # Reactive dataframe for leaflet output

  suicide_map <- reactive({
    req(input$idYear)
    suicide_year_cumul <- suicide %>%
      group_by(country, year, Capital.Major.City, Latitude, Longitude) %>%
      summarise(
        total_suicide = sum(suicides_no),
        population = sum(population)
      ) %>%
      filter(year %in% input$idYear) %>%
      mutate(ratio = total_suicide / population * 100000)

    # If polygons are available, merge and return a polygon sf
    if (exists("has_world", envir = .GlobalEnv) && has_world) {
      world_sub <- world %>% filter(NAME %in% suicide_year_cumul$country)
      suicide_year_cumul <- suicide_year_cumul %>%
        mutate(country = as.character(country)) %>%
        filter(country %in% world_sub$NAME)
      merge(suicide_year_cumul, world_sub, by.x = "country", by.y = "NAME") %>%
        sf::st_as_sf()
    } else {
      # Fall back to point markers using latitude/longitude (avg per country)
      pts <- suicide_year_cumul %>%
        dplyr::group_by(country) %>%
        dplyr::summarise(
          total_suicide = sum(total_suicide),
          population = sum(population),
          ratio = mean(ratio, na.rm = TRUE),
          Latitude = mean(Latitude, na.rm = TRUE),
          Longitude = mean(Longitude, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::filter(!is.na(Longitude) & !is.na(Latitude))

      sf::st_as_sf(pts, coords = c("Longitude", "Latitude"), crs = 4326)
    }
  })

  # Reactive text for leaflet output

  mytext <- reactive({
    paste(
      suicide_map()$country,
      " : ",
      round(suicide_map()$ratio, digits = 1),
      "suicides per 100k habs in ",
      input$idYear
    )
  })

  # Leaflet output

  map <- reactive({
    m <- leaflet(suicide_map()) %>%
      addTiles() %>%
      setView(lat = 46.2, lng = 2.2, zoom = 1.5)

    # Use polygons if we have polygons, else use circle markers
    if (exists("has_world", envir = .GlobalEnv) && has_world) {
      m <- m %>%
        addPolygons(
          fillColor = ~ colorNumeric(
            palette = "Reds",
            domain = ratio,
            na.color = "transparent"
          )(ratio),
          stroke = FALSE,
          fillOpacity = 1,
          label = mytext(),
          highlight = highlightOptions(
            weight = 5,
            color = "white",
            bringToFront = TRUE
          )
        ) %>%
        addLegend(
          pal = colorNumeric(palette = "Reds", domain = suicide_map()$ratio),
          values = ~ratio,
          opacity = 0.7,
          title = "Suicides per 100k inhabitants",
          position = "bottomright"
        )
    } else {
      pal <- colorNumeric(palette = "Reds", domain = suicide_map()$ratio)
      m <- m %>%
        addCircleMarkers(
          radius = ~ pmin(20, (ratio / max(ratio, na.rm = TRUE)) * 20),
          fillColor = ~ pal(ratio),
          color = "#777777",
          fillOpacity = 0.9,
          stroke = FALSE,
          label = mytext()
        ) %>%
        addLegend(
          pal = pal,
          values = ~ratio,
          opacity = 0.7,
          title = "Suicides per 100k inhabitants",
          position = "bottomright"
        )
    }

    m
  })

  output$mymap <- renderLeaflet({
    map()
  })

  user_map <- reactive({
    m <- map()
    if (!is.null(input$mymap_center) && !is.null(input$mymap_center$lng)) {
      m <- m %>%
        setView(
          lng = input$mymap_center$lng,
          lat = input$mymap_center$lat,
          zoom = input$mymap_zoom
        )
    }
    m
  })

  output$map_dl <- downloadHandler(
    filename = "leafletmap.pdf",
    content = function(file) {
      mapshot(
        x = user_map(),
        file = file,
        cliprect = "viewport", # The clipping rectangle matches the height & width from the viewing port
        selfcontained = FALSE
      ) # When this was not specified, the function for produced a PDF of two pages: one of the leaflet map, the other a blank page
    }
  )

  ### Plot panel

  # Global plot

  output$plot_global <- renderPlotly({
    req(input$country_select)
    p <- if (input$country_select == "Monde") {
      suicide %>%
        group_by(year) %>%
        summarise(
          suicide_100k = sum(suicides_no) / sum(population) * 100000
        ) %>%
        ggplot(aes(x = year, y = suicide_100k)) +
        geom_line(size = 2) +
        ggtitle("Evolution of the number of suicide per 100k habs") +
        labs(y = "Number of suicide per 100k habs") +
        theme_bw()
    } else {
      suicide %>%
        group_by(country, year) %>%
        filter(country == input$country_select) %>%
        summarise(
          suicide_100k = sum(suicides_no) / sum(population) * 100000
        ) %>%
        ggplot(aes(x = year, y = suicide_100k)) +
        geom_line(size = 2) +
        ggtitle("Evolution of the number of suicide per 100k habs") +
        labs(y = "Number of suicide per 100k habs") +
        theme_bw()
    }
    plotly::ggplotly(p)
  })

  # Sex plot

  output$plot_selected_sex <- renderPlotly({
    req(input$country_select)
    p <- if (input$country_select == "Monde") {
      suicide %>%
        group_by(year, sex) %>%
        summarise(
          suicide_100k = sum(suicides_no) / sum(population) * 100000
        ) %>%
        ggplot(aes(x = year, y = suicide_100k, color = sex)) +
        geom_line(size = 2) +
        ggtitle(
          "Evolution of the number of suicide per 100k habs between genders."
        ) +
        labs(y = "Number of suicide per 100k habs") +
        theme_bw()
    } else {
      suicide %>%
        group_by(country, year, sex) %>%
        filter(country == input$country_select) %>%
        summarise(
          suicide_100k = sum(suicides_no) / sum(population) * 100000
        ) %>%
        ggplot(aes(x = year, y = suicide_100k, color = sex)) +
        geom_line(size = 2) +
        ggtitle(
          "Evolution of the number of suicide per 100k habs between genders."
        ) +
        labs(y = "Number of suicide per 100k habs") +
        theme_bw()
    }
    plotly::ggplotly(p)
  })

  # Age plot

  output$plot_selected_age <- renderPlotly({
    req(input$country_select)
    p <- if (input$country_select == "Monde") {
      suicide %>%
        group_by(year, age) %>%
        summarise(
          suicide_100k = sum(suicides_no) / sum(population) * 100000
        ) %>%
        ggplot(aes(x = year, y = suicide_100k, color = age)) +
        geom_line(size = 2) +
        ggtitle(
          "Evolution of the number of suicide per 100k habs between age categories."
        ) +
        labs(y = "Number of suicide per 100k habs") +
        theme_bw()
    } else {
      suicide %>%
        group_by(country, year, age) %>%
        filter(country == input$country_select) %>%
        summarise(
          suicide_100k = sum(suicides_no) / sum(population) * 100000
        ) %>%
        ggplot(aes(x = year, y = suicide_100k, color = age)) +
        geom_line(size = 2) +
        ggtitle(
          "Evolution of the number of suicide per 100k habs between age categories."
        ) +
        labs(y = "Number of suicide per 100k habs") +
        theme_bw()
    }
    plotly::ggplotly(p)
  })

  # Generation plot

  output$plot_selected_generation <- renderPlotly({
    req(input$country_select)
    p <- if (input$country_select == "Monde") {
      suicide %>%
        group_by(year, generation) %>%
        summarise(
          suicide_100k = sum(suicides_no) / sum(population) * 100000
        ) %>%
        ggplot(aes(x = year, y = suicide_100k, color = generation)) +
        geom_line(size = 2) +
        ggtitle(
          "Evolution of the number of suicide per 100k habs between generations."
        ) +
        labs(y = "Number of suicide per 100k habs") +
        theme_bw()
    } else {
      suicide %>%
        group_by(country, year, generation) %>%
        filter(country == input$country_select) %>%
        summarise(
          suicide_100k = sum(suicides_no) / sum(population) * 100000
        ) %>%
        ggplot(aes(x = year, y = suicide_100k, color = generation)) +
        geom_line(size = 2) +
        ggtitle(
          "Evolution of the number of suicide per 100k habs between generations"
        ) +
        labs(y = "Number of suicide per 100k habs") +
        theme_bw()
    }
    plotly::ggplotly(p)
  })

  ### Country ranking panel

  # Reactive definition to select top countries

  # Lowest

  low <- reactive({
    if (input$indicator_select == "suicide rates per 100k habs") {
      suicide %>%
        group_by(year, country) %>%
        filter(
          year >= input$date_length_select[1] &
            year <= input$date_length_select[2]
        ) %>%
        summarise(
          total_suicide100k = sum(suicides_no) / sum(population) * 100000
        ) %>%
        ungroup() %>%
        group_by(country) %>%
        summarise(suicide_100k = mean(total_suicide100k)) %>%
        dplyr::arrange(suicide_100k) %>%
        slice(1:input$country_number_select)
    } else {
      suicide %>%
        group_by(year, country) %>%
        filter(
          year >= input$date_length_select[1] &
            year <= input$date_length_select[2]
        ) %>%
        summarise(total_suicide = sum(suicides_no)) %>%
        ungroup() %>%
        group_by(country) %>%
        summarise(suicide_total = mean(total_suicide)) %>%
        dplyr::arrange(suicide_total) %>%
        slice(1:input$country_number_select)
    }
  })

  # Highest

  high <- reactive({
    if (input$indicator_select == "suicide rates per 100k habs") {
      suicide %>%
        group_by(year, country) %>%
        filter(
          year >= input$date_length_select[1] &
            year <= input$date_length_select[2]
        ) %>%
        summarise(
          total_suicide100k = sum(suicides_no) / sum(population) * 100000
        ) %>%
        ungroup() %>%
        group_by(country) %>%
        summarise(suicide_100k = mean(total_suicide100k)) %>%
        dplyr::arrange(desc(suicide_100k)) %>%
        slice(1:input$country_number_select)
    } else {
      suicide %>%
        group_by(year, country) %>%
        filter(
          year >= input$date_length_select[1] &
            year <= input$date_length_select[2]
        ) %>%
        summarise(total_suicide = sum(suicides_no)) %>%
        ungroup() %>%
        group_by(country) %>%
        summarise(suicide_total = mean(total_suicide)) %>%
        dplyr::arrange(desc(suicide_total)) %>%
        slice(1:input$country_number_select)
    }
  })

  # Output definition for country rankings

  output$high_rank <- renderText({
    paste(
      "Top ",
      input$country_number_select,
      "country with the highest",
      input$indicator_select,
      "between",
      input$date_length_select[1],
      "and",
      input$date_length_select[2],
      "."
    )
  })

  output$table_high <- renderTable({
    high()
  })

  output$low_rank <- renderText({
    paste(
      "Top ",
      input$country_number_select,
      "country with the lowest",
      input$indicator_select,
      "between",
      input$date_length_select[1],
      "and",
      input$date_length_select[2],
      "."
    )
  })

  output$table_low <- renderTable({
    low()
  })

  # Add a download button

  output$download_data <- downloadHandler(
    filename = function() {
      paste("top", input$country_number_select, "country_ranking.csv")
    },

    content = function(file) {
      # Create the file to download

      low_add <- low() %>% mutate(rank_categ = rep("low", nrow(low())))
      high_add <- high() %>% mutate(rank_categ = rep("high", nrow(high())))
      all_add <- rbind(low_add, high_add)

      # Write the csv file

      write.csv(all_add, file, row.names = FALSE)
    }
  )

  ### Data panel

  output$dataTable_raw <- renderDataTable({
    suicide
  })

  ### About panel

  # Add the link for data informations

  link <- a(
    "Link for the raw data and brief explanation",
    href = "https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016"
  )

  output$link_data <- renderUI({
    tagList(link)
  })

  # Add the link for github repo for source code

  url <- a(
    "Github repo for the source code of the app",
    href = "https://github.com/antoinelucasfra/shinyappM2Proj"
  )

  output$git_repo <- renderUI({
    tagList(url)
  })
}
