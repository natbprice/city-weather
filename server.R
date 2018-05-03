# Define server logic -----------------------------------------------------
function(input, output, session) {

  # Set default ggplot theme
  myTheme <- theme_bw(base_size = 20) +
    theme(
      panel.grid.major.y = element_line(color = "grey92"),
      axis.line = element_line(colour = "black"),
      legend.title = element_blank()
    )
  theme_set(myTheme)
  
  # Load data
  weatherData <- reactive({
    read_feather("data/weatherData.feather")
  })
  
  # Output select city UI
  output$citySelect <- renderUI({
    req(weatherData())
    selectizeInput(
      inputId = "cityState",
      label = h4("Select cities to compare:"),
      choices = weatherData()$cityState,
      selected = c("LINCOLN, NE", "GAINESVILLE, FL"),
      multiple = TRUE
    )
  })
  
  pageWidth <- reactive({
    input$dimension[1]
  })
  
  # Plots -----------------------------------------------------------------
  output$myPlots <- renderUI({
    req(weatherData(), input$cityState, pageWidth())

    weatherData() %>%
      split(.$variable) %>%
      map(~ myPlot(weatherData = ., input$cityState)) %>%
      map(~ ggplotly(p = ., width = pageWidth()*.9, tooltip = "text") %>%
            layout(hovermode = "compare")) %>%
      map(~ renderPlotly(expr = .)) %>%
      map(tagList)
  })

  # Function to create line plots -------------------------------------------
  myPlot <- function(weatherData, cities) {
    
    # Filter data
    plotData <-
      weatherData %>%
      filter(cityState %in% cities)
    
    # y-label lookup table
    yLabels <- c(
      "nrmmin" = "temperature, °F",
      "nrmmax" = "temperature, °F",
      "nrmavg" = "temperature, °F",
      "nrmcdd" = "cooling degree days",
      "nrmhdd" = "heating degree days",
      "nrmpcp" = "percipitation, in."
    )
    
    # Match y-label to variable
    ylabel <- yLabels[match(plotData$variable[1], names(yLabels))]
    
    # Create plot
    ggplot(data = plotData,
           aes(
             x = month,
             y = value,
             color = cityState,
             group = cityState,
             text = value
           )) +
      geom_line(size = 1.1) +
      geom_point(size = 2) +
      labs(title = plotData$variableName[1],
           y = ylabel)
  }
}

