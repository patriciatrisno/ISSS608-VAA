library(shiny)
library(highcharter)
library(leaflet)
library(DT)
library(dplyr)

# ==== Sample Data ====
set.seed(123)
sample_data <- data.frame(
  year = sample(2015:2023, 100, replace = TRUE),
  event = sample(c("Flood", "Earthquake", "Storm"), 100, replace = TRUE),
  region_level = sample(c("National", "District"), 100, replace = TRUE),
  region = sample(c("Region A", "Region B", "Region C"), 100, replace = TRUE),
  value = runif(100, 10, 100),
  lat = runif(100, 1.2, 1.5),
  lng = runif(100, 103.6, 104.0)
)

# ==== UI ====

ui <- fluidPage(
  # Header
  fluidRow(
    column(12,
           div(style = "background-color: #d9e3ec; padding: 15px; font-size: 24px; font-weight: bold;",
               "From Folk to Fame")
    )
  ),
  
  # Sidebar and content
  fluidRow(
    # Sidebar
    column(3,
           tags$div(style = "background-color: #f0f0f0; height: 100vh; padding: 10px;",
                    tags$h4("Navigation"),
                    tags$ul(style = "list-style-type: none; padding-left: 0;",
                            tags$li(tags$b("Sailor Shift’s Career Explorer")),
                            tags$li(style = "background-color: #c0d4e4;", "Oceanus Folk Influence Tracker"),
                            tags$li("Rising Star Prediction Dashboard")
                    ),
                    tags$hr(),
                    tabsetPanel(id = "tabs", type = "tabs",
                                tabPanel("Overview", ""),
                                tabPanel("Influence Timeline", ""),
                                tabPanel("Outward Impact", ""),
                                tabPanel("Roots", "")
                    )
           )
    ),
    
    # Main content
    column(9,
           fluidRow(
             column(3,
                    checkboxGroupInput("influence_type", "Influence Type", 
                                       choices = c("CoverOf", "DirectlySamples", "InStyleOf", "InterpolatesFrom", "LyricalReferenceTo"),
                                       selected = c("CoverOf", "DirectlySamples")),
                    sliderInput("year", "Year", min = 1990, max = 2030, value = c(2000, 2025)),
                    tags$div(style = "border: 1px solid #aaa; padding: 10px; margin-top: 10px;",
                             tags$b("Description"),
                             p("Descriptive Text")
                    )
             ),
             column(9,
                    plotOutput("line_plot", height = "400px")
             )
           )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    influence_data %>%
      filter(Year >= input$year[1], Year <= input$year[2],
             Type %in% input$influence_type)
  })
  
  output$line_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Year, y = Count, color = Type)) +
      geom_line(size = 1) +
      labs(title = "Oceanus Folk Influences Over Time", y = "Number of Songs") +
      theme_minimal()
  })
}

shinyApp(ui, server)
  
ui <- fluidPage(
  titlePanel("Oceanus Folk's Influence Over Time"),
  
  fluidRow(
    highchartOutput("lineChart1", height = "300px"),
    sliderInput("yearRange", "Select Year Range:", 
                min = 2015, max = 2023, value = c(2016, 2022), sep = ""),
    selectizeInput("eventSelect", "Select Event Type:", 
                   choices = unique(sample_data$event), 
                   multiple = TRUE, selected = "Flood"),
    radioButtons("regionLevel", "Select Region Level:", 
                 choices = c("National", "District"), selected = "National"),
    selectizeInput("regionSelect", "Select Region:", 
                   choices = unique(sample_data$region), multiple = TRUE),
    actionButton("filterBtn", "Update View"),
    checkboxInput("showLine2", "Show Secondary Line Chart", value = TRUE),
    conditionalPanel(
      condition = "input.showLine2 == true",
      highchartOutput("lineChart2", height = "300px")
    ),
    checkboxInput("showMap", "Show Map", value = TRUE),
    conditionalPanel(
      condition = "input.showMap == true",
      leafletOutput("map", height = "400px")
    )
  ),
  
  fluidRow(
    DT::dataTableOutput("dataTable")
  )
)

# ==== Server ====
server <- function(input, output, session) {
  
  filteredData <- eventReactive(input$filterBtn, {
    sample_data %>%
      filter(year >= input$yearRange[1], year <= input$yearRange[2],
             event %in% input$eventSelect,
             region_level == input$regionLevel,
             region %in% input$regionSelect)
  }, ignoreNULL = FALSE)
  
  output$lineChart1 <- renderHighchart({
    df <- filteredData() %>%
      group_by(year) %>%
      summarise(avg_value = mean(value))
    
    hchart(df, "line", hcaes(x = year, y = avg_value)) %>%
      hc_title(text = "Avg Value Over Time")
  })
  
  output$lineChart2 <- renderHighchart({
    df <- filteredData() %>%
      group_by(event) %>%
      summarise(avg_value = mean(value))
    
    hchart(df, "column", hcaes(x = event, y = avg_value)) %>%
      hc_title(text = "Avg Value by Event Type")
  })
  
  output$map <- renderLeaflet({
    leaflet(data = filteredData()) %>%
      addTiles() %>%
      addCircleMarkers(~lng, ~lat, label = ~paste(event, year),
                       color = "blue", radius = 4)
  })
  
  output$dataTable <- DT::renderDataTable({
    filteredData()
  })
}

# ==== Run App ====
shinyApp(ui = ui, server = server)
