library(shiny)
library(ggplot2)
library(dplyr)

# Sample dummy data
set.seed(42)
influence_data <- data.frame(
  Year = rep(1990:2030, 5),
  Count = sample(0:30, 205, replace = TRUE),
  Type = rep(c("CoverOf", "DirectlySamples", "InStyleOf", "InterpolatesFrom", "LyricalReferenceTo"), each = 41)
)

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
