library (shiny)
library (tidyverse)

exam <- read_csv("Exam_data.csv")
print(exam)

ui <- fluidPage(
  titlePanel("Pupils Exam Results dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "variable",
                  label = "subject:",
                  choices = c("English" = "ENGLISH",
                              "Maths" = "MATHS",
                              "Science" = "SCIENCE"),
                  selected = "ENGLISH"),
      sliderInput(inputId = "bins",
                  label = "Number of Bins",
                  min = 5,
                  max = 20,
                  value = 10)
      ),
    
    mainPanel(
      plotOutput("distPlot")
    )
  )
)


server <- function(input, output) {
  output$distPlot <- renderPlot({
    ggplot(data = exam,
           aes_string(x = input$variable)) +
      geom_histogram(bins = input$bins,
                     color = "black",
                     fill = "light blue")})}
shinyApp (ui=ui, server=server)

