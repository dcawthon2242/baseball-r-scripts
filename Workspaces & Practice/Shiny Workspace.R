# Shiny Workspace
install.packages("shiny")
library(shiny)
install.packages("bslib")
library(bslib)

# Define the UI for app that draws a histogram
ui <- page_sidebar(
  # App title
  title = "Hello Shiny!",
  # Sidebar panel for inputs
  sidebar = sidebar(
    # Input: slider for number of bins
    sliderInput(
      inputId = "bins",
      label = "Number of bins:",
      min = 1,
      max = 50,
      value = 30
    )
  ),
  # Output: Histogram
  plotOutput(outputId = "distPlot")
)

server <- function(input, output) {
  # Histogram of Old Faithful Geyser Data with requested # of bins
  # This expression that generates a histogram is wrapped in a call to renderPlot
  # 1. Indicates it is "reactive" (Should change when inputs change)
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    x <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#007bc2", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
  })
}

shinyApp(ui = ui, server = server)

run



