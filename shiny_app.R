

library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "year",
        #"animation", "Looping Animation:",
                  label = "Year",
                  min = 2001,
                  max = 2017,
                  value = 2001,
                  sep = "",
                  step = 1,
             #     animate = TRUE,
        animate = animationOptions(interval = 1500, loop = FALSE, playButton = NULL))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "mainPlot")
      
    )
  )
)


server <- function(input, output) {
  
  output$mainPlot <- renderPlot({
    
    
    ggmap(mapgilbert) +
      geom_point(data = filter(rides_dif,year == input$year), aes(x = long, y = lat, fill = "red", alpha = 0.8, size = abs_dif), shape = 21) +
      scale_size_continuous(limits=c(1000,3000000)) +
      guides(fill=FALSE, alpha=FALSE, size = FALSE) +
      ggtitle(input$year) +
      theme(plot.title = element_text(size = 40, face = "bold"))
    
  },width = 600, height = 600)
  
}

shinyApp(ui = ui, server = server)


