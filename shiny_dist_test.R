# a little test to try to visualise distributions using Shiny

library(plotly)

# Global variables can go here
data <- as.data.frame(matrix(data = runif(1000), nrow = 100, ncol = 10))


# Define the UI
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("Test"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("vr", "Indicator", 
                  choices=colnames(data)),
      hr(),
      helpText("Whoopty doo")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      
      fluidRow(
        column(5,
               plotlyOutput("densplot2")
        ),
        column(5, offset = 1,
               plotOutput("densplot1")
        )
      )
    )
    
  )
)

# Define the server code
server <- function(input, output) {
  
  # Histogram
  output$densplot1 <- renderPlot({
  

    ggplot(data, aes_string(x = input$vr)) + 
      geom_dotplot(binaxis = "x", stackdir = "center", dotsize=1, stackratio=0.5, alpha = 0.3) + theme_light() 

    
  })
  
  # Histogram
  output$densplot2 <- renderPlotly({
    ggplotly(
    ggplot(data, aes_string(x = input$vr)) +
      geom_histogram(colour = "#e9ecef", bins = 10) + theme_light()
    )
  })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)