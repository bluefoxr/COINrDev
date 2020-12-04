# shiny test 2

library(shiny)
library(ggplot2)


data(mpg)
data <- as.data.frame(matrix(data = runif(1000), nrow = 100, ncol = 10))

ui <- fluidPage(
  inputPanel(
    selectInput('x', 'Indicator', choices = colnames(data),
                selected = "class")#,
    #selectInput('y', 'Y', choices = c( "trans", "fl", "drv"), 
    #            selected = "drv")
  ),
  
  mainPanel(plotOutput("outplot"))
  
)

server <- function(input, output) {
  
  output$outplot <- renderPlot({
    ggplot(data, aes_string(x = input$x)) +
      geom_histogram(colour = "#e9ecef", bins = 10) + theme_light()
  })
  
}

shinyApp(ui = ui, server = server)