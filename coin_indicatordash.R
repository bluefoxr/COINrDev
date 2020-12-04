#' Indicator visualisation dashboard
#'
#' Generates an interactive visualisation of one or two indicators at a time. Requires Shiny and an active R session.
#' 
#' @param COINobj The COIN object, or a data frame of indicator data.
#' @param inames A set of indicator codes to include. Defaults to all indicators.
#' @param dset The data set to plot.
#' 
#' @examples coin_indicatordash(COINobj, inames = NULL, dset = "raw")
#'
#' @return Interactive visualisation
#' 
#' @export
#' 

coin_indicatordash <- function(COINobj, inames = NULL, dset = "raw"){
  
  library(shiny)
  
  # first, get the indicator data from input object
  out <- coin_aux_objcheck(COINobj, dset, inames)
  ind_data_only <- out$ind_data_only
  # this is used to label scatter plot. Will need to be generalised.
  code_yr <- paste0(out$ind_data$Code_country,out$ind_data$Year_ref)
  
  ###--------- Define the UI ---------------
  
  ui <- fluidPage(    
    fluidRow(
      column(5, plotlyOutput("histo") ),
      column(5, offset = 0, plotlyOutput("violin") )
    ),
    
    hr(),
    
    fluidRow(
      column(5,
             textOutput("sk"),
             #hr(),
             textOutput("k"),
             hr(),
             selectInput("vr2", "Indicator 1", choices=colnames(ind_data_only)),
             selectInput("vr3", "Indicator 2", choices=colnames(ind_data_only)) ),
      column(7,
             plotlyOutput("scatter")
      )
    )
  )
  
  ###------ Define the server code -----------
  
  server <- function(input, output) {
    
    # Violin plot
    output$violin <- renderPlotly({
      
      fig <- plot_ly(data = ind_data_only, y = ~get(input$vr2), type = 'violin',
          box = list(visible = T),
          meanline = list(visible = T),
          x0 = input$vr2,
          points = 'all',
          pointpos = -1.5,
          jitter = 0.1,
          hoveron = "violins+points+kde"
        ) %>%
        
        layout( yaxis = list(title = "", zeroline = F) )
      
      fig
      
      #ggplot(ind_data_only, aes_string(x = input$vr2)) + 
        #geom_dotplot(binaxis = "x", stackdir = "center", dotsize=1, stackratio=0.5, alpha = 0.3) + theme_light() 
    })
    
    # Histogram
    output$histo <- renderPlotly({
      
      plot_ly(data = ind_data_only, x = ~get(input$vr2), type = "histogram") %>% 
        layout(bargap=0.1, xaxis = list(title = input$vr2))
      
      #ggplotly(
        #ggplot(ind_data_only, aes_string(x = input$vr2)) +
          #geom_histogram(colour = "#e9ecef", bins = 10) + theme_light()
      #)
    })
    
    # scatter plot
    output$scatter <- renderPlotly({
      
      sc <- plot_ly(data = ind_data_only, type = 'scatter', mode = 'markers') %>%
        add_trace(
          x = ~get(input$vr2), 
          y = ~get(input$vr3),
          text = code_yr,
          hoverinfo = 'text',
          marker = list(size = 15),
          showlegend = F
        ) %>% 
        layout(xaxis = list(title = input$vr2),
               yaxis = list(title = input$vr3))
      
      sc
      
    })
    
    output$sk <- renderText({
      paste0("Skew = ",
             moments::skewness(ind_data_only[[input$vr2]], na.rm = T) %>%
               round(3))
    })
    output$k <- renderText({ 
      paste0("Kurtosis = ",
             moments::kurtosis(ind_data_only[[input$vr2]], na.rm = T) %>%
               round(3))
    })
    
  }
  
  # Return a Shiny app object
  shinyApp(ui = ui, server = server, options = list(height = 500))
  
}