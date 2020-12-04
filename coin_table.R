#' Generate table to visualise results
#'
#' Uses the reactable package to build interactive tables
#'
#' @param country The country/unit to build the doc for.
#' 
#' @examples 
#'
#' @return Tables
#'
#' @export

coin_table <- function(COINobj){
  
  # get data and reverse so that index is first
  tabledata <- rev(COINobj$data$data_aggregated)
  
  sticky_style <- list(position = "sticky", left = 0, background = "#fff", zIndex = 1,
                       borderRight = "1px solid #eee")
  
  
  # to do: make a function which loops over columns of the data frame
  # for each col, it adds to a list using the coldef function.
  # hopefully should be able to subst whole list into the reactable function.
  # basically what I did here below, but then with looping over cols.
  # lapply probably a good bet.
  
  coldefs <- list(Index = colDef(
    style = function(value) {
      normalized <- (value - min(tabledata$Index)) / (max(tabledata$Index) - min(tabledata$Index))
      color <- orange_pal(normalized)
      list(background = color)
    }
  ))
  
  reactable(tabledata, 
            defaultSorted = "Index", defaultSortOrder = "desc", 
            #groupBy = "Group_GDP",
            
            columns = coldefs
              
              # ),
              # Code_country = colDef(
              #   style = sticky_style,
              #   headerStyle = sticky_style
              # )
            
            
  )
  
}

orange_pal <- function(x) rgb(colorRamp(c("#ffe4cc", "#ffb54d"))(x), maxColorValue = 255)

#}