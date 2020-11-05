#' Plotting indicators in many ways
#'
#' Plots indicators
#' 
#' @param datain A data frame (tibble) of indicator data
#' @param plotinds A character vector of indicator names (column names of datain) to plot. Defaults to all indicators.
#' @param type The type of plot
#' @param facetplot If TRUE, plots each indicator
#'
#' @examples coin_indicatorplot(COINobj, type = "Box")
#'
#' @return Nice plots
#'
#' @export
#' 

coin_indicatorplot <- function(COINobj, inames = NULL, type = "Box", facetplot = TRUE, ntype = NULL, npara = NULL){
   
  # First. check to see what kind of input we have.
  if ("COIN object" %in% class(COINobj)){ # COIN obj
    
    ind_data <- COINobj$data$data_raw # get indicator data
    ind_names <- COINobj$parameters$ind_names # get indicator names
    
  } else if (is.data.frame(COINobj)){ # Data frame
    
    ind_data <- COINobj
    
    if (is.null(inames)){
      ind_names <- colnames(COINobj) # use all columns, if no indicator names supplied
    } else {
      ind_names <- inames 
    }
    
  } else {
    stop("Input should either be COIN object or data.frame.")
  }
  
  # select indicators to plot, if specified. Otherwise, plot all.
  datain <- select(ind_data,all_of(ind_names))
  
  # Normalise if required
  if (!is.null(ntype)){
    datain<-coin_normalise(datain, ntype, npara)
  }
  
  datamelt <- suppressMessages(melt(datain)) # have to put dataframe in long format for ggplot
  
  if (type == "Box"){
    
    plt <- ggplot(data = datamelt, aes(y = value))
    plt <- plt + geom_boxplot() + theme_light() 
    
  } else if (type == "Dot"){
    
    # Note that this might be messy, and can be adusted with stackratio and dotsize
    
    plt <- ggplot(data = datamelt, aes(x = variable, y = value))
    plt <- plt + geom_dotplot(binaxis = "y", stackdir = "center", dotsize=1, stackratio=0.5, alpha = 0.3) + theme_light() 
    
  } else if (type == "Violin"){
    
    # You might have to resize the window here to make it look OK
    
    plt <- ggplot(data = datamelt, aes(x = variable, y = value))
    plt <- plt + geom_violin(scale = "area") + theme_light()
    
  } else if (type == "Violindot"){
    
    # You might have to resize the window here to make it look OK
    
    plt <- ggplot(data = datamelt, aes(x = variable, y = value))
    plt <- plt + geom_violin(scale = "area") + geom_dotplot(binaxis = "y", stackdir = "center", dotsize=1, stackratio=0.5, alpha = 0.3) + theme_light()
    
  } else if (type == "Histogram"){
    
    # You can adjust the bin width
    
    plt <- ggplot(data = datamelt, aes(x = value))
    plt <- plt + geom_histogram(colour = "#e9ecef", bins = 10) + theme_light()
    
  } 
  
  if (facetplot == FALSE){
    plt <- plt + labs(x = "Indicator", y = "Value") # add axis labels
  } else {
    nfrows <- ceiling(sqrt(nlevels(datamelt$variable))/2) # A way to get the number of rows so that we have about twice as many cols as rows
    plt <- plt + facet_wrap(~ variable, nrow = nfrows, scales="free") + labs(x = "", y = "")
  }
  
  plt
  
}