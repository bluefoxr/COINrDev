#' Investigating correlations  between indicators
#'
#' Calculates correlations and plots, plus tables. Some plots might be more useful with subsets of indicators - a large number will be cluttered.
#' 
#' @param datain A data frame (tibble) of indicator data
#'
#' @examples df_norm <- coin_normalise(df, ntype="minmax", npara = c(0,1))
#'
#' @return Nice plots
#'
#' @export
#' 

coin_precorr <- function(COINobj, inames = NULL, ntype = NULL, npara = NULL){
  
  # First. check to see what kind of input we have.
  if ("COIN object" %in% class(COINobj)){ # COIN obj
    
    ind_data <- COINobj$data$data_raw # get indicator data
    
    if (is.null(inames)){
      ind_names <- COINobj$parameters$ind_names # get indicator names
    } else {
      ind_names <- inames 
    }
    
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
  
  datain <- select(ind_data,all_of(ind_names)) # select relevant columns to plot
  
  # Normalise if required
  if (!is.null(ntype)){
    datain<-coin_normalise(datain, ntype, npara)
  }
  
  ggpairs(datain)
  
}