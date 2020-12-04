#' Format check auxiliary function
#'
#' Checks whether function input is COIN object or data frame, and returns input useful to most coin functions
#' Also 
#' 
#' @param COINobj A list of indicator data, stuctured using the COIN_assemble function
#' @param thresh A 2-length vector, used to detect outliers, where the first value is the skewness threshold, and the second is the kurtosis threshold. Defaults to c(2,3.5).
#'
#' @examples out <- coin_aux_objcheck(COINobj, dset = "raw", inames = NULL)
#'
#' @return A list with .$ind_names is a vector with indicator names, 
#' .$ind_data is a data frame /tibble with indicator data, plus possibly grouping/denominator columns, etc.
#' .$ind_data_only is a data frame/tibble with only indicator data columns (no names, groups, etc)
#' 
#' @export
#' 

coin_aux_objcheck <- function(COINobj, dset = "raw", inames = NULL){
  
  # Check to see what kind of input we have.
  if ("COIN object" %in% class(COINobj)){ # COIN obj
    
    # Select data set to use
    if (dset=="raw"){
      ind_data <- COINobj$data$data_raw # get raw indicator data
    } else  if (dset=="denominated"){
      ind_data <- COINobj$data$data_denominated # get denominated indicator data
    } else  if (dset=="imputed"){
      ind_data <- COINobj$data$data_imputed # get imputed indicator data
    } else  if (dset=="normalised"){
      ind_data <- COINobj$data$data_normalised # get normalised indicator data
    } else  if (dset=="treated"){
      ind_data <- COINobj$data$data_treated # get treated indicator data
    } else {
      stop("dset name not recognised...")
    }
    
    if (is.null(inames)){
      # default: use all indicators
      ind_names <- COINobj$parameters$ind_names
    } else if (length(inames)==1 &
               any(COINobj$parameters$agg_names == inames)) {
      # if only one, check if this is a reference to an aggregation group
      # then find which level this belongs to
      aggcol <- colnames(COINobj$metadata)[grepl(inames, COINobj$metadata)]
      # then get the indicators belonging to this group
      ind_names <- COINlist$metadata %>%
      filter(!!as.symbol(aggcol) == inames) %>%
        pull(Code)
    } else {
      # otherwise, use the indicator names here
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
  
  out <- list(ind_names = ind_names, 
              ind_data = ind_data, 
              ind_data_only = select(ind_data,all_of(ind_names))
              )
  
  return(out)
}