#' Build COIN object
#'
#' This takes the raw data provided by the user and puts it into an list format (COIN object) that is recognised by COINr. It also checks whether there are any syntax errors in the data provided.
#'
#' @param data A dataframe of indicator data.
#' @param metadata A dataframe containing auxillary information for each indicator
#' @param framewk A dataframe specifiying the names and weights of each aggregation group
#'
#' @examples df_norm <- coin_normalise(df, ntype="minmax", npara = c(0,1))
#'
#' @return A "COIN object" (list) formatted to the specifications of COINr. Note that the COIN object is just a tag. It doesn't impose restrictions on the structure of the list.
#'
#' @export

coin_assemble <- function(data_raw, metad, framewk){
  
  # Extract indicator codes from raw data
  cnames1 <- data_raw %>% select(!starts_with(c("Code_","Year_","Name_","Group_","Den_"))) %>% colnames()
  
  denoms <- data_raw %>% select(starts_with(c("Code_","Year_","Den_"))) # keep denominators to one side for the moment
  data_raw <- data_raw %>% select(!starts_with("Den_"))
  
  # Build list
  COINlist <- list("data" = list("data_raw" = data_raw, "denominators" = denoms), "metadata" = metad, "framework" = framewk)
  
  # Check that codes in the two tables match, save to list
  if (length(setdiff(cnames1,metad$Code)) > 0){
    
    stop("Indicator codes in metadata table and indicator table are not the same. Please correct.")
    
  } else {
    
    message("-----------------")
    message("Indicator codes cross-checked and OK.")
    message("-----------------")
    COINlist$parameters$n_ind <- length(cnames1) # save to list
    COINlist$parameters$ind_names <- cnames1 # save to list
    COINlist$parameters$n_unit <- n_distinct(data_raw$Code_country, na.rm = TRUE)
    message(paste("Number of indicators =",length(cnames1)))
    message(paste("Number of units =",COINlist$parameters$n_unit))
    
    if (ncol(select(data,starts_with("Year"))) > 0){
      message(paste("Number of reference years of data =",n_distinct(select(data_raw,starts_with("Year")))))
      message(paste("Years from",min(select(data_raw,starts_with("Year"))),"to",max(select(data_raw,starts_with("Year")))))
    } else {
      message("No Year column detected in input data. Assuming you only have one year of data.")
    }
    
  }
  
  # Check aggregation levels present and say how many
  agg_cols <- metad %>% select(starts_with("Agg_"))
  n_agg_levels <- length(agg_cols)
  
  agg_cols_fwk <- select(framewk,ends_with("_code"))
  
  if (n_agg_levels > 0){
    
    message(paste("Number of aggregation levels =", n_agg_levels))
    message("-----------------")
    
    # Loop through aggregation levels. Get names of agg groups and store, plus print to console.
    for (agg_no in 1:n_agg_levels){
      
      agg_names <- unique(agg_cols[agg_no])
      n_agg_groups <- nrow(agg_names)
      message(paste("Aggregation level",agg_no,"with",n_agg_groups,"aggregate groups:",paste0(agg_names[[1]], collapse=", ")))
      
      # TO DO: how to create list
      
      #COINlist$parameters$aggregation[agg_no] <- agg_names
      #COINlist$parameters$aggregation[[agg_no]]$n_groups <- n_agg_groups
      
      agg_cols_fwk_iter <- na.omit(agg_cols_fwk[[agg_no]])
      
      if (length(setdiff(agg_cols_fwk_iter,agg_names[[1]])) > 0){
        stop("Aggregation codes in framework are not consistent with metadata")
      }  else {
        message("Cross-check between metadata and framework = OK.")
      }
      
    }
    
  } else {
    
    warning("No aggregation levels were detected.")
    
  }
  COINlist$parameters$n_agg_levels <- n_agg_levels # save to list
  
  message("-----------------")
  
  class(COINlist) <- "COIN object" # assigns a "COIN object" class to the list. Helpful for later on.
  
  return(COINlist)
  
}