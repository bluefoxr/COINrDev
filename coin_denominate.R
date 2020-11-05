#' Denominate indicator data sets
#'
#' Indicators can be denominated (divided) by other external indicators. Typically, the aim here is to convert extensive (size-related) variables into intensive variables (comparable between units of different sizes).
#'
#' @param COINobj COIN object
#' @param specby Selects the source of the specifications for denomination. If "metadata", uses the denominator column in .$metadata. If "user", takes a character vector of denominator codes (one for each indicator, with NA for indicators that should not be denominated, and in the same order as the indicators).
#'
#' @examples df_norm <- coin_normalise(df, ntype="minmax", npara = c(0,1))
#'
#' @return An updated COIN object, with a new entry .$data$data_denominated of denominated indicators.
#'
#' @export

coin_denominate <- function(COINobj, specby = "metadata", denomby = NULL){
  
  ind_names <- COINobj$parameters$ind_names
  
  if (specby == "metadata"){ # use the metadata table to specify which indicators to use for denomination
    
    denoms <- cbind(COINobj$data$denominators,"Ones"=1) # get denominator cols and add a dummy column of ones
    den_spec <- COINobj$metadata$Denominator %>% replace(is.na(COINobj$metadata$Denominator),"Ones") # the vector specifying which denominators to use. Replace NAs with "Ones"
    denomtrix <- denoms[den_spec] # build data frame, same size as indicator data frame, with corresponding denominator columns
    data_denom <- data # make a copy just to be safe
    data_denom[ind_names] <- data[ind_names]/denomtrix # divide one by the other to give denominated data.
    
  } else if (specby == "user"){
    
    denoms <- cbind(COINobj$data$denominators,"Ones"=1) # get denominator cols and add a dummy column of ones
    den_spec <- denomby %>% replace(is.na(COINobj$metadata$Denominator),"Ones") # the vector specifying which denominators to use. Replace NAs with "Ones"
    denomtrix <- denoms[den_spec] # build data frame, same size as indicator data frame, with corresponding denominator columns
    data_denom <- data # make a copy just to be safe
    data_denom[ind_names] <- data[ind_names]/denomtrix # divide one by the other to give denominated data.
    
  }
  
  COINobj$data$data_denominated <- data_denom
  COINobj$method$denominators <- den_spec %>% replace(den_spec == "Ones", NA)
  return(COINobj)
}
  