#' Weight indicators and aggregation levels
#'
#' Assigns weights to indicators and aggregation levels using various different approaches.
#'
#' @param COINonj COIN object
#' @param wtype The type of aggregation method.
#' @param dset Which data set (contained in COIN object) to use
#' 
#' @examples 
#'
#' @return An updated COIN object with weights assigned.
#'
#' @export

coin_weight <- function(COINobj, wtype = "user", user_weights = NULL, output = "to_obj"){
  
  
  
  if (wtype == "user"){ # here we expect a list of weights with one vector per 
    
    # just do some checks first
    
    if (is.null(user_weights)){ # in case weights not specified
      stop("You need to supply a list of user-defined weights.")
    }
    if (!is.list(user_weights)){
      stop("User-defined weights need to be a list, where each list element is a numerical vector of weights for each aggregation level.")
    }
    if (length(user_weights) != (COINobj$parameters$N_agg_level +1)){
      stop("Wrong number of elements in list. Must also include a weight for final index, if it exists (set to 1 usually).")
    }
    
    wts <- user_weights
  
  } else if (wtype == "equal"){
    
    # get columns with indicator codes and aggregation codes
    agg_cols <- COINobj$metadata %>% select(Code | starts_with("Agg_"))
    # here we make a list with an entry for each level, and assign equal weights to each indicator/group
    # weights are normalised to sum to 1
    wts <- lapply(agg_cols, 
                  function(x) rep(1,length(unique(x)))/length(unique(x))
                  )
    
  }
  
  #------- Direct output of function according to call
  
  if (output == "to_obj"){
    
    COINobj$parameters$agweights <- wts
    return(COINobj)
    
  } else if (output == "to_list"){
    
    return(wts)
  }
}