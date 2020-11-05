#' Normalise indicator data sets
#'
#' A dataset of indicators is normalised using one of several methods. This function also supports custom normalisation.
#'
#' @param df A dataframe of indicator data following the COINR format
#' @param ntype The type of normalisation method. Either "minmax", "zscore", or "custom".
#' @param npara Supporting object for ntype.
#' @param inames Character vector of indiator names, indicating which columns to normalise. Use this if you only want to normalise certain columns, or you are inputting a data frame with some columns which are not to be normalised (e.g. country names, groups, ...)
#'
#' @examples df_norm <- coin_normalise(df, ntype="minmax", npara = c(0,1))
#'
#' @return A dataframe of normalised indicators.
#'
#' @export

coin_normalise <- function(COINobj, ntype="minmax", npara = NULL, inames = NULL, dset = "raw"){

  # First. check to see what kind of input we have.
  if ("COIN object" %in% class(COINobj)){ # COIN obj
    
    # First, select the dataset to impute from the COIN object
    if (dset == "raw"){
      ind_data <- COINobj$data$data_raw # get indicator data (raw) 
    } else if (dset == "imputed"){
      ind_data = tryCatch({
        COINobj$data$data_imputed
      }, error = function(e) {
        stop("No normalised data set exists. Create one first using coin_normalise.")
      })
    }
    
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
  
  databit<-select(ind_data,all_of(ind_names)) # select relevant columns to normalise

  if (ntype == "minmax"){

    if (is.null(npara)){ # default parameters
      npara <- c(0,100)
    }
    datamod<-modify(databit,~{ (.x-min(.x, na.rm = TRUE))/(max(.x, na.rm = TRUE)-min(.x, na.rm = TRUE))*(npara[2]-npara[1]) + npara[1]} )

  } else if (ntype == "zscore"){

    if (is.null(npara)){ # default parameters
      npara <- c(0,1)
    }
    datamod<-modify(databit,~{(.x-mean(.x, na.rm = TRUE))/sd(.x, na.rm = TRUE)})

  } else if (ntype == "custom"){

    datamod = tryCatch({
      modify(databit,npara)
    }, error = function(e) {
      stop("Error: custom function not valid for some reason.")
    })

  }
  
  if (is.data.frame(COINobj)){ # Data frame
    COINobj[ind_names] <- datamod
  } else {
    dataout <- COINobj$data$data_raw
    dataout[ind_names] <- datamod
    COINobj$data$data_normalised <- dataout
  }

  return(COINobj)
  
}
