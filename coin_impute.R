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

coin_impute <- function(COINobj, imtype="ind_mean", impara = NULL, inames = NULL, dset = "raw", groupvar = NULL, byyear = FALSE){
  
  # First. check to see what kind of input we have.
  if ("COIN object" %in% class(COINobj)){ # COIN obj
    
    # If COIN obj, select the dataset to impute from the COIN object
    if (dset == "raw"){
      ind_data <- COINobj$data$data_raw # get indicator data (raw) 
    } else if (dset == "normalised"){
      ind_data = tryCatch({
        COINobj$data$data_normalised
      }, error = function(e) {
        stop("No normalised data set exists. Create one first using coin_normalise.")
      })
    }
    
    # get indicator names
    if (is.null(inames)){
      ind_names <- COINobj$parameters$ind_names # get indicator names
    } else {
      ind_names <- inames 
    }
    
  } else if (is.data.frame(COINobj)){ # Data frame
    
    # if the input is a data frame we can just use that
    ind_data <- COINobj
    
    if (is.null(inames)){
      ind_names <- colnames(COINobj) # use all columns, if no indicator names supplied
    } else {
      ind_names <- inames 
    }
    
  } else {
    stop("Input should either be COIN object or data.frame.")
  }
  
  # get number of NAs before imputation
  nasumz <- colSums(is.na(ind_data))
  nNA_start <- sum(nasumz[ind_names])
  
  message(paste("Missing data points detected = ", nNA_start)) 
  
  ###### IMPUTATION ###### 
  
  # first, get some info about years, if needed (either when imputing by year, or when using latest year)
  if (byyear==T | imtype == "latest_year"){ 
    nyears <- ind_data %>% select(starts_with("Year")) %>% unique() %>% nrow() # number of years present
    yrcol <- ind_data %>% colnames() %>% str_subset("Year") # the column name which has the years in it
    yrs <- ind_data %>% select(starts_with("Year")) %>% unique()
  }
  
  ## Now actually do the imputation, depending on the type...
  
  if (imtype == "agg_mean"){ # use the mean of the other indicators in the aggregation group. Only works if data is normalised first.
    
    # first check that normalised data is available
    if (exists("data_normalised",COINlist$data)){ # we may proceed...
      
      # call the aggregation function.... seems easiest. TO FINISH
      
    } else {
      stop("Normalised data set not found (required for agg_mean). Please run coin_normalise first.")
    }
    
  } else if (imtype == "ind_mean"){ # impute using column MEAN, i.e. the mean of the indicator over all units
    
    if (byyear==T){ # If we have to impute by year
      ind_data_imp_list <- vector("list",nyears) # make an empty list for populating, one for each year
      for (yr in 1:nyears){
        
        ind_data_yr <- filter(data,.data[[yrcol]] == yrs[[yr,1]]) # get only rows from year
        #now impute...
        ind_data_imp_list[[yr]] <- ind_data_yr %>% mutate(across(all_of(ind_names), ~{replace_na(.x, mean(.x, na.rm = TRUE))}))
      }
      ind_data_imp <- bind_rows(ind_data_imp_list) # join everything back together
      
    } else { # if not imputing by year
      # the following applies the replace_na function to all columns specified by ind_names
      ind_data_imp <- ind_data %>% mutate(across(all_of(ind_names), ~{replace_na(.x, mean(.x, na.rm = TRUE))}))
    }
    
  } else if (imtype == "ind_median"){ # impute using column MEDIAN, i.e. the median of the indicator over all units
    
    if (byyear==T){ # If we have to impute by year
      ind_data_imp_list <- vector("list",nyears) # make an empty list for populating, one for each year
      for (yr in 1:nyears){
        
        ind_data_yr <- filter(data,.data[[yrcol]] == yrs[[yr,1]]) # get only rows from year
        #now impute...
        ind_data_imp_list[[yr]] <- ind_data_yr %>% mutate(across(all_of(ind_names), ~{replace_na(.x, median(.x, na.rm = TRUE))}))
      }
      ind_data_imp <- bind_rows(ind_data_imp_list) # join everything back together
      
    } else { # if not imputing by year
      # the following applies the replace_na function to all columns specified by ind_names
      ind_data_imp <- ind_data %>% mutate(across(all_of(ind_names), ~{replace_na(.x, median(.x, na.rm = TRUE))}))
    }
    
  } else if (imtype == "indgroup_mean"){ # use column MEAN, restricted to a particular group
    if(is.null(groupvar)){stop("Group mean imputation requires that you specify which grouping to use (column name).")} # throw error if no group
    
    if (byyear==T){ # If we have to impute by year
      ind_data_imp_list <- vector("list",nyears) # make an empty list for populating, one for each year
      for (yr in 1:nyears){
        
        ind_data_yr <- filter(data,.data[[yrcol]] == yrs[[yr,1]]) # get only rows from year
        #now impute...
        ind_data_imp_list[[yr]] <- ind_data_yr %>% group_by(.dots=groupvar) %>% 
          mutate(across(all_of(ind_names), ~replace_na(.x, mean(.x, na.rm = TRUE))))
      }
      ind_data_imp <- bind_rows(ind_data_imp_list) # join everything back together
      
    } else { # if not imputing by year
      # This works by grouping the data by the grouping variable first. Operations then performed by group.
      ind_data_imp <- ind_data %>% group_by(.dots=groupvar) %>% 
        mutate(across(all_of(ind_names), ~replace_na(.x, mean(.x, na.rm = TRUE))))
    }
    
  } else if (imtype == "indgroup_median"){ # use column MEDIAN, restricted to a particular group
    if(is.null(groupvar)){stop("Group median imputation requires that you specify which grouping to use (column name).")} # throw error if no group
    
    if (byyear==T){ # If we have to impute by year
      ind_data_imp_list <- vector("list",nyears) # make an empty list for populating, one for each year
      for (yr in 1:nyears){
        
        ind_data_yr <- filter(data,.data[[yrcol]] == yrs[[yr,1]]) # get only rows from year
        #now impute...
        ind_data_imp_list[[yr]] <- ind_data_yr %>% group_by(.dots=groupvar) %>% 
          mutate(across(all_of(ind_names), ~replace_na(.x, median(.x, na.rm = TRUE))))
      }
      ind_data_imp <- bind_rows(ind_data_imp_list) # join everything back together
      
    } else { # if not imputing by year
      # This works by grouping the data by the grouping variable first. Operations then performed by group.
      ind_data_imp <- ind_data %>% group_by(.dots=groupvar) %>% 
        mutate(across(all_of(ind_names), ~replace_na(.x, median(.x, na.rm = TRUE))))
    }
    
  } else if (imtype == "latest_year"){ # substitute NAs with any available points from previous years
    
    ind_data_imp_list <- vector("list",nyears) # make an empty list for populating, one for each year
    ind_data_imp_list[[1]] <- filter(data,.data[[yrcol]] == yrs[[1,1]]) # only imputing backwards in time, so first year available remains the same.
    
    if(nyears>1){
    for (yr in 2:nyears){
      
      # get indicator from year and year-1 as separate dfs
      ind_data_yr_all <- filter(data,.data[[yrcol]] == yrs[[yr,1]]) %>% as.data.frame() # get only rows from year and ind. cols. Have to change to df because otherwise next step doesn't work
      ind_data_yr <- ind_data_yr_all %>% select(ind_names) # done in 2 steps so can access the other cols in a min.
      ind_data_prev_yr <- filter(data,.data[[yrcol]] == yrs[[yr-1,1]]) %>% as.data.frame() %>% select(ind_names) # get only rows from year-1
      
      #now substitute any NAs from yr with those from prev_yr
      ind_data_yr[is.na(ind_data_yr)] <- ind_data_prev_yr[is.na(ind_data_yr)]
      
      ind_data_yr <- cbind(select(ind_data_yr_all,-ind_names),ind_data_yr)
      
      ind_data_imp_list[[yr]] <- ind_data_yr # add to the list
    }
    ind_data_imp <- bind_rows(ind_data_imp_list) # join everything back together
    } else {stop("You can't impute by latest year with only one year of data.")}
  }
  
  if (is.data.frame(COINobj)){ # Data frame
    return(ind_data_imp)
  } else {
    COINobj$data$data_imputed <- ind_data_imp
    COINobj$Method$Imputation <- imtype
  }
  
  nasumz <- colSums(is.na(ind_data_imp))
  nNA_end <- sum(nasumz[ind_names]) # counts total number of NAs in indicator columns, after imputation
  message(paste("Missing data points imputed = ", nNA_start-nNA_end, "using method =", imtype)) 
  
  return(COINobj)
  
}
