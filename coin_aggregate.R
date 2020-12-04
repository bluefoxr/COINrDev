#' Aggregate indicators
#'
#' Takes indicator data and a specified structure and heirarchically aggregates to a single index (or whatever the structure specified)
#'
#' @param COINonj COIN object
#' @param agtype The type of aggregation method.
#' @param dset Which data set (contained in COIN object) to use
#' 
#' @examples COINlist <- coin_aggregate(COINlist, agtype="arith_mean", dset = "normalised")
#'
#' @return An updated COIN object containing the new aggregated data set.
#'
#' @export

coin_aggregate <- function(COINobj, agtype="arith_mean", agweights = NULL, 
                           dset = "imputed", agtype_bylevel = NULL){
  
  # First. check to see what kind of input we have.
  if ("COIN object" %in% class(COINobj)){ # COIN obj
    
    # If COIN obj, select the dataset to aggregate from the COIN object
    if (dset == "raw"){
      ind_data <- COINobj$data$data_raw # get indicator data (raw) 
    } else if (dset == "normalised"){
      ind_data = COINobj$data$data_normalised
    } else if (dset == "imputed"){
      ind_data = COINobj$data$data_imputed
    }
    
  } else {
    stop("Aggregation requires a COIN object (you need data plus the index structure). Use coin_assemble first.")
  }
  
  # get weights - if not explicitly specified, we assume it is in the COIN obj
  if (is.null(agweights)){
    agweights = COINobj$parameters$agweights
  }
  
  ##### NOW AGGREGATE #######
  
  if (agtype == "arith_mean"){ # Arithmetic mean
    
    agg_cols <- COINobj$metadata %>% select(starts_with("Agg_")) # the columns with aggregation info in them
    
    for (aglev in 1:COINobj$parameters$n_agg_levels){ # Loop over number of aggregation levels
      
      agg_colname <- colnames(agg_cols[aglev]) # the name of the aggregation level
      agg_names <- unique(agg_cols[[aglev]]) # the names of the aggregation groups
      weights_lev <- COINobj$parameters$agweights[[aglev]] # the weights at this level
      
      # first we have to get the columns (indicators, or agg levels below) to aggregate
      if (aglev ==1){
        sub_codes <- metad$Code # the ingredients to aggregate are the base indicators 
      } else {
        sub_codes <- pull(agg_cols, aglev-1) # the ingredients to aggregate are aggregate level below
      }

      for (agroup in 1:length(agg_names)){ # loop over aggregation groups, inside the given agg level
        
        iselect <- sub_codes[metad[,agg_colname]==agg_names[agroup]] %>% unique() # get indicator names belonging to group
        # get weights belonging to group, using codes
        weights_group <- weights_lev[unique(sub_codes) %in% iselect]

        # Now get the mean. Had to do in a roundabout way to avoid rowmeans type functions... probably an easier way exists though
        newcol <- ind_data %>% select(all_of(iselect)) %>% rowwise() %>% 
          transmute(!!agg_names[agroup] := weightedMean(c_across(cols = everything()), 
                                                        w = weights_group, na.rm = TRUE))
        ind_data <- cbind(ind_data,newcol) # add new col to data set
        
        }
    }
  } else if (agtype == "median"){ # Arithmetic mean
    
    agg_cols <- COINobj$metadata %>% select(starts_with("Agg_")) # the columns with aggregation info in them
    
    for (aglev in 1:COINobj$parameters$n_agg_levels){ # Loop over number of aggregation levels
      
      agg_colname <- colnames(agg_cols[aglev]) # the name of the aggregation level
      agg_names <- unique(agg_cols[[aglev]]) # the names of the aggregation groups
      weights_lev <- COINobj$parameters$agweights[[aglev]] # the weights at this level
      
      # first we have to get the columns (indicators, or agg levels below) to aggregate
      if (aglev ==1){
        sub_codes <- metad$Code # the ingredients to aggregate are the base indicators 
      } else {
        sub_codes <- pull(agg_cols, aglev-1) # the ingredients to aggregate are aggregate level below
      }
      
      for (agroup in 1:length(agg_names)){ # loop over aggregation groups, inside the given agg level
        
        iselect <- sub_codes[metad[,agg_colname]==agg_names[agroup]] %>% unique() # get indicator names belonging to group
        # get weights belonging to group, using codes
        weights_group <- weights_lev[unique(sub_codes) %in% iselect]
        
        #ind_data <- ind_data %>% mutate(!!agg_names[agroup] := rowMeans(select(ind_data, all_of(iselect)), na.rm = TRUE)) # take mean, removing NAs. Also assigned col name using the weird := thing
        
        # Now get the mean. Had to do in a roundabout way to avoid rowmeans type functions... probably an easier way exists though
        newcol <- ind_data %>% select(all_of(iselect)) %>% rowwise() %>% 
          transmute(!!agg_names[agroup] := weightedMedian(c_across(cols = everything()), 
                                                        w = weights_group, na.rm = TRUE))
        ind_data <- cbind(ind_data,newcol) # add new col to data set
        
      }
    }
  } else if (agtype == "mixed"){ # the aggregation can vary from one level to the next
    
    agtype_bylevel
    
    agg_cols <- COINobj$metadata %>% select(starts_with("Agg_")) # the columns with aggregation info in them
    
    for (aglev in 1:COINobj$parameters$n_agg_levels){ # Loop over number of aggregation levels
      
      agg_colname <- colnames(agg_cols[aglev]) # the name of the aggregation level
      agg_names <- unique(agg_cols[[aglev]]) # the names of the aggregation groups
      weights_lev <- COINobj$parameters$agweights[[aglev]] # the weights at this level
      
      # first we have to get the columns (indicators, or agg levels below) to aggregate
      if (aglev ==1){
        sub_codes <- metad$Code # the ingredients to aggregate are the base indicators 
      } else {
        sub_codes <- pull(agg_cols, aglev-1) # the ingredients to aggregate are aggregate level below
      }
      
      agtype_lev <- agtype_bylevel[aglev] # the aggregation type for this level
      
      for (agroup in 1:length(agg_names)){ # loop over aggregation groups, inside the given agg level
        
        iselect <- sub_codes[metad[,agg_colname]==agg_names[agroup]] %>% unique() # get indicator names belonging to group
        # get weights belonging to group, using codes
        weights_group <- weights_lev[unique(sub_codes) %in% iselect]
        
        if (agtype_lev == "arith_mean"){
          newcol <- ind_data %>% select(all_of(iselect)) %>% rowwise() %>% 
            transmute(!!agg_names[agroup] := weightedMean(c_across(cols = everything()), 
                                                          w = weights_group, na.rm = TRUE))
          ind_data <- cbind(ind_data,newcol) # add new col to data set
        } else if (agtype_lev == "median"){
          newcol <- ind_data %>% select(all_of(iselect)) %>% rowwise() %>% 
            transmute(!!agg_names[agroup] := weightedMedian(c_across(cols = everything()), 
                                                          w = weights_group, na.rm = TRUE))
          ind_data <- cbind(ind_data,newcol) # add new col to data set
        } else if (agtype_lev == "geom_mean"){
          
        }
        
        
      }
    }
  }
  
  COINobj$data$data_aggregated <- ind_data # add to the COIN object
  return(COINobj)
}