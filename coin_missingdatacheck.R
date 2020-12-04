#' Detailed missing data analysis
#' 
#' More detailed data analysis by each aggregation level.
#' 
#' @param COINobj The COINobj object
#' @param ind_thresh A data availability threshold, which controls both Rule 1 and 2. Default 0.66. Specify as a fraction.
#' @param Rule1 Logical: if TRUE, Rule1 is applied (default), else if FALSE Rule1 is not applied.
#' @param Rule2 Logical: if TRUE, Rule1 is applied, else if FALSE Rule1 is not applied (default).
#' @param Force A data frame with any additional countries to force inclusion or exclusion. First column is ISO code. Second column either "Include" or "Exclude" for each country to force.
#'
#' @return An updated COINobj object with tables showing missing data, and a filtered list of countries to include in subsequent calculations.
#' @export
#'
#' @examples
#' 
coin_missingdatacheck <- function(COINobj, ind_thresh=2/3, Rule1 = TRUE, Rule2 = FALSE, Force = NULL){
  
  # Write threshold to object for the record
  COINobj$meth$ind_thresh <- ind_thresh
  
  # Isolate indicator data
  ind_data_only <- COINobj$data$raw %>% select(all_of(COINobj$param$ind_names))
  
  #--- RULE 1: 66% data for all countries or OUT
  
  nabyrow <- rowSums(is.na(ind_data_only)) # number of missing data by row
  Prc_avail = 1 - nabyrow/ncol(input_data) # the percentage of data available
  
  data_avail <- data.frame(Code = COINobj$param$unit_names,
                           N_missing = nabyrow,
                           PrcDataAll = Prc_avail*100,
                           LowDataAll = Prc_avail<(ind_thresh))
  
  #--- RULE 2: 66% data availability at the sub-pillar level or ADIOS
  
  # the easiest way to do this is to loop over sub-pillars. Get first the index structure
  # (selects indicator codes plus all aggregation level columns/codes)
  agg_levels <- select(COINobj$input$ind_meta, "Code" | starts_with("Agg_"))
  
  data_avail_bygroup <- data.frame("Code" = COINobj$param$unit_names)
  
  for (ilev in 1:(ncol(agg_levels)-1)){ # loop over aggregation levels, except the last one
    
    agg1 <- pull(agg_levels,1) # names of indicators
    agg2 <- pull(agg_levels,ilev+1) # names of aggregation groups in the level above
    agg2_names <- unique(agg2) # only the names of agg level above (no repetitions)
    
    # pre-allocate a data frame for prc data availability
    d_avail_lev <- as.data.frame(matrix(NA, nrow = nrow(ind_data_only), ncol = length(agg2_names)))
    
    for (igroup in 1:length(agg2_names)){ # now looping over groups inside this level
      
      gname <- agg2_names[igroup] # select group name
      
      # get indicator codes belonging to group
      gcodes <- agg1[agg2 == gname]
      # get corresponding indicator columns
      ginds <- ind_data_only %>% select(all_of(gcodes))
      # now count prc data available and add to data frame
      d_avail_lev[,igroup] <- 100*rowSums(!is.na(ginds))/ncol(ginds)
      
    }
    
    # add column names (aggregation group names) to data availability table
    colnames(d_avail_lev) <- agg2_names
    # add to big table
    data_avail_bygroup <- cbind(data_avail_bygroup, d_avail_lev)
    
  }
  
  #----- Flag data availability at the sub-index level
  
  # get subindex codes, remove NAs
  SI_codes <- unique(agg_levels$Agg_3)
  SI_codes <- SI_codes[!is.na(SI_codes)]
  
  SI_avail <- select(data_avail_bygroup, all_of(SI_codes))
  
  data_avail <- data_avail %>% add_column(
    PrcDataInput = pull(SI_avail,1),
    PrcDataOuput = pull(SI_avail,2),
    LowDataSI = rowSums(SI_avail < ind_thresh*100) > 0
  )
  
  #----- Now we will apply the rule of 66 at the pillar level. Slightly more complicated.
  
  # get sub-pillar codes
  SP_codes <- COINobj$input$agg_meta$Agg_1_code
  # get data availability, by country, for each sub-pillar
  SP_avail <- data_avail_bygroup %>% select(all_of(SP_codes))
  # assign NAs to anything less than threshold 
  SP_avail[SP_avail < ind_thresh*100] <- NA
  
  P_codes <- unique(COINobj$input$agg_meta$Agg_2_code)
  P_codes <- P_codes[!is.na(P_codes)]
  
  # pre-allocate a data frame for prc data availability
  P_avail <- as.data.frame(matrix(NA, nrow = nrow(ind_data_only), ncol = length(P_codes)))
  
  for (igroup in 1:length(P_codes)){ # now looping over groups inside this level
    
    gname <- P_codes[igroup] # select group name
    
    # get indicator codes belonging to group
    gcodes <- agg_levels$Agg_1[agg_levels$Agg_2 == gname] %>% unique()
    # get corresponding indicator columns
    gSPs <- SP_avail %>% select(all_of(gcodes))
    # now count prc data available and add to data frame
    P_avail[,igroup] <- 100*rowSums(!is.na(gSPs))/ncol(gSPs)
    
  }
  
  # add pillar names
  colnames(P_avail) <- P_codes
  # add code column
  P_avail <- P_avail %>% add_column(
    Code = COINobj$param$unit_names,
    .before = 1)
  
  # add low data flag to output table
  data_avail <- data_avail %>% add_column(
    LowDataPillar = rowSums(P_avail < ind_thresh*100) > 0
  )
  
  # Now add final column which says if country is included or not
  if (Rule1==T & Rule2==T){
    data_avail <- data_avail %>%
      mutate(Included = LowDataPillar == FALSE & LowDataSI == FALSE)
    # i.e. it is TRUE (included) if both low data flags are FALSE
  } else if (Rule1==T & Rule2==F){
    data_avail <- data_avail %>%
      mutate(Included = LowDataSI == FALSE)
    # i.e. it is TRUE (included) if SI data is sufficient. Don't worry about Rule2
  } else if (Rule1==F & Rule2==T){
    data_avail <- data_avail %>%
      mutate(Included = LowDataPillar == FALSE)
    # i.e. it is TRUE (included) if pillar data is sufficient. Don't worry about Rule1
  } else {
    data_avail <- data_avail %>% 
      mutate(Included = TRUE) # include everything
  }
  
  if (!is.null(Force)){ # if some countries to force include/exclude
    # convert to logical
    Force[2] <- Force[2]=="Included"
    # substitute in output table
    data_avail$Included[ data_avail$Code %in% Force$Code[Force$Status == TRUE] ] <- TRUE
    data_avail$Included[ data_avail$Code %in% Force$Code[Force$Status == FALSE] ] <- FALSE
    
  }
  
  
  # add summary tables to object
  COINobj$analysis$data_avail <- data_avail
  COINobj$analysis$data_avail_detailed <- data_avail_bygroup
  COINobj$analysis$data_avail_pillar <- P_avail
  
  # create new data set which filters out the countries that didn't make the cut
  COINobj$data$screened <- filter(COINobj$data$raw, COINobj$analysis$data_avail$Included)
  
  return(COINobj)
}