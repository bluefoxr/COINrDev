#' Pre-aggregation analysis
#'
#' Takes a COIN object, or data frame and returns a table of statistics, including max, min, median, mean, std, kurtosis, etc. Flags indicators with possible outliers.
#' 
#' @param COINobj A list of indicator data, stuctured using the COIN_assemble function
#' @param thresh A 2-length vector, used to detect outliers, where the first value is the skewness threshold, and the second is the kurtosis threshold. Defaults to c(2,3.5).
#'
#' @examples df_norm <- coin_normalise(df, ntype="minmax", npara = c(0,1))
#'
#' @return If the input is a COIN object, returns an updated COIN object with relevant tables. If the input is a data frame, returns a table as a data frame.
#'
#' @export
#' 

coin_preanalyse_old <- function(COINobj, thresh = c(2, 3.5, 0.9, 0.8, 65, 1.5), inames = NULL, dset = "raw"){
  
  t_skew <- thresh[1] # skewness threshold
  t_kurt <- thresh[2] # kurtosis threshold
  t_colin <- thresh[3] # collinearity threshold (absolute value of correlation)
  t_denom <- thresh[4] # high correlation with denominator threshold
  t_missing <- thresh[5] # missing data theshold, in percent
  IQR_coef <- thresh[6]
  
  # First. check to see what kind of input we have.
  if ("COIN object" %in% class(COINobj)){ # COIN obj
    
    # Select data set to use
    if (dset=="raw"){
      ind_data <- COINobj$data$data_raw # get raw indicator data
    } else  if (dset=="denominated"){
      ind_data <- COINobj$data$data_denominated # get raw indicator data
    } else  if (dset=="imputed"){
      ind_data <- COINobj$data$data_imputed # get raw indicator data
    } else  if (dset=="normalised"){
      ind_data <- COINobj$data$data_normalised # get raw indicator data
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
  
  ind_data_only <- ind_data[ind_names] # get just indicator data
  
  imean <- ind_data_only %>% map_dbl(mean, na.rm = T) # means
  imin <- ind_data_only %>% map_dbl(min, na.rm = T) # min
  imax <- ind_data_only %>% map_dbl(max, na.rm = T) # max
  istd <- ind_data_only %>% map_dbl(sd, na.rm = T) # std 
  iskew <- ind_data_only %>% map_dbl(moments::skewness, na.rm = T) # skew
  ikurt <- ind_data_only %>% map_dbl(moments::kurtosis, na.rm = T) # kurtosis
  ina <- ind_data_only %>% map_dbl(~sum(is.na(.x)), na.rm = T) # n. missing
  iprcna <- (1-ina/COINobj$parameters$n_unit) * 100  # percent available data
  imissflag <- iprcna < t_missing # flag if data availability is below threshold
  skflag <- (abs(iskew)>t_skew) & (ikurt>t_kurt) # flag if exceed both skew and kurt thresholds
  q25 <- ind_data_only %>% map_dbl(~quantile(.x, probs = 0.25, na.rm = TRUE)) # 25th prc
  q75 <- ind_data_only %>% map_dbl(~quantile(.x, probs = 0.75, na.rm = TRUE)) # 75th prc
  iIQR <- ind_data_only %>% map_dbl(IQR, na.rm = TRUE) # interquartile range
  
  # switching some things into a for loop for clarity
  out_flag <- matrix("OK", nrow = nrow(ind_data_only), ncol = ncol(ind_data_only)) # empty df for populating

  for (ii in 1:ncol(ind_data_only)){
    
    # Populate matrix with high and low outliers, using IQR approach
    icol <- ind_data_only[ii]
    flag_col <- out_flag[,ii]
    flag_col <- replace(flag_col, icol<(q25[ii]-IQR_coef*iIQR[ii]), "Low")
    flag_col <- replace(flag_col, icol>(q75[ii]+IQR_coef*iIQR[ii]), "High")
    out_flag[,ii] <- flag_col
    
    
  }
  out_flag <- data.frame(out_flag)
  colnames(out_flag) <- ind_names
  
  out_low <- out_flag %>% map_dbl(~sum(.x=="Low", na.rm = TRUE))
  out_high <- out_flag %>% map_dbl(~sum(.x=="High", na.rm = TRUE)) # interquartile range
  
  ind_stats <- ind_data %>% summarise_at(ind_names, mean, na.rm = TRUE) # initialise stats table (MEAN)
  
  # Add rows for each statistic of interest
  ind_stats <-  add_row(ind_stats,summarise_at(ind_data,ind_names, median, na.rm = TRUE)) # MEDIAN
  ind_stats <-  add_row(ind_stats,summarise_at(ind_data,ind_names, min, na.rm = TRUE)) # MIN
  ind_stats <-  add_row(ind_stats,summarise_at(ind_data,ind_names, max, na.rm = TRUE)) # MAX
  ind_stats <-  add_row(ind_stats,summarise_at(ind_data,ind_names, sd, na.rm = TRUE)) # STD
  ind_stats <-  add_row(ind_stats,summarise_at(ind_data,ind_names, moments::skewness, na.rm = TRUE)) # SKEW
  ind_stats <-  add_row(ind_stats,summarise_at(ind_data,ind_names, moments::kurtosis, na.rm = TRUE)) # KURTOSIS
  ind_stats <-  add_row(ind_stats,summarise_at(ind_data,ind_names, ~sum(is.na(.x)), na.rm = TRUE)) # N. MISSING DATA
  ind_stats <-  add_row(ind_stats,summarise_at(ind_data,ind_names, ~sum(!is.na(.x))/length(.x)*100, na.rm = TRUE)) # % complete
  ind_stats <-  add_row(ind_stats,summarise_at(ind_data,ind_names, ~(abs(moments::skewness(.x, na.rm = TRUE)) > 
                                                                       t_skew & moments::kurtosis(.x, na.rm = TRUE) > t_kurt), na.rm = TRUE)) # OULIER FLAG
  ind_stats <-  add_row(ind_stats,summarise_at(ind_data,ind_names, ~quantile(.x, probs = 0.25, na.rm = TRUE)))
  ind_stats <-  add_row(ind_stats,summarise_at(ind_data,ind_names, ~quantile(.x, probs = 0.75, na.rm = TRUE)))
  ind_stats <-  add_row(ind_stats,summarise_at(ind_data,ind_names, ~IQR(.x, na.rm = TRUE) ))
  ind_stats <-  add_row(ind_stats,summarise_at(ind_data,ind_names, ~sum(.x < (quantile(.x, probs = 0.25, na.rm = TRUE) - 
                                                                                1.5*IQR(.x, na.rm = TRUE)), na.rm = T ) ))
  ind_stats <-  add_row(ind_stats,summarise_at(ind_data,ind_names, ~sum(.x > (quantile(.x, probs = 0.75, na.rm = TRUE) + 
                                                                                1.5*IQR(.x, na.rm = TRUE)), na.rm = T ) ))
  
  browser()
  
  # matrix which looks for any points which are considered outliers according to IQR approach, and replaces them with arbitary number
  outlier_flag <- ind_data %>% mutate(across(all_of(ind_names), ~{replace(
    (.x < (quantile(.x, probs = 0.25, na.rm = TRUE) - 1.5*IQR(.x, na.rm = TRUE)) | # condition OR
       .x > (quantile(.x, probs = 0.75, na.rm = TRUE) + 1.5*IQR(.x, na.rm = TRUE)) ), # other condition
    1000001)})) # replacement value
    
  COINobj$Analysis$outlier_flag <- outlier_flag  
  
  ##------- Now checking correlations ---------
  
  # isolate relevant data
  ind_data_only <- ind_data[ind_names]
  den_data_only <- select(COINlist$data$denominators, starts_with("Den_"))
  
  # indicator correlations
  corr_ind <- cor(ind_data_only, method = "pearson", use = "na.or.complete") # get correlation matrix, just indicators
  COINobj$Analysis$indicator_correlations <- corr_ind
  diag(corr_ind) <- NA # replace 1s with NAs since we are not interested in them
  p_ind <- cor.mtest(ind_data_only, method = "pearson") # p values
  
  # check for collinearity
  maxcor <- sapply(as.data.frame(abs(corr_ind)), max, na.rm = T) # the max absolute correlations
  maxcor <- if_else(maxcor>t_colin,1,0) # if any values exceed threshold, flag
  ind_stats <- rbind(ind_stats,maxcor) # add to table
  message(paste("Number of collinear indicator pairs = ",sum(maxcor)))
  
  # denominator correlations
  corr_denom <- cor(ind_data_only, den_data_only, method = "pearson", use = "na.or.complete")
  COINobj$Analysis$denominator_correlations <- corr_denom
  diag(corr_denom) <- NA
  maxcor <- sapply(as.data.frame(abs(corr_denom)), max, na.rm = T) # the max absolute correlations
  maxcor <- if_else(maxcor>t_denom,1,0) # if any values exceed threshold, flag
  ind_stats <- rbind(ind_stats,maxcor) # add to table
  message(paste("Number of denominator correlations = ",sum(maxcor)))
  
  ## ---- Now also should check outliers in different ways ----- ##
  
  sapply(as.data.frame(ind_data_only), boxplot.stats, coef=2) # this will get number of outliers, according to a formula. Coef is a multiple of IQRs (to be parameterised)
  
  statnames = c("Mean", "Median", "Min", "Max", "St. dev.", "Skewness", "Kurtosis", "N. missing", 
                "Prc complete", "SK Outliers", "25th percentile", "75th percentile", "Interquartile range",
                "Low outliers (IQR)", "High outliers (IQR)", "Collinearity", "Corr. with denominator")
  
  ind_stats <- ind_stats %>% add_column(Statistic = statnames, .before = ind_names[1]) # add column for descriptor
  
  
  # plot indicator correlations (perhaps move this to another function...)
  corrplot(corr_ind, p.mat = p_ind$p, sig.level = .01, insig = "blank")
  agspec <- select(COINobj$metadata,starts_with(("Agg_"))) # the columns showing aggregation groups. Need for grouping the plot.
  
  colourz <- c("black","blue","green","red")
  for (ii in 1:(COINobj$parameters$n_agg_levels-1)){
    groupz <- as.data.frame(table(agspec[,ii])) # this gets a summary of the number of repetitions of each value, so the number of indicators in each agg. group
    corrRect(groupz[,2], col = colourz[ii], lwd = ii) # plot rectangle, changing colour and thickness
  }
  
  
  if (is.data.frame(COINobj)){
    return(ind_stats) # if a data.frame was input, return data frame. Otherwise append to COIN object.
  } else {
    COINobj$Analysis$Ind_stats <- ind_stats
    return(COINobj)
  }
  
}