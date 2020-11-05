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

coin_preanalyse <- function(COINobj, thresh = c(2, 3.5, 0.9, 0.8, 65, 1.5), inames = NULL, dset = "raw"){
  
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
  
  ##------ Get loads of different stats on indicators. Will be added to a big table at the end.
  
  imean <- ind_data_only %>% map_dbl(mean, na.rm = T) # means
  imed <- ind_data_only %>% map_dbl(median, na.rm = T) # means
  imin <- ind_data_only %>% map_dbl(min, na.rm = T) # min
  imax <- ind_data_only %>% map_dbl(max, na.rm = T) # max
  istd <- ind_data_only %>% map_dbl(sd, na.rm = T) # std 
  iskew <- ind_data_only %>% map_dbl(moments::skewness, na.rm = T) # skew
  ikurt <- ind_data_only %>% map_dbl(moments::kurtosis, na.rm = T) # kurtosis
  ina <- ind_data_only %>% map_dbl(~sum(is.na(.x)), na.rm = T) # n. missing
  iprcna <- (1-ina/COINobj$parameters$n_unit) * 100  # percent available data
  imissflag <- (iprcna < t_missing) %>% if_else(true = "Low", false = "OK") # flag if data availability is below threshold
  skflag <- ((abs(iskew)>t_skew) & (ikurt>t_kurt)) %>% if_else(true = "Outliers", false = "OK") # flag if exceed both skew and kurt thresholds
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
  # now convert to data fame and add column names
  out_flag <- data.frame(out_flag)
  colnames(out_flag) <- ind_names
  
  # count number of high and low outliers for each variable
  out_low <- out_flag %>% map_dbl(~sum(.x=="Low", na.rm = TRUE))
  out_high <- out_flag %>% map_dbl(~sum(.x=="High", na.rm = TRUE)) # interquartile range
  
  # build indicator stats table. Will add some more columns also below.
  ind_stats <- tibble(
    Indicator = ind_names,
    Min = imin, Max = imax,
    Mean = imean, Median = imed,
    Q.25 = q25, Q.75 = q75, IQ.range = iIQR,
    Std.dev = istd, Skew = iskew, Kurtosis = ikurt,
    N.missing = ina, Prc.complete = iprcna, Low.data.flag = imissflag,
    SK.outlier.flag = skflag, Low.Outliers.IQR = out_low, High.Outliers.IQR = out_high
  )
  
  COINobj$Analysis$outlier_flag <- out_flag
  
  ##------- Now checking correlations ---------
  
  # isolate relevant data
  den_data_only <- select(COINlist$data$denominators, starts_with("Den_"))
  
  # indicator correlations
  corr_ind <- cor(ind_data_only, method = "pearson", use = "na.or.complete") # get correlation matrix, just indicators
  COINobj$Analysis$indicator_correlations <- corr_ind
  diag(corr_ind) <- NA # replace 1s with NAs since we are not interested in them
  p_ind <- cor.mtest(ind_data_only, method = "pearson") # p values
  
  # check for collinearity
  maxcor <- sapply(as.data.frame(abs(corr_ind)), max, na.rm = T) # the max absolute correlations
  maxcor <- if_else(maxcor>t_colin,"Collinear","OK") # if any values exceed threshold, flag
  ind_stats <- ind_stats %>% add_column(Collinearity = maxcor) # add to table
  message(paste("Number of collinear indicators = ",sum(maxcor=="Collinear")))
  
  # check for significant negative correls
  signegs <- map2_dbl(as.data.frame(corr_ind),as.data.frame(p_ind$p), # loops over correlations and p values simultaneously
           ~ sum(.x<0 & .y<0.05,na.rm = T)) # if corr is negative AND p value below 0.05, count
  ind_stats <- ind_stats %>% add_column(Neg.Correls = signegs) # add to table
  message(paste("Number of signficant negative indicator correlations = ",sum(signegs)))
  
  # denominator correlations
  corr_denom <- cor(den_data_only, ind_data_only, method = "pearson", use = "na.or.complete")
  COINobj$Analysis$denominator_correlations <- corr_denom
  maxcor <- as.data.frame(abs(corr_denom)) %>% map_dbl(max, na.rm = T) # the max absolute correlations
  maxcor <- if_else(maxcor>t_denom,"High","OK") # if any values exceed threshold, flag
  ind_stats <- ind_stats %>% add_column(Denom.correlation = maxcor) # add to table
  message(paste("Number of indicators with high denominator correlations = ",sum(maxcor=="High")))
  
  # plot indicator correlations (perhaps move this to another function...)
  corrplot(corr_ind, p.mat = p_ind$p, sig.level = .01, insig = "blank")
  agspec <- select(COINobj$metadata,starts_with(("Agg_"))) # the columns showing aggregation groups. Need for grouping the plot.
  
  # Now overlay aggregation groups
  colourz <- c("black","blue","green","red")
  for (ii in 1:(COINobj$parameters$n_agg_levels-1)){
    groupz <- as.data.frame(table(agspec[,ii])) # this gets a summary of the number of repetitions of each value, so the number of indicators in each agg. group
    corrRect(groupz[,2], col = colourz[ii], lwd = ii) # plot rectangle, changing colour and thickness
  }
  
  ##---- PCA ---------------##
  
  # pca <- prcomp(ind_data_only, center = T, scale. = T)
  # should plot the PCA
  # should also see if sth can be added to the table based on PCA -  maybe see some audits here.
  
  ##---- Write results -----##
  
  if (is.data.frame(COINobj)){
    return(ind_stats) # if a data.frame was input, return data frame. Otherwise append to COIN object.
  } else {
    COINobj$Analysis$Ind_stats <- ind_stats
    return(COINobj)
  }
  
}