#' Treatment of outliers
#'
#' Takes a COIN object, or data frame and returns an equivalent data set but with outliers treated
#' Uses Winsorisation as well as transformations.
#' 
#' @param COINobj A list of indicator data, structured using the COIN_assemble function
#' @param dset The data set to treat
#' @param inames The set of indicators to treat
#' @param winmax The maximum number of points to Winsorise for each indicator
#' @param wintype Type of Winsorisation to perfom. "Max" or "prc".
#' @param thresh A 2-length vector with absolute skew threshold and kurtosis threshold
#' @param individual A data frame specifying individual treatment for each indicator
#' @param indiv_only Logical: if TRUE, only the indicators specified in "individual" are treated. 
#' If false, all indicators are treated: any outside of "individual" will get default treatment.
#'
#' @examples 
#'
#' @return If the input is a COIN object, returns an updated COIN object. If the input is a data frame, returns a table as a data frame.
#'
#' @export
#' 

coin_treat <- function(COINobj, dset = "raw", inames = NULL, winmax = NULL, wintype = "max",
                       thresh = c(2, 3.5), individual = NULL, indiv_only = TRUE){
  
  # check input type, get basic data sets
  objcheck <- coin_aux_objcheck(COINobj, dset = dset, inames = inames)
  ind_data <- objcheck$ind_data
  ind_data_only <- objcheck$ind_data_only
  ind_names <- objcheck$ind_names
  
  t_skew <- thresh[1] # skewness threshold
  t_kurt <- thresh[2] # kurtosis threshold
  
  # if winmax not specified, default to 10% of units
  if(is.null(winmax)){
    winmax <- floor(0.1*nrow(ind_data_only))
  }
  
  ind_data_treated <- ind_data # make a copy, for treated data
  treat_flag <- matrix("No", nrow = nrow(ind_data_only), ncol = ncol(ind_data_only)) # empty matrix for populating
  
  ###----------- DEFAULT INDICATOR TREATMENT -------------------------
  
  if (is.null(individual)){ # means that all indicators follow the same default treatment process
  
  # looping over indicators (columns)
  for (ii in 1:ncol(ind_data_only)){
    
    icol <- pull(ind_data_only,ii) # get relevant column
    
    # first, check skew and kurtosis
    sk <- moments::skewness(icol, na.rm = T)
    kt <- moments::kurtosis(icol, na.rm = T)
    
    winz<-0 # set counter to 0
    imax<-imin<-NULL # reset to NULL
    
    while ( ((abs(sk)>t_skew) & (kt>t_kurt)) & (winz <= winmax) ) { # keep going until sk and kt below thresholds OR reached winsorisation limit
      
      if (sk>=0){ # skew is positive, implies high outliers
        
        imax <- which(icol==max(icol, na.rm = T)) # position(s) of maximum value(s)
        if (wintype=="max"){
          icol[imax] <- max(icol[-imax], na.rm = T) # replace imax with max value of indicator if imax value(s) excluded
        } else if (wintype=="prc"){
          icol[imax] <- quantile(icol,0.95, na.rm = T)
        }
        
      } else { # skew is negative, implies low outliers
        
        imin <- which(icol==min(icol, na.rm = T)) # ditto, but with min
        if (wintype=="max"){
          icol[imin] <- min(icol[-imin], na.rm = T)
        } else if (wintype=="prc"){
          icol[imin] <- quantile(icol,0.05, na.rm = T)
        }
        
      }
      
      # test skew and kurtosis again
      sk <- moments::skewness(icol, na.rm = T)
      kt <- moments::kurtosis(icol, na.rm = T)
      
      winz<-winz+1 # add the winsorisation counter
    }
    
    # Here, loop may have exited because treatment succeeded, or reached winmax. Let's check
    if ( (winz >= winmax)  &  ((abs(sk)>t_skew) & (kt>t_kurt)) ){ # didn't work
      
      icol <- pull(ind_data_only,ii) # get fresh version of column
      
      if (sum(icol<=0, na.rm=T)>0){ # negative values. No can log.
        
        warning(paste0(ind_names[ii],": indicator exceeded max winsorisation but cannot do log transform because negative or zero values. Please check."))
        treat_flag[,ii] <- "Err"
        # icol will be passed through with no treatment
        
      } else { # OK to log
        
        icol <- log(icol)
        treat_flag[,ii] <- "Log"
        
      }
      
    } else { # Winsorization DID work
      treat_flag[imax,ii] <- "WHigh" # Flag Winsorisation (if NULL, will not assign anything)
      treat_flag[imin,ii] <- "WLow"
    }
    
    ind_data_treated[ind_names[ii]]<-icol # subst treated col into treated data set
  }
    
  ###------ INDIVIDUAL INDICATOR TREATMENT -----
    
  } else {
    
    # looping over indicators (columns)
    for (ii in 1:ncol(ind_data_only)){
      
      icol <- pull(ind_data_only,ii) # get relevant column
      ind_name <- ind_names[ii] # get indicator name
      
      # check if this indicator is specified in the "individual" table
      # if it is, we treat it as specified in the table
      if (ind_name %in% individual$Code) {
        # INDIVIDUAL TREATMENT
        
        # Now check which kind of treatment to apply using table
        if( individual$Treat[individual$Code==ind_name] == "win"){
          # this indicator should be winsorised
          
          # first, check skew and kurtosis
          sk <- moments::skewness(icol, na.rm = T)
          kt <- moments::kurtosis(icol, na.rm = T)
          
          winz<-0 # set counter to 0
          imax<-imin<-NULL # reset to NULL
          winmaxi <- individual$Winmax[individual$Code==ind_name] # winmax for this indicator
          
          while ( ((abs(sk)>t_skew) & (kt>t_kurt)) & (winz <= winmaxi) ) { # keep going until sk and kt below thresholds OR reached winsorisation limit
            
            if (sk>=0){ # skew is positive, implies high outliers
              
              imax <- which(icol==max(icol, na.rm = T)) # position(s) of maximum value(s)
              if (wintype=="max"){
                icol[imax] <- max(icol[-imax], na.rm = T) # replace imax with max value of indicator if imax value(s) excluded
              } else if (wintype=="prc"){
                icol[imax] <- quantile(icol,0.95, na.rm = T)
              }
              
            } else { # skew is negative, implies low outliers
              
              imin <- which(icol==min(icol, na.rm = T)) # ditto, but with min
              if (wintype=="max"){
                icol[imin] <- min(icol[-imin], na.rm = T)
              } else if (wintype=="prc"){
                icol[imin] <- quantile(icol,0.05, na.rm = T)
              } 
              
            }
            
            # test skew and kurtosis again
            sk <- moments::skewness(icol, na.rm = T)
            kt <- moments::kurtosis(icol, na.rm = T)
            
            winz<-winz+1 # add the winsorisation counter
          }
          
          # loop exited, we don't know if we succeeded or not and don't go to log
          
          treat_flag[imax,ii] <- "WHigh" # Flag Winsorisation (if NULL, will not assign anything)
          treat_flag[imin,ii] <- "WLow"
          
          ind_data_treated[ind_names[ii]]<-icol # subst treated col into treated data set
          
          # END INDIVIDUAL WINSORISATION
          
        } else if ( individual$Treat[individual$Code==ind_name] == "log"){
          # this indicator should be log transformed
          
          icol <- pull(ind_data_only,ii) # get fresh version of column
          
          if (sum(icol<=0, na.rm=T)>0){ # negative values. No can log.
            
            warning(paste0(ind_names[ii],": cannot do log transform because negative or zero values. Please check."))
            treat_flag[,ii] <- "Err"
            # icol will be passed through with no treatment
            
          } else { # OK to log
            
            icol <- log(icol)
            treat_flag[,ii] <- "Log"
            
          }
          ind_data_treated[ind_names[ii]]<-icol # subst treated col into treated data set
          
          # END INDIVIDUAL LOG TRANSFORM
          
        } else if ( individual$Treat[individual$Code==ind_name] == "none"){
          # this indicator should be excluded from any treatment
          treat_flag[,ii] <- "ForcedNo"
        }
        
      } else if (indiv_only == FALSE){
        # here means that indicator is NOT specified in the individual table, and
        # the indiv_only flag is set so that all other indicators should be treated by default process
        # So, applying default process to this one.
        
        # first, check skew and kurtosis
        sk <- moments::skewness(icol, na.rm = T)
        kt <- moments::kurtosis(icol, na.rm = T)
        
        winz<-0 # set counter to 0
        imax<-imin<-NULL # reset to NULL
        
        while ( ((abs(sk)>t_skew) & (kt>t_kurt)) & (winz <= winmax) ) { # keep going until sk and kt below thresholds OR reached winsorisation limit
          
          if (sk>=0){ # skew is positive, implies high outliers
            
            imax <- which(icol==max(icol, na.rm = T)) # position(s) of maximum value(s)
            if (wintype=="max"){
              icol[imax] <- max(icol[-imax], na.rm = T) # replace imax with max value of indicator if imax value(s) excluded
            } else if (wintype=="prc"){
              icol[imax] <- quantile(icol,0.95, na.rm = T)
            }
            
          } else { # skew is negative, implies low outliers
            
            imin <- which(icol==min(icol, na.rm = T)) # ditto, but with min
            if (wintype=="max"){
              icol[imin] <- min(icol[-imin], na.rm = T)
            } else if (wintype=="prc"){
              icol[imin] <- quantile(icol,0.05, na.rm = T)
            }
            
          }
          
          # test skew and kurtosis again
          sk <- moments::skewness(icol, na.rm = T)
          kt <- moments::kurtosis(icol, na.rm = T)
          
          winz<-winz+1 # add the winsorisation counter
        }
        
        # Here, loop may have exited because treatment succeeded, or reached winmax. Let's check
        if ( (winz >= winmax)  &  ((abs(sk)>t_skew) & (kt>t_kurt)) ){ # didn't work
          
          icol <- pull(ind_data_only,ii) # get fresh version of column
          
          if (sum(icol<=0, na.rm=T)>0){ # negative values. No can log.
            
            warning(paste0(ind_names[ii],": indicator exceeded max winsorisation but cannot do log transform because negative or zero values. Please check."))
            treat_flag[,ii] <- "Err"
            # icol will be passed through with no treatment
            
          } else { # OK to log
            
            icol <- log(icol)
            treat_flag[,ii] <- "Log"
            
          }
          
        } else { # Winsorization DID work
          treat_flag[imax,ii] <- "WHigh" # Flag Winsorisation (if NULL, will not assign anything)
          treat_flag[imin,ii] <- "WLow"
        }
        
        ind_data_treated[ind_names[ii]]<-icol # subst treated col into treated data set
        
      } # end of if indicator in individual table
    } # end indicator loop
  } # end IF indicator individual treatment
    
  # tidy up a bit
  
  ntreated <- data.frame(
    Low = map_dbl(as.data.frame(treat_flag), ~sum(.x=="WLow")),
    High = map_dbl(as.data.frame(treat_flag), ~sum(.x=="WHigh")),
    row.names = ind_names
  )
  
  colnames(treat_flag) <- colnames(ind_data_only)
  treat_flag <- as.data.frame(treat_flag)
  treat_flag <- treat_flag %>%
    add_column(Code = COINobj$data$data_raw$Code_country, .before = 1)

  ###---- Write results -----##
  
  if (is.data.frame(COINobj)){
    return(list(ind_data_treated, treat_flag, ntreated)) # if a data.frame was input, return list
  } else {
    COINobj$data$data_treated <- ind_data_treated
    COINobj$Analysis$Treated_data_flags <- treat_flag
    COINobj$Analysis$n_winz <- ntreated
    return(COINobj)
  }
}