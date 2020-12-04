#' Plot index structure
#'
#' Plots the structure of the index using a sunburst plot
#'
#' @param data COIN object, or a list of two data frames: first data frame should be a metadata data frame,
#' and the second should be the aggregation weights. In the same format as coin_assemble.
#'
#' @examples coin_plotframework(COINobj)
#'
#' @return Interactive sunburst plot.
#'
#' @export

coin_plotframework <- function(COINobj){
  
  # the structure is in the metadata and framework data frames
  if ("COIN object" %in% class(COINobj)){ # COIN obj
    
    metad<-COINobj$metadata
    fwk <- COINobj$framework
    
  } else if ("list" %in% class(COINobj) & length(COINobj)==2) { # we expect a list with 2 elements in it
    
    metad<-COINobj[[1]]
    fwk<-COINobj[[2]]
    
  } else { # no clue what it is
    
    stop("Input data type not recognised. Should be COIN object or list of indmeta/aggmeta data frames.")
  }
  
  # isolate columns with aggregation labels in them, and indictaor labels
  agg_cols <- metad %>% select(starts_with("Agg_"))
  agg_cols <- agg_cols %>% add_column(Code=metad$Code, .before = 1)
  # get weights for aggregation groups
  wt_cols <- fwk %>% select(ends_with("weight"))
  
  # preallocate some variables
  lbls <- NULL
  prnts <- NULL
  wts <- NULL
  for (ii in 1:ncol(agg_cols)){
    
    # add labels
    lbls <- c(lbls,pull(agg_cols,ii))
    
    # add weights, but adjust so sum to 1 inside their aggregation group. Slightly fiddly.
    
    if (ii==1){ # indicators
      
      wcol <- metad$Ind_weight # the indicator weights
      prnt <- pull(agg_cols,ii+1) # the column specifying parent agg groups
      prntu <- unique(prnt) # the parent agg groups, duplicates removed
      
      # now loop over aggregation groups
      wts_level <- NULL # preallocating for just the weights in the level
      for (jj in 1:length(prntu)){
        wgroup <- wcol[prnt == prntu[jj]] # only the weights in the agg group
        wts_level <- c(wts_level,wgroup/sum(wgroup, na.rm = T)) # normalise to 1 and add to weight vector
      }
      wts <- c(wts,wts_level) # have to make sure weights add up to 1 at every level
      
    } else { # aggregation groups
      
      wcol <- pull(wt_cols,ii-1) # the aggregation level weights
      wcol <- wcol[!is.na(wcol)]
      agg_col <- pull(agg_cols,ii) # the column with the aggregation group labels in
      if (ii<ncol(agg_cols)){
        prnt <- pull(agg_cols,ii+1) # the column specifying parent agg groups
      } else {
        prnt <- rep(1,length(agg_col)) # if we reached the root, use a dummy vector
      }
      
      chld_prnt <- unique(data.frame(agg_col,prnt)) # put together and remove duplicates
      
      prntu <- unique(prnt) # the parent agg groups, duplicates removed
      
      wts_level <- NULL # preallocating for just the weights in the level
      for (jj in 1:length(prntu)){
        wgroup <- wcol[chld_prnt$prnt == prntu[jj]] # only the weights in the agg group
        wts_level <- c(wts_level,wgroup/sum(wgroup, na.rm = T)) # normalise to 1 and add to weight vector
      }
      wts <- c(wts,wts_level) # have to make sure weights add up to 1 at every level
      
    }
    
    # now we have to go back through again to get the effective weights in the index...
    wts_eff <- NULL # preallocate
    for (jj in 1:ncol(agg_cols)){
      # get level plus all parent levels
      pcols <- select(agg_cols,jj:ncol(agg_cols)) %>% unique
      # sust names for weights of each element
      pcols_w <- map_dfr(pcols,~wts[match(.x,unique(lbls))])
      wts_eff <- c(wts_eff,rowProds(as.matrix(pcols_w)))
    }
    
    # add parents
    if (ii==ncol(agg_cols)){
      prnts <- c(prnts,rep("",nrow(agg_cols)))
    } else {
      prnts<-c(prnts,pull(agg_cols,ii+1))
    }
  }
  
  lbls_prnts <- data.frame(lbls,prnts) %>% unique() # remove repeated rows
  #wts <- wts[!is.na(wts)] # remove NAs
  
  fig <- plot_ly(
    labels = lbls_prnts$lbls, parents = lbls_prnts$prnts, values = wts_eff,
    type = 'sunburst',
    branchvalues = 'total'
  )
  
  fig
  
}

