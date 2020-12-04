#' Plots of correlations
#'
#' Visualisation of correlations between indicators.
#' 
#' @param COINobj A list of indicator data, stuctured using the COIN_assemble function
#' @param inames An optional character vector of indicator codes to plot
#'
#' @examples coin_plotcorr(COINobj)
#'
#' @return A correlation plot.
#'
#' @export
#' 

coin_plotcorr <- function(COINobj, inames = NULL, dset = "raw"){
  
  # First, get relevant bits of the input
  out <- coin_aux_objcheck(COINobj, dset, inames)
  ind_data <- out$ind_data
  ind_data_only <- out$ind_data_only
  ind_names <- out$ind_names
  
  corr_ind <- cor(ind_data_only, method = "pearson", use = "na.or.complete") # get correlation matrix, just indicators
  p_ind <- cor.mtest(ind_data_only, method = "pearson") # p values
  
  # plot indicator correlations (perhaps move this to another function...)
  corrplot.mixed(corr_ind, p.mat = p_ind$p, sig.level = .01, insig = "blank",
                 upper = "ellipse", tl.pos = "lt")
  agspec <- select(COINobj$metadata,starts_with(("Agg_"))) # the columns showing aggregation groups. Need for grouping the plot.
  
  # Now overlay aggregation groups
  if(is.null(inames)){ # only plot rectangles if plot all indicators.
    colourz <- c("black","blue","green","red")
    for (ii in 1:(COINobj$parameters$n_agg_levels-1)){
      groupz <- as.data.frame(table(agspec[,ii])) # this gets a summary of the number of repetitions of each value, so the number of indicators in each agg. group
      corrRect(groupz[,2], col = colourz[ii], lwd = ii) # plot rectangle, changing colour and thickness
    }
  }
}