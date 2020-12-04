# a first step towards COINr

library("tidyverse")
library("moments") # for calculating skewness, kurtosis, etc
library("reshape2") # helps for manipulating dfs for plotting
library("GGally") # for correlation plots, plotmatrix, ...
library("openxlsx") # for writing to Excel
library("corrplot") # for correlations (for some reason couldn't install Hmisc)
library("plotly") # for interactive plots
library("matrixStats") # for some matrix operations (product of columns, etc)

# Read in some test data
data<-read_csv("test_indicators_w_dens.csv",col_types = cols())
metad<-read_csv("test_meta.csv",col_types = cols())
framewk<-read_csv("test_structure.csv",col_types = cols())
denoms <- read_csv("test_denom_noyear.csv",col_types = cols())

source("coin_assemble.R")
COINlist <- coin_assemble(data, metad, framewk)

source("coin_preanalyse.R")
COINlist <- coin_preanalyse(COINlist, dset = treated)

source("coin_indicatorplot.R")
coin_indicatorplot(COINlist, type = "Histogram", normalise = FALSE)

source("coin_precorr.R")
coin_precorr(COINlist, inames = c("Fly", "Ship", "Bord"), ntype = "minmax")

COINlist <- coin_denominate(COINlist, specby = "metadata")

source("coin_normalise.R")
COINlist <- coin_normalise(COINlist ,ntype = "minmax")

source("coin_impute.R")
COINlist <- coin_impute(COINlist ,imtype="latest_year")

source("coin_aggregate.R")
COINlist <- coin_aggregate(COINlist, agtype="arith_mean", dset = "raw")

coin_2excel(COINobj, fname = "COINresults.xlsx")
  