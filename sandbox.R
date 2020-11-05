# a first step towards COINr

library("tidyverse")
library("moments") # for calculating skewness, kurtosis, etc
library("reshape2") # helps for manipulating dfs for plotting
library("GGally") # for correlation plots, plotmatrix, ...
library("openxlsx") # for writing to Excel
library("corrplot") # for correlations (for some reason couldn't install Hmisc)

# Read in some test data
data<-read_csv("test_indicators_w_dens.csv")
metad<-read_csv("test_meta.csv")
framewk<-read_csv("test_structure.csv")
denoms <- read_csv("test_denom_noyear.csv")

source("coin_assemble.R")
COINlist <- coin_assemble(data, metad, framewk)

source("coin_preanalyse.R")
COINlist <- coin_preanalyse(COINlist)

source("coin_indicatorplot.R")
coin_indicatorplot(COINlist, type = "Histogram", normalise = FALSE)

source("coin_precorr.R")
coin_precorr(COINlist, inames = c("Fly", "Ship", "Bord"), ntype = "minmax")

COINlist <- coin_denominate(COINlist, specby = "metadata")

source("coin_normalise.R")
COINlist <- coin_normalise(COINlist ,ntype = "minmax")

source("coin_impute.R")
COINlist <- coin_impute(COINlist ,imtype="latest_year")

COINlist <- coin_aggregate(COINlist, agtype="median", dset = "raw")

coin_2excel(COINobj, fname = "COINresults.xlsx")
  