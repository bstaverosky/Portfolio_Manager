rm(list=ls())
source("/home/brian/Documents/projects/scripts/adhoc_functions.R")
library(quantmod)
library(PerformanceAnalytics)
library(xts)
library(lubridate)
library(ggplot2)
library(argparse)
library(magrittr)
library(tibble)
library(quantmod)
library(plyr)

### Load from RData ###
#Returns
load("/home/brian/Documents/projects/portfolio_manager_data/tframe.RData")
#Quality Scores
load("/home/brian/Documents/projects/portfolio_manager_data/qualdf.RData")

# Load from mysql



