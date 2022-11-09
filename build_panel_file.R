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
# Quality Scores
load("/home/brian/Documents/projects/portfolio_manager_data/qualdf.RData")
# Growth Scores
load("/home/brian/Documents/projects/portfolio_manager_data/growthdf.RData")
# Value Scores
load("/home/brian/Documents/projects/portfolio_manager_data/valuedf.RData")

### Load from mysql ###

### Split returns by date
rbyd <- split(tframe, tframe$date)
rtest <- rbyd[[10000]]

### Convert Factor frame 
test <- split(qual, row.names(qual))

tst <- test[[1]]
qdate <- row.names(tst)[[1]]
tst <- data.frame(t(tst))
names(tst) <- "quality"
tst$ticker <- row.names(tst)
tst$date <- qdate
tst <- tst[,c("date", "ticker", "quality")]





