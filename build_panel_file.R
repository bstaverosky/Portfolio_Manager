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


mobs <- min(sapply(list(growth,qual,value), FUN = function(x){nrow(x)}))


### Load from mysql ###

### CONVERT TO PANELDAT ###

retpanel  <- split(tframe, tframe$date)
retpanel  <- retpanel[(length(retpanel)-(mobs-1)):length(retpanel)]
qualpanel <- convert_to_panel(qual, "quality")
qualpanel  <- qualpanel[(length(qualpanel)-(mobs-1)):length(qualpanel)]
growpanel <- convert_to_panel(growth, "growth")
valpanel  <- convert_to_panel(value, "value")


### MERGE ALL PANELS ###

panel_list <- list()
for (i in 1:mobs){
  print(i)
  q <- qualpanel[[i]]
  g <- growpanel[[i]]
  v <- valpanel[[i]]
  r <- retpanel[[i]]
  dflist <- list(q,g,v,r)
  out <- Reduce(function(x, y) merge(x, y, by = intersect(names(x),names(y))), dflist)
  panel_list[[i]] <- out
}









### CONVERT RAW TO PANEL FORM ###
convert_to_panel <- function(fdata, fname){
  if(fname=="dreturn"){
    y <- split(fdata, fdata$date)
  } else {
    y <- split(fdata, row.names(fdata))
  }
  plist <- lapply(y, FUN = function(x){
    #x <- y[[1]]
    qdate <- row.names(x)[[1]]
    x <- data.frame(t(x))
    names(x) <- fname
    x$ticker <- row.names(x)
    x$date <- qdate
    x <- x[,c("date", "ticker", fname)]
    x <- x[!x$ticker=="correlation",]
    x
  })
  plist
}




