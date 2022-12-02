rm(list=ls())
source("/home/brian/Documents/projects/scripts/adhoc_functions.R")
library(quantmod)
library(PerformanceAnalytics)
library(xts)
library(zoo)
library(lubridate)
library(ggplot2)
library(argparse)
library(magrittr)
library(tibble)
library(quantmod)
library(plyr)
library(data.table)

zscore <- function(x){
  zs <- sapply(x, FUN = function(i){
    (i-mean(x, na.rm = T))/sd(x, na.rm = T)
  })
  zs
}

### LOAD PANELDATA ###

load("/home/brian/Documents/projects/portfolio_manager_data/panel_list.RData")


# Get Month ends 
mends <- unlist(lapply(panel_list, FUN = function(x){x$date[[1]]}))
ym    <- unique(substr(mends, 1,7))

mes <- sapply(ym, FUN = function(x){
  max(mends[grep(x, mends)])
})

### Create Multifactor Score ###

spanel <- lapply(panel_list, FUN = function(x){
  x$quality <- zscore(x$quality)
  x$growth  <- zscore(x$growth)
  x$value   <- zscore(x$value)
  x$score   <- rowSums(x[,c("quality","growth","value")])
  x
})

save(spanel, file = "/home/brian/Documents/projects/portfolio_manager_data/spanel.RData")
load("/home/brian/Documents/projects/portfolio_manager_data/spanel.RData")

### SPANEL LAG DRETURN BY ONE DAY ###

spanel <- do.call("rbind", spanel)
spanel <- split(spanel, spanel$ticker)

spanel <- lapply(spanel, FUN = function(x){
  x$dreturn <- data.table::shift(x$dreturn, -1)
  x
})
spanel <- do.call("rbind", spanel)
spanel <- split(spanel, spanel$date)

### Create Daily Portfolios
portl <- lapply(spanel, FUN = function(x){
  x <- head(x[order(-x$score),],100)
  x
})

retl <- lapply(portl, FUN = function(x){
  return      <- sum(x$dreturn * (1/100))
  outf        <- data.frame(date = x$date[[1]], strat = as.numeric(return))
  outf$strat  <- as.numeric(outf$strat)
  outf        <- xts(outf[,"strat"], order.by = as.Date(x$date[[1]]))
  names(outf) <- "strat" 
  outf
})

### GET BMK ###

bmk <- "SPY"
bmk <- getSymbols(bmk, src = "yahoo", from = "1900-01-01", auto.assign = FALSE)
bmk <- bmk[,4]
bmk <- dailyReturn(bmk)


strat <- do.call("rbind", retl)
strat <- merge(strat, bmk)
strat <- strat[complete.cases(strat),]


charts.PerformanceSummary(strat)

stratm <- strat["2020/"]
  
charts.PerformanceSummary(stratm)

