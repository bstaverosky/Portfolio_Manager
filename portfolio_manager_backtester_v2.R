rm(list=ls())
source("/home/brian/Documents/projects/scripts/adhoc_functions.R")
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
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
library(DEoptim)
library(quadprog)
library(Rglpk)

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

# spanel <- lapply(panel_list, FUN = function(x){
#   x$quality <- zscore(x$quality)
#   x$growth  <- zscore(x$growth)
#   x$value   <- zscore(x$value)
#   x$score   <- rowSums(x[,c("quality","growth","value")])
#   x
# })

#save(spanel, file = "/home/brian/Documents/projects/portfolio_manager_data/spanel.RData")
load("/home/brian/Documents/projects/portfolio_manager_data/spanel.RData")

### SPANEL LAG DRETURN BY ONE DAY ###
spanel <- do.call("rbind", spanel)
# make a list of data frames by ticker
spanel <- split(spanel, spanel$ticker)
# lag returns for each data frame by one day
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
# Make Return List
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
stratm <- strat["2018/"]
charts.PerformanceSummary(stratm)

### Convert Portfolio List to large xts for PortfolioAnalytics ###

scorel <- lapply(portl, FUN = function(x){
  df <- t(data.frame(x$score))
  df <- as.data.frame(df)
  names(df) <- x$ticker
  row.names(df) <- x$date[[1]]
  df
})

# old <- scorel[[1]]
# for (i in 2:length(scorel)){
#   new <- scorel[[i]]
#   newadds <- setdiff(names(new), names(old))
#   for (x in newadds){
#     old[,x] <- NA
#   }
#   oldadds <- setdiff(names(old), names(new))
#   for (x in oldadds){
#     new[,x] <- NA
#   }
#   outdf <- rbind(old,new)
#   old <- outdf
# }

#save(outdf, file = "/home/brian/Documents/projects/portfolio_manager_data/outdf.RData")
load("/home/brian/Documents/projects/portfolio_manager_data/outdf.RData")

# starting universe as a single row from outdf
univ <- outdf[2000,]
univ[is.na(univ)] <- 0

# create equal weighted portfolio
wts <- univ
wts[which(univ > 0)] <- 1/length(wts[which(univ > 0)])


# Define the optimization problem
stock_returns      <- as.numeric(univ[1,]) # vector of stock returns
stock_positions    <- wts # vector of initial position sizes
portfolio_turnover <- 0.1 # maximum allowable turnover for the entire portfolio

# Minimum and maximum position size
min_position_size <- 0.01 # minimum position size for any stock
max_position_size <- 0.05 # maximum position size for any stock

# Define the objective function
f <- -stock_returns

# Define the constraints
# Position size constraints
A <- diag(stock_positions)
b <- rep(1, length(stock_positions)) # maximum portfolio size is 1

# Turnover constraint
Aeq <- rep(1, length(stock_positions))
beq <- portfolio_turnover # maximum turnover is 0.1

# Minimum and maximum position size constraints
lb <- rep(min_position_size, length(stock_positions)) # lower bound
ub <- rep(max_position_size, length(stock_positions)) # upper bound

# Run the optimization
solution <- solve.QP(Dmat = diag(length(stock_returns)), dvec = f, 
                     Amat = A, bvec = b, Aeq = Aeq, beq = beq,
                     lb = lb, ub = ub)

# The optimized position sizes are in the "solution$solution" vector
optimized_positions <- solution$solution


