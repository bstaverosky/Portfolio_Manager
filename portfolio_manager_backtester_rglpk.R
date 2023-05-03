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
library(Rglpk)

### FUNCTIONS ###
zscore                <- function(x){
  zs <- sapply(x, FUN = function(i){
    (i-mean(x, na.rm = T))/sd(x, na.rm = T)
  })
  zs
}
get_month_ends        <- function(panel_list){
  dends <- unlist(lapply(panel_list, FUN = function(x){x$date[[1]]}))
  ym    <- unique(substr(dends, 1,7))
  mes <- sapply(ym, FUN = function(x){
    max(mends[grep(x, mends)])
  })
}
get_multifactor_score <- function(panel_list){
  panel_with_score <- lapply(panel_list, FUN = function(x){
    x$quality <- zscore(x$quality)
    x$growth  <- zscore(x$growth)
    x$value   <- zscore(x$value)
    x$score   <- rowSums(x[,c("quality","growth","value")])
    x
  })
  save(panel_with_score, file = "/home/brian/Documents/projects/portfolio_manager_data/panel_with_score.RData")
  panel_with_score
}
lag_panel_score       <- function(panel_with_score){
  
  panel_with_score <- do.call("rbind", panel_with_score)
  # make a list of data frames by ticker
  panel_with_score <- split(panel_with_score, panel_with_score$ticker)
  # lag returns for each data frame by one day
  panel_with_score <- lapply(panel_with_score, FUN = function(x){
    x$dreturn <- data.table::shift(x$dreturn, -1)
    x
  })
  panel_with_score <- do.call("rbind", panel_with_score)
  panel_with_score <- split(panel_with_score, panel_with_score$date)
  lagged_panel <- panel_with_score
  lagged_panel
  
}
### Backtest Functions ###
get_unconstrained_equal_weight_strat <- function(daily_portfolios){
  
  retl <- lapply(daily_portfolios, FUN = function(x){
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
  
}

### LOAD PANELDATA ###
### THIS WILL BECOME A SQL DATABASE EVENTUALLY
### AND WILL BE PULLED VIA A SQL QUERY
load("/home/brian/Documents/projects/portfolio_manager_data/panel_list.RData")

# GET MONTH ENDS
get_month_ends(panel_list)

### CREATE PANEL WITH MULTIFACTOR SCORE ###
#panel_with_score <- get_multifactor_score(panel_list)
load("/home/brian/Documents/projects/portfolio_manager_data/spanel.RData")
panel_with_score <- spanel

### LAG THE PANEL WITH MULTIFACTOR SCORE BY ONE DAY ###
lagged_panel <- lag_panel_score(panel_with_score)

### Create Daily Portfolios
daily_portfolios <- lapply(lagged_panel, FUN = function(x){
  x <- head(x[order(-x$score),],100)
  x
})

### Make List of Equal Weighted Strategy Returns
get_unconstrained_equal_weight_strat(daily_portfolios)


### Convert Portfolio List to large xts for PortfolioAnalytics ###

scorel <- lapply(daily_portfolios, FUN = function(x){
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

### CREATE NEXT PORTFOLIO WITH A TURNOVER CONSTRAINT ###
#port_spec <- portfolio.spec(assets = names(outdf[2000,]), weight_seq = opt_single$weights)

turnover_list <- list()
target_list   <- list()
for (i in 2:nrow(outdf)){
  print(i)
  dte <- row.names(outdf[i,])
  ##### STARTING PORTFOLIO #####
  # starting universe as a single row from outdf
  univ <- outdf[i-1,]
  univ[is.na(univ)] <- 0
  
  # Over/Underweight Limit
  ouweight <- 0.002
  
  # create equal weighted portfolio
  wts <- univ
  wts[which(univ > 0)] <- 1/length(wts[which(univ > 0)])
  minv <- as.numeric(wts - ouweight)
  minv[minv<0] <- 0
  maxv <- as.numeric(ifelse(wts==0, 0, wts + ouweight))
  
  # Define the expected return
  expected_returns <- univ
  minv <- minv
  maxv <- maxv
  
  # Define the objective function to maximize portfolio expected return
  obj_fun <- -expected_returns
  
  # Define the constraint matrix and right-hand side vector
  A <- rbind(diag(length(expected_returns)),diag(length(expected_returns)), rep(1, length(expected_returns)))
  rhs <- c(minv, maxv, 1)
  dir <- c(rep(">=", length(expected_returns)), rep("<=", length(expected_returns)), "==")
  
  # Define the variable type (continuous)
  type <- rep("C", length(expected_returns))
  
  # Use RGLPK to find the optimal portfolio weights
  solution <- Rglpk_solve_LP(obj = obj_fun, 
                             mat = A, 
                             dir = dir, 
                             rhs = rhs, 
                             #bounds = list(upper = maxv, lower = minv),
                             types = type,
                             max = TRUE)
  
  # Print the optimal portfolio weights
  #cat("Optimal portfolio weights:", solution$solution)
  
  length(which(solution$solution > 0))
  sum(solution$solution)
  
  begin_portfolio <- solution$solution
  
  ##### PORTFOLIO # 2 #####
  
  univ <- outdf[i,]
  univ[is.na(univ)] <- 0
  
  ouweight <- 0.002
  # create equal weighted portfolio
  wts <- univ
  wts[which(univ > 0)] <- 1/length(wts[which(univ > 0)])
  minv <- as.numeric(wts - ouweight)
  minv[minv<0] <- 0
  maxv <- as.numeric(ifelse(wts==0, 0, wts + ouweight))
  
  # Define the expected return
  expected_returns <- univ
  minv <- minv
  maxv <- maxv
  
  # Define the objective function to maximize portfolio expected return
  obj_fun <- -expected_returns
  
  # Define the constraint matrix and right-hand side vector
  A <- rbind(diag(length(expected_returns)),diag(length(expected_returns)), rep(1, length(expected_returns)))
  rhs <- c(minv, maxv, 1)
  dir <- c(rep(">=", length(expected_returns)), rep("<=", length(expected_returns)), "==")
  
  # Define the variable type (continuous)
  type <- rep("C", length(expected_returns))
  
  # Use RGLPK to find the optimal portfolio weights
  solution <- Rglpk_solve_LP(obj = obj_fun, 
                             mat = A, 
                             dir = dir, 
                             rhs = rhs, 
                             #bounds = list(upper = maxv, lower = minv),
                             types = type,
                             max = TRUE)
  
  # Print the optimal portfolio weights
  #cat("Optimal portfolio weights:", solution$solution)
  
  next_portfolio <- solution$solution
  
  turnover <- sum(abs(next_portfolio-begin_portfolio))/2
  target   <- next_portfolio
  target   <- as.data.frame(t(as.data.frame(target)))
  names(target)     <- names(univ)
  row.names(target) <- dte
  
  target_list[[i]]   <- target
  turnover_list[[i]] <- turnover
}

save(target_list, file = "/home/brian/Documents/projects/portfolio_manager_data/target_list.RData")
save(turnover_list, file = "/home/brian/Documents/projects/portfolio_manager_data/turnover_list.RData")

load("/home/brian/Documents/projects/portfolio_manager_data/target_list.RData")

#### LOAD RETURN DATAFRAME #####

load("/home/brian/Documents/projects/portfolio_manager_data/prices.RData")

### Make Return DataFrame
returns <- pricetoreturn(prices)
retdf   <- do.call("cbind", returns)

### MULTIPLY WEIGHTS BY STOCK RETURNS TO GET PORTFOLIO RETURN ###

target_list <- Filter(Negate(is.null), target_list)

return_list <- list()
for (i in 1:length(target_list)){
  wts <- target_list[[i]]
  dte <- row.names(wts)
  period_returns        <- retdf[which(index(retdf)==dte),]
  sorted_period_returns <- period_returns[,sort(intersect(names(period_returns), names(wts)))]
  period_wts            <- wts[,sort(intersect(names(sorted_period_returns), names(wts)))]
  period_wts            <- as.xts(period_wts, order.by = as.Date(row.names(period_wts)))
  
  port_return <- sum(period_wts * sorted_period_returns, na.rm = T)
  port_return <- data.frame(Date = dte, Return = port_return)
  return_list[[i]] <- port_return 
}

strat <- do.call("rbind", return_list)
strat <- as.xts(strat[,2], order.by = as.Date(strat$Date))

charts.PerformanceSummary(strat)






