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






