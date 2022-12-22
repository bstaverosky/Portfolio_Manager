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


### CREATE INITIAL PORTFOLIO ###
# Create limited turnover portfolio
#init <- T
assets <- outdf[1,]
port_spec <- portfolio.spec(names(assets))
# Add the weight sum constaint
port_spec <- add.constraint(portfolio = port_spec, 
                            type = "full_investment")
# Add long only constraint
#port_spec <- add.constraint(portfolio = port_spec, 
#                            type = "long_only")

# Position Size Constraint
port_spec <- add.constraint(portfolio = port_spec,
                            type = "box", 
                            min = 0.01,
                            max = 0.01)


#Position Limit Constraint
port_spec <- add.constraint(portfolio = port_spec,
                            type = "position_limit",
                            max_pos = 100)

# Add return objective 
port_spec <- add.objective(portfolio = port_spec, type = "return", name = "mean")

# Single Optim

returns <- assets
opt_single <- optimize.portfolio(R = returns,
                                 portfolio = port_spec,
                                 optimize_method = "ROI",
                                 trace=TRUE)

print(opt_single)

### CREATE NEXT PORTFOLIO WITH A TURNOVER CONSTRAINT ###

port_spec <- portfolio.spec(assets = names(outdf[2,]), weight_seq = opt_single$weights)

# Add the weight sum constaint
port_spec <- add.constraint(portfolio = port_spec, 
                            type = "full_investment")

port_spec <- add.constraint(portfolio=port_spec, type="turnover", turnover_target=0.006)

# Position Size Constraint
port_spec <- add.constraint(portfolio = port_spec,
                            type = "box", 
                            min = 0.01,
                            max = 0.01)


#Position Limit Constraint
port_spec <- add.constraint(portfolio = port_spec,
                            type = "position_limit",
                            max_pos = 100)

# Add return objective 
port_spec <- add.objective(portfolio = port_spec, type = "return", name = "mean")

opt_next <- optimize.portfolio(R = outdf[3000,],
                                 portfolio = port_spec,
                                 optimize_method = "ROI",
                                 trace=TRUE)

### Manually Calculate Turnover ###

portfolio_turnover(opt_single$weights, opt_next$weights)



### LOAD RETURN DATAFRAME ###
# Make return xts from TFrame
load("/home/brian/Documents/projects/portfolio_manager_data/tframe.RData")
retf <- split(tframe, tframe$ticker)
test <- retf[[1]]
retf <- lapply(retf, FUN = function(x){
  tname <- x$ticker[[1]]
  x  <- xts(x[,"dreturn"], order.by = as.Date(x$date))
  names(x) <- tname
  x
})
retf <- do.call("cbind", retf)
retf <- retf[,names(outdf)]






