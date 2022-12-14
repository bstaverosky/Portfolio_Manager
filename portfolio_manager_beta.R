rm(list=ls())
library(quantmod)
library(PerformanceAnalytics)
library(xts)
library(lubridate)
library(knitr)
library(kableExtra)
library(ggplot2)
library(ggthemes)
library(xtable)
source("/home/brian/Documents/projects/scripts/adhoc_functions.R")
library(argparse)
library(rvest)
library(magrittr)
library(tibble)
library(quantmod)
library(plyr)

# MOD 11/8/2022

spy_data <- get_sp500_data()
symbols  <- spy_data$Symbol
prices   <- get_closing_prices(symbols)
#save(prices, file = "/home/brian/Documents/projects/portfolio_manager/prices.RData")
load("/home/brian/Documents/projects/portfolio_manager_data/prices.RData")
#write.csv(prices, file="/var/lib/mysql-files/prices.csv")

returns <- pricetoreturn(prices)
### Make Return DataFrame
retdf   <- do.call("cbind", returns)

# Transform RETDF to dataframe export as csv for MYSQL server
# COLUMNS AS date, ticker and dreturn

test <- data.frame(retdf)
test$date <- as.Date(row.names(test))

tsplit <- split(test, test$date)

tmod <- lapply(tsplit, FUN = function(x){
  pdate <- row.names(x)
  x$date <- NULL
  x <- data.frame(t(x))
  x$date <- pdate
  x$ticker <- row.names(x)
  names(x) <- c("dreturn", "date", "ticker")
  x <- x[,c("date", "ticker", "dreturn")]
  x
})

tframe <- do.call("rbind", tmod)
save(tframe, file = "/home/brian/Documents/projects/portfolio_manager_data/tframe.RData")
write.csv(tframe, file = "/home/brian/Documents/projects/portfolio_manager_data/returnsmysql.csv",row.names = F)

##### GET FUNDAMENTAL INDEX RETURNS #####
fbase_symbols <- c("SPHQ", "RPG", "RPV")
fbase         <- get_returns(fbase_symbols)
#fbase <- do.call("cbind", fbase)

### GET BENCHMARK RETURNS ###
bmk <- get_returns("SPY")[[1]]

# List of active returns
actret <- get_act_ret(returns, bmk)

### GET ACTIVE FACTOR RETURNS ###
fret <- get_act_ret(fbase, bmk)

### GET QUALITY CORRELATIONS ###

# olist <- list()
# x <- fret[[1]]
#   for(i in actret){
#     print(names(i))
#     outp  <- roll_cor(x, i)
#     olist[[names(i)]] <- outp
#   }
#   
#save(olist, file = "/home/brian/Documents/projects/portfolio_manager_data/qualcor.RData")
load("/home/brian/Documents/projects/portfolio_manager_data/qualcor.RData")

# Top Scoring Quality Names
tqual <- tail(qual,1)
tqual <- t(tqual)

##### CREATE A FUNCTION FOR CBINDING LIST OF DATAFRAMES WITH DIFFERENT LENGTHS #####

dflist_out <- cbind_diff_frames(olist)
qual       <- do.call("cbind",dflist_out)
save(qual, file = "/home/brian/Documents/projects/portfolio_manager_data/qualdf.RData")
load("/home/brian/Documents/projects/portfolio_manager_data/qualdf.RData")

##### GET GROWTH CORRELATIONS #####

glist <- list()
x <- fret[[2]]
  for(i in actret){
    print(names(i))
    outp  <- roll_cor(x, i, window = 253)
    glist[[names(i)]] <- outp
  }

save(glist, file = "/home/brian/Documents/projects/portfolio_manager_data/growth_cor_list.RData")

dflist_out <- cbind_diff_frames(glist)
growth     <- do.call("cbind", dflist_out)
save(growth, file = "/home/brian/Documents/projects/portfolio_manager_data/growthdf.RData")
load("/home/brian/Documents/projects/portfolio_manager_data/growthdf.RData")

# Top Scoring Growth Names
test  <- tail(growth, 1)
test2 <- t(test) 

##### GET VALUE CORRELATIONS #####

vlist <- list()
x <- fret[[3]]
for(i in actret){
  print(names(i))
  outp  <- roll_cor(x, i, window = 253)
  vlist[[names(i)]] <- outp
}

vlist_out <- cbind_diff_frames(vlist)
value     <- do.call("cbind", vlist_out)
save(value, file = "/home/brian/Documents/projects/portfolio_manager_data/valuedf.RData")
load("/home/brian/Documents/projects/portfolio_manager_data/valuedf.RData") 

# Top Scoring Value Names
test <- tail(value, 1)
head(test[order(-test)],10)

### Create Paneldata file








