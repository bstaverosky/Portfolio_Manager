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

get_returns <- function(symbols){
  lapply(symbols, FUN = function(x){
    print(x)
    df <- getSymbols(x, src = "yahoo", from = "1900-01-01", auto.assign = FALSE)
    df <- df[,4]
    names(df) <- gsub(".Close", "", names(df))
    returns <- Return.calculate(df)
    returns
  })
}


sp500_wiki <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies")

symbols_table <- sp500_wiki %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table()
symbols_table <- symbols_table[[1]]
symbols <- as.character(symbols_table$Symbol)

symbols <- gsub("\\.","-", symbols) 

### DOWNLOAD UNIVERSE PRICES ###
# prices <- lapply(symbols, FUN = function(x){
#   print(x)
#   df <- getSymbols(x, src = "yahoo", from = "1900-01-01", auto.assign = FALSE)
#   df <- df[,4]
#   names(df) <- gsub(".Close", "", names(df))
#   #returns <- Return.calculate(df)
#   #returns
#   df
# })

#save(prices, file = "/home/brian/Documents/projects/portfolio_manager/prices.RData")

load("/home/brian/Documents/projects/portfolio_manager_data/prices.RData")
#write.csv(prices, file="/var/lib/mysql-files/prices.csv")

### CALCULATE DAILY RETURNS ###
returns <- lapply(prices, FUN = function(x){
  print(names(x))
  returns <- Return.calculate(x)
  returns
})

### Make Return DataFrame
retdf <- do.call("cbind", returns)

### SAVE TO MYSQL SERVER ###
#write.csv(retdf, file="/home/brian/Documents/projects/portfolio_manager_data/returns.csv")


##### GET FUNDAMENTAL INDEX RETURNS #####

fbase_symbols <- c("SPHQ", "RPG", "RPV")
fbase <- get_returns(fbase_symbols)
#fbase <- do.call("cbind", fbase)

### GET BENCHMARK RETURNS ###
bmk <- get_returns("SPY")[[1]]

# Create active return dataframe

actret <- lapply(returns, FUN = function(x){
  df <- merge(x, bmk)
  df <- df[complete.cases(df),]
  df[,1] <- df[,1]-df[,2]
  df[,1]
})

# get active factor returns

fret <- lapply(fbase, FUN = function(x){
  df <- merge(x, bmk)
  df <- df[complete.cases(df),]
  df[,1] <- df[,1]-df[,2]
  df[,1]
})

# get correlations

# Make function that takes xts of factor return and xts of stock return
# and returns a data frame of rolling correlations

x <- fret[[1]]
y <- actret[[1]]




roll_cor <- function(x, y){
  df <- merge(x, y)
  df <- df[complete.cases(df),]
  olst <- list()
  if(nrow(df)>253){
    for (j in 253:nrow(df)){
      #print(j)
      jdate <- index(df)[[j]]
      subf <- df[((j-252):j),]
      cr   <- cor(subf[,1],subf[,2])
      
      outdf <- data.frame(correlation = cr)
      names(outdf) <- names(df)[[2]]
      row.names(outdf) <- jdate
      olst[[jdate]] <- outdf
    }
  } else {
    jdate <- tail(index(df),1)
    outdf <- data.frame(correlation = NA)
    row.names(outdf) <- jdate
    olst[[jdate]] <- outdf
  }
  outdf <- do.call("rbind", olst)
  outdf
}


# Quality Scores
olist <- list()
x <- fret[[1]]
  for(i in actret){
    print(names(i))
    outp  <- roll_cor(x, i)
    olist[[names(i)]] <- outp
  }
  
save(olist, file = "/home/brian/Documents/projects/portfolio_manager_data/qualcor.RData")


maxrow <- lapply(olist, )

qual <- plyr::ldply(olist, rbind)

qual <- do.call("cbind", olist)
  
  
 







lapply(actret, FUN = function(x){
  
  df <- merge(x, fbase[[1]])
  
  
  
})




