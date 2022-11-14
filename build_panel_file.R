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
library(RMySQL)
library(DBI)

### CONNECT TO MYSQL DATABASE ###

mysqlconnection = dbConnect(RMySQL::MySQL(),
                            dbname='returns',
                            host='localhost',
                            port=3306,
                            user=rstudioapi::askForPassword("Database user"),
                            password=rstudioapi::askForPassword("Database password")
)


mysqlconnection = dbConnect(RMySQL::MySQL(),
                            dbname='returns',
                            host='localhost',
                            port=3306,
                            user='root',
                            password='M@uricio24'
                            )

### Load from RData ###
#Returns
load("/home/brian/Documents/projects/portfolio_manager_data/tframe.RData")
# Quality Scores
load("/home/brian/Documents/projects/portfolio_manager_data/qualdf.RData")
# Growth Scores
load("/home/brian/Documents/projects/portfolio_manager_data/growthdf.RData")
# Value Scores
load("/home/brian/Documents/projects/portfolio_manager_data/valuedf.RData")

# Maximum number of observations
mobs <- min(sapply(list(growth,qual,value), FUN = function(x){nrow(x)}))

### Load from mysql ###

### CONVERT TO PANELDATA ###
retpanel  <- convert_to_panel(tframe, "dreturn", mobs)
qualpanel <- convert_to_panel(qual, "quality", mobs)
growpanel <- convert_to_panel(growth, "growth", mobs)
valpanel  <- convert_to_panel(value, "value", mobs)

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

save(panel_list, file = "/home/brian/Documents/projects/portfolio_manager_data/panel_list.RData")
### CONVERT THE ABOVE INTO A FUNCTION ###
#merge_panels <- function(plist){
#}











