rm(list=ls())
library(PortfolioAnalytics)
library(DEoptim)
library(ROI)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)

data(edhec)
# Use the first 4 columns in edhec for a returns object
returns <- edhec[, 1:4]
colnames(returns) <- c("CA", "CTAG", "DS", "EM")
print(head(returns, 5))

# Get a character vector of the fund names
fund.names <- colnames(returns)

# Specify a portfolio object by passing a character vector for the
# assets argument.
pspec <- portfolio.spec(assets=fund.names)
print.default(pspec)


# Add the full investment constraint that specifies the weights must sum to 1.
pspec <- add.constraint(portfolio=pspec,
                        type="full_investment")


pspec <- add.constraint(portfolio=pspec,
                        type="box",
                        min=0.05,
                        max=0.4)

pspec <- add.constraint(portfolio=pspec, type="position_limit", max_pos=3)

pspec <- add.objective(portfolio=pspec,
                       type='return',
                       name='mean')


opt_maxret <- optimize.portfolio(R=returns, portfolio=pspec,
                                 optimize_method="ROI",
                                 trace=TRUE)
print(opt_maxret)



rm(list=ls())
library(PortfolioAnalytics)
library(corpcor)

data(edhec)
R <- edhec[, 1:5]
colnames(R) <- c("CA", "CTAG", "DS", "EM", "EQM")
funds <- colnames(R)
#wgts <- data.frame("CA" = .1, "CTAG" = .3, "DS" = .4, "EM" = .1, "EQM" = .1)
wgts <- c(0.1,0.3,0.4,0.1,0.1)
wgts

# Set up portfolio with objectives and constraints
#init.portf <- portfolio.spec(assets=funds,weight_seq = wgts)
init.portf <- portfolio.spec(assets=funds)
#init.portf <- portfolio.spec(weight_seq = wgts) # Does not work with this line added
init.portf <- add.constraint(portfolio = init.portf, type="weight_sum", min_sum = .99, max_sum = 1.01)
init.portf <- add.constraint(portfolio = init.portf, type="long_only")
init.portf <- add.constraint(portfolio = init.portf, type="turnover", turnover_target = 0.1)

# Add an objective to minimize portfolio standard deviation
init.portf <- add.objective(portfolio=init.portf, type="return", name="mean")
print.default(init.portf)

# Solve with DEoptim
#minStdDev.DE <- optimize.portfolio(R=R, portfolio=init.portf, optimize_method="ROI", search_size = 2000)
minStdDev.DE <- optimize.portfolio(R=R, portfolio=init.portf, optimize_method="pso")
minStdDev.DE

# Calculate turnover 
iwts <- rep(1/5, 5)
ewts <- minStdDev.DE$weights

sum(abs(iwts-ewts))/5


# load packages and data
library(quadprog)
library(PortfolioAnalytics)
data(edhec)
dat <- edhec[,1:4]

# add initial weights to initial portfolio
funds <- c("Convertible Arbitrage" = 0.4, "CTA Global" = 0.3, "Distressed Securities" = 0.2, "Emerging Markets" = 0.1)








