#HDFC Capital Builder Value Fund- Portfolio from HDFC MF Sheet - October 2023
#__________________________________________________________________________________
# By Group- 
# Hrithik Prakash Bokade  - 22020845015
# Pranav Mehta            - 22020845024
# Arnav Chauhan           - 22020845009  

#######################################################################################

# Required packages

library(tidyverse)
library(tidyquant)
setwd("E:/Hrithik Bokade/SIBM Bangluru MBA -BA Study Material/Subject Study Material/Semester- IV/Financial Ecometrics (FE)/Portfolio Optimization")

#To Calculate stock returns
# Stocks Adjusted Close taken from 1st April 2022 to 31st March 2023
stock_data <- data.frame(c("ICICIBANK.NS","AXISBANK.NS","INFY.NS","BHARTIARTL.NS",
                              "ITC.NS","COALINDIA.NS","CIPLA.NS","NTPC.NS","TECHM.NS",
                              "HCLTECH.NS") %>%
                              tq_get(get="stock.prices",
                                     from="2022-04-01",
                                     to="2023-03-31") %>%
                              group_by(symbol)%>%
                              tq_transmute(select = adjusted,
                                           mutate_fun = periodReturn,
                                           period="daily",
                                           type="log",
                                           col_rename = "Ra"))

write.csv(stock_data,"stock_data.csv")

# Pivot table function used in excel for "stock_data.csv", renamed as "stock_data1.csv"
stock_data1 <- read.csv("stock_data1.csv", header=TRUE)
print(stock_data1)

# Converting the data into time series object
library(tseries)
stock_data1_ts <- ts(stock_data1)

# Converting the data into extensible time series format
stock_data1_xts <- as.xts(stock_data1_ts)
str(stock_data1_xts)

# Creating a portfolio data-set
stock_data1 <- stock_data1_xts[,2:11]  #Dropping the date column
print(stock_data1)

# Visualize the trend in return
chart.CumReturns(stock_data1, main="Cumulative returns",
                 legend.loc="topleft", wealth.index=TRUE)

# Downside Risk
chart.Drawdown(stock_data1, legend.loc="bottomright")

# Generating the Downside_risk in table form
stock_data1_downside_risk <- data.frame(table.DownsideRisk(stock_data1, p=.95))
write.csv(stock_data1_downside_risk,"stock_data1_downside_risk.csv")

chart.Correlation(histogram= TRUE,stock_data1)
chart.Correlation(histogram= FALSE,stock_data1)
# HCLTECH.NS & INFY.NS - highly correlated with the value of 0.77 in correlation matrix

chart.Correlation(stock_data1_downside_risk)
chart.Correlation(histogram= FALSE,stock_data1_downside_risk)

# Check for Outliers in the stock return
chart.Boxplot(stock_data1, sort.by = "variance")

# Descriptive Statistics
stock_data1_des_stat=table.Distributions(stock_data1)
stock_data1_des1= table.Stats(stock_data1)


library(PortfolioAnalytics)
# Creating a portfolio object
my_stock_portfolio  <- portfolio.spec(colnames(stock_data1))

# To add constraints to the portfolio
my_stock_portfolio <- add.constraint(portfolio = my_stock_portfolio, 
                               type = "full_investment")
my_stock_portfolio <- add.constraint(portfolio = my_stock_portfolio, 
                               type = "long_only")


# To add objectives to the portfolio 
my_stock_portfolio <- add.objective(portfolio = my_stock_portfolio,
                              type = "return", name="mean")

my_stock_portfolio <- add.objective(portfolio = my_stock_portfolio,
                              type = "risk", name="StdDev")
print(my_stock_portfolio)

# To Optimize the portfolio
opt <- optimize.portfolio(stock_data1, portfolio = my_stock_portfolio,
                          optimize_method = "random", trace=TRUE)
print(opt)

chart.RiskReward(opt, risk.col = "StdDev",return.col = "mean")

# To create efficient frontier 
stock_data1_efficient_options <- create.EfficientFrontier(stock_data1,portfolio = my_stock_portfolio,
                                              n.portfolios = 50,match.col = "mean",
                                              type = "random", search_size = 2000)
# more the search size more the return (closer to the n.portfolios)
summary(stock_data1_efficient_options)
