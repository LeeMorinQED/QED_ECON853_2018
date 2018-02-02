################################################################################
# 
# ECON 853: Time Series Econometrics
# Practice Exercises in Univariate Time Series Modeling
# 
# Lee Morin, Ph.D.
# Adjunct Assistant Professor
# Queen's University
# 
# February 2, 2018
# 
################################################################################
# 
# This program provides examples univariate times series data.
# Choose a series and analyze it for potential ARMA models. 
# Consider a transformation as needed. 
# Estimate a chosen ARMA model.
# 
# 
################################################################################


################################################################################
# Setup Workspace and Load Libraries
################################################################################

# Clear workspace.
rm(list=ls(all=TRUE))

# Load libraries for time series analysis.


# install.packages('timeSeries')
# library(timeSeries)

# install.packages('xts')
# library(xts)

# install.packages('tseries')
# library(tseries)

# install.packages('forecast')
# library(forecast)

# Load library for data sources.
# install.packages('AER')
library(AER)


################################################################################
# Argentina CPI
################################################################################

data("ArgentinaCPI")
summary(ArgentinaCPI)

plot(ArgentinaCPI)
plot(log(ArgentinaCPI))



################################################################################
# US consumption data (1950-1993)
################################################################################


data("USConsump1993")
summary(USConsump1993)

plot(USConsump1993[, 'income'])
plot(USConsump1993[, 'expenditure'])



################################################################################
# Inflation, Growth and Stock Returns, 1952-1982
################################################################################

data("BenderlyZwick")
summary(BenderlyZwick)




################################################################################
# Bond Yield Data
################################################################################

data("BondYield")
summary(BondYield)
plot(BondYield)


################################################################################
# Grain Cartel Stability
################################################################################


data("CartelStability")
summary(CartelStability)



################################################################################
# Chinese Real National Income Data
################################################################################


data("ChinaIncome")
summary(ChinaIncome)



################################################################################
# Properties of a Fast-Moving Consumer Good
################################################################################

data("ConsumerGood")
summary(ConsumerGood)

plot(ConsumerGood)



################################################################################
# Dow Jones Index Data
################################################################################


data("DJFranses")
summary(DJFranses)


plot(DJFranses)


################################################################################
# Dow Jones Industrial Average (DJIA) index
################################################################################


data("DJIA8012")


summary(DJIA8012)
plot(DJIA8012)




################################################################################
# TV and Radio Advertising Expenditures Data
################################################################################

data("DutchAdvert")

summary(DutchAdvert)
plot(DutchAdvert)


################################################################################
# Dutch Retail Sales Index Data
################################################################################


data("DutchSales")

summary(DutchSales)
plot(DutchSales)






################################################################################
# Price of Frozen Orange Juice
################################################################################

data("FrozenJuice")

summary(FrozenJuice)
plot(FrozenJuice)


################################################################################
# Unemployment in Germany
################################################################################


data("GermanUnemployment")

summary(GermanUnemployment)
plot(GermanUnemployment)




################################################################################
# Gold and Silver Prices
################################################################################


data("GoldSilver")

summary(GoldSilver)
plot(GoldSilver)



################################################################################
# DEM/USD Exchange Rate Returns
################################################################################


data("MarkDollar")

summary(MarkDollar)
plot(MarkDollar)



################################################################################
# DEM/GBP Exchange Rate Returns
################################################################################


data("MarkPound")

summary(MarkPound)
plot(MarkPound)



################################################################################
# MSCI Switzerland Index
################################################################################


data("MSCISwitzerland")

summary(MSCISwitzerland)
plot(MSCISwitzerland)



################################################################################
# Daily NYSE Composite Index
################################################################################


data("NYSESW")

summary(NYSESW)
plot(NYSESW)



################################################################################
# Black and White Pepper Prices
################################################################################


data("PepperPrice")

summary(PepperPrice)
plot(PepperPrice)



################################################################################
# Monthly US Macroeconomic Data (1947-2004, Stock \& Watson)
################################################################################


data("USMacroSWM")

summary(USMacroSWM)
plot(USMacroSWM)



################################################################################
# USMoney
################################################################################


data("USMoney")

summary(USMoney)
plot(USMoney)



################################################################################
# Monthly US Stock Returns (1931-2002, Stock \& Watson)
################################################################################


data("USStocksSW")

summary(USStocksSW)
plot(USStocksSW)




################################################################################
# End
################################################################################

