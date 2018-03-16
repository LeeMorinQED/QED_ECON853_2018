################################################################################
# 
# ECON 853: Time Series Econometrics
# Seasonality
# 
# Lee Morin, Ph.D.
# Adjunct Assistant Professor
# Queen's University
# 
# March 14, 2018
# 
################################################################################
# 
# This program provides examples of seasonality.
# Reference: Using R for Time Series Analysis
# http://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html
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

# install.packages('FitARMA')
# library(FitARMA)

# Load libraries designed for nonstationary time series.
# install.packages('urca')
# library(urca)

# Load library designed for fractionally integrated (long memory) processes.
# install.packages('arfima')
# library(arfima)

# Library designed to measure seasonality.
# install.packages('x12')
library(x12)

# Another package for 
# install.packages('x12')
library('TTR')



# Set working directory.
wd_path <- 'C:/Users/iky155/Documents/Repos/ECON853w2018_repo_2'
# wdPath <- '/path/to/your/folder'
setwd(wd_path)


# Set path for S&P 500 dataset.
data_path <- sprintf('%s/Data', wd_path)



################################################################################
# Load Sample datasets
################################################################################

#--------------------------------------------------------------------------------
# Sample dataset of births in New York City.
#--------------------------------------------------------------------------------

# This dataset contains the number of births per month in New York city, 
# from January 1946 to December 1959 (originally collected by Newton). 
# This data is available in the file http://robjhyndman.com/tsdldata/data/nybirths.dat 
# We can read the data into R, and store it as a time series object, by typing:

# Read the data into R directly from the online source. 
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
# Translate this to a time series object. 
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
# Display the entire dataset to screen. 
birthstimeseries

#--------------------------------------------------------------------------------
# Sample dataset of souvenir sales in Queensland, Australia.
#--------------------------------------------------------------------------------

# The file http://robjhyndman.com/tsdldata/data/fancy.dat contains monthly sales 
# for a souvenir shop at a beach resort town in Queensland, Australia, 
# for January 1987-December 1993 (original data from Wheelwright and Hyndman, 1998). 
  
# Read the data into R directly from the online source. 
souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
# Translate this to a time series object. 
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))
# Display the entire dataset to screen. 
souvenirtimeseries


#--------------------------------------------------------------------------------
# Load air passenger data.
# Optional, since it is a much more involved example of seasonality adjustments. 
#--------------------------------------------------------------------------------

data(AirPassengersX12)

summary(AirPassengersX12)
# summary(AirPassengersX12,oldOutput=10)



################################################################################
# Fit models of seasonality
################################################################################



#--------------------------------------------------------------------------------
# Seasonality of births in New York City.
#--------------------------------------------------------------------------------

# Plot the data. 
plot.ts(birthstimeseries)
# Notice an upward trend and clear seasonality. 

# Decompose into several components.
birth_components <- decompose(birthstimeseries)

# See the attributes attached to the components object. 
attributes(birth_components)

# Note that this is an additive decomposition. 
birth_components$type
# The components add up to the original series. 

# This shows a seasonal effect for each month. 
birth_components$seasonal

# This shows the trend.
birth_components$trend


# Plot the components together.
plot(birth_components)


# Plot the components individually on the same plot.
plot(birthstimeseries,
     main = c('Births per Month in New York, NY', 'Jan. 1946 to Dec. 1959'),
     xlab = 'Time',
     ylab = 'Births per Month',
     col = 'green', type = 'l', lwd = 3)
lines(birth_components$trend,
      col = 'blue', type = 'l', lwd = 3)
lines(birth_components$trend + birth_components$seasonal,
      col = 'red', type = 'l', lwd = 3)

# Analyze the residuals.
birth_components$random
summary(birth_components$random)
hist(birth_components$random)


# Inspect for autocorrelation.
acf(birth_components$random[!is.na(birth_components$random)])
pacf(birth_components$random[!is.na(birth_components$random)])
# Now one can fit an ARMA model, for instance. 




#--------------------------------------------------------------------------------
# Seasonality of souvenir sales in Queensland, Australia.
#--------------------------------------------------------------------------------

plot.ts(souvenirtimeseries)
# Notice the exponential growth pattern.

# Transform by taking logs.
logsouvenirtimeseries <- log(souvenirtimeseries)
plot.ts(logsouvenirtimeseries)
# Trend now appears linear. 


# Decompose into several components.
log_souvenir_components <- decompose(logsouvenirtimeseries)


# See the attributes attached to the components object. 
attributes(log_souvenir_components)

# This shows a seasonal effect for each month. 
log_souvenir_components$seasonal

# This shows the trend.
log_souvenir_components$trend


# Plot the components together.
plot(log_souvenir_components)


# Plot the components individually on the same plot.
plot(logsouvenirtimeseries,
     main = c('Log Souvenir Sales at a Resort in Australia', 'Jan. 1987 to Dec. 1993'),
     xlab = 'Time',
     ylab = 'Births per Month',
     col = 'green', type = 'l', lwd = 3)
lines(log_souvenir_components$trend,
      col = 'blue', type = 'l', lwd = 3)
lines(log_souvenir_components$trend + log_souvenir_components$seasonal,
      col = 'red', type = 'l', lwd = 3)

# Analyze the residuals.
log_souvenir_components$random
summary(log_souvenir_components$random)
hist(log_souvenir_components$random)


# Inspect for autocorrelation.
acf(log_souvenir_components$random[!is.na(log_souvenir_components$random)])
pacf(log_souvenir_components$random[!is.na(log_souvenir_components$random)])
# Not much variation aside from the seasonality. 



#--------------------------------------------------------------------------------
# Airline passenger data.
# Optional, since it is a much more involved example of seasonality adjustments. 
#--------------------------------------------------------------------------------

# Load the above dataset for this example.
xts <- x12(AirPassengers)
summary(xts)

attributes(xts)

# Estimate the model to take into account the seasonality. 
xs <- x12(new("x12Single", ts = AirPassengers))
summary(xs)



################################################################################
# End
################################################################################
