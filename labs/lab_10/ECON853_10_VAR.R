################################################################################
# 
# ECON 853: Time Series Econometrics
# Vector AutoRegression (VAR) models
# 
# Lee Morin, Ph.D.
# Adjunct Assistant Professor
# Queen's University
# 
# March 22, 2018
# 
################################################################################
# 
# This program provides examples of estimating Vector AutoRegression (VAR) models.
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
library(tseries)

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
# library(x12)

# Another package for measuring seasonality.
# install.packages('x12')
# library('TTR')


# Library designed to estimate Vector AutoRegression (VAR) models.
# install.packages('vars')
library(vars)






################################################################################
# Loading data
################################################################################

# Load Canada dataset.
data(Canada)

summary(Canada)
# Variables are:
#   e: employment
#   prod: maesure of labour productivity
#   U: unemployment rate
#   rw: real wage
# These are calculated from a dataset from the OECD, using transformations from 
# economic indicators, national accounts and labour force statistics.
# See the vars package documentation in: https://cran.r-project.org/web/packages/vars/vars.pdf



################################################################################
# Preliminary analysis
################################################################################


#--------------------------------------------------------------------------------
# Visual inspection of the data
#--------------------------------------------------------------------------------

plot(Canada[, 'e'], type = 'l', col = 'black', lwd = 2)
plot(Canada[, 'prod'], type = 'l', col = 'black', lwd = 2)
plot(Canada[, 'rw'], type = 'l', col = 'black', lwd = 2)
plot(Canada[, 'U'], type = 'l', col = 'black', lwd = 2)


################################################################################
# Analysis of Stationarity
################################################################################

#--------------------------------------------------------------------------------
# ADF tests
#--------------------------------------------------------------------------------


adf.test(Canada[, 'e'])
adf.test(Canada[, 'prod'])
adf.test(Canada[, 'rw'])
adf.test(Canada[, 'U'])

# These ADF tests do not look promising.
# The null of a unit root cannot be rejected in either case. 
# However, some of the graphs (U, maybe rw) might appear stationary.


#--------------------------------------------------------------------------------
# Analysis of Autocovariance
#--------------------------------------------------------------------------------

# Check the autocovariance functions and make a call.
acf(Canada[, 'e'])
acf(Canada[, 'prod'])
acf(Canada[, 'rw'])
acf(Canada[, 'U'])

# Quite a lot of persistence but not as strong as the UR tests would indicate. 
# Careful, since the dataset is not very long. 
# Still, take a chance and model the levels. 


# A practical approach to check for stationarity is to fit a model anyway
# and forecast to see if the series diverge. 



################################################################################
# VAR Order Selection
################################################################################

# Calculate information criteria and select the lag order.

# AIC might specify more lags than necessary. Performance improves for large samples.
# HQ places a larger penalty on the number of parameters, so it might suggest a smaller model.
# SC/BIC places an even larger penalty on the number of parameters, so it might suggest an even smaller model. 
# Forecast prediction might still improve with an overspecified model but this might be overfitting. 

VARselect(Canada, lag.max = 5, type="const")
# 2 or 3 lags, 2 is conservative. 

VARselect(Canada, lag.max = 5, type="both")
# 1, 2, or 3 lags, 2 is in the middle.

# Go with 2 lags, could adjust later for sensitivity.


################################################################################
# Estimation
################################################################################

var_2n <- VAR(Canada, p = 2, type = "none")
var_2c <- VAR(Canada, p = 2, type = "const")
var_2t <- VAR(Canada, p = 2, type = "trend")
var_2b <- VAR(Canada, p = 2, type = "both")


summary(var_2c)
summary(var_2b)
# See the R^2 values. Too good to be true?




################################################################################
# Postestimation Diagnostics
################################################################################

#--------------------------------------------------------------------------------
# Roots of the chaeracteristic equation
#--------------------------------------------------------------------------------

# Eigenvalues of the companion coefficient matrix of a VAR(p)-process

roots(var_2c)
# All roots are within the unit circle, even though one is very close to 1.

roots(var_2b)
# With the added trend, the model looks closer to stationary.


# Continue with the model with a constant, as an example.
# Would repeat for the other candidates. 


#--------------------------------------------------------------------------------
# Analyze the residuals
#--------------------------------------------------------------------------------

# Calculate residuals.
var_2c_resid <- resid(var_2c)

summary(var_2c_resid)

plot(var_2c_resid[, 'e'], type = 'l', col = 'red', lwd = 2)
plot(var_2c_resid[, 'prod'], type = 'l', col = 'red', lwd = 2)
plot(var_2c_resid[, 'rw'], type = 'l', col = 'red', lwd = 2)
plot(var_2c_resid[, 'U'], type = 'l', col = 'red', lwd = 2)



# Test for serial correlation.
serial.test(var_2c, lags.pt = 16, type = "PT.adjusted")
# No strong evidence for serial correlation. Lag order is high enough.

# If this test rejected, you could investigate the original series and decide 
# how to modify the model. 


# Tests of normality of the residuals.
normality.test(var_2c)
# Errors appear normal.



# Analyze predictions.
var_2c_fcst <- predict(var_2c, n.ahead = 1000, ci = 0.95)

plot(var_2c_fcst$fcst$e[, 'fcst'], type = 'l', col = 'blue', lwd = 2)
plot(var_2c_fcst$fcst$prod[, 'fcst'], type = 'l', col = 'blue', lwd = 2)
plot(var_2c_fcst$fcst$rw[, 'fcst'], type = 'l', col = 'blue', lwd = 2)
plot(var_2c_fcst$fcst$U[, 'fcst'], type = 'l', col = 'blue', lwd = 2)


# Append these to the original series.
plot(c(Canada[, 'e'], var_2c_fcst$fcst$e[, 'fcst']), 
     type = 'l', col = 'blue', lwd = 2)
plot(c(Canada[, 'prod'], var_2c_fcst$fcst$prod[, 'fcst']), 
     type = 'l', col = 'blue', lwd = 2)
plot(c(Canada[, 'rw'], var_2c_fcst$fcst$rw[, 'fcst']), 
     type = 'l', col = 'blue', lwd = 2)
plot(c(Canada[, 'U'], var_2c_fcst$fcst$U[, 'fcst']), 
     type = 'l', col = 'blue', lwd = 2)

# Since it is a stationary model, the series eventually converge to a long run mean.
# However, there is a great deal of growth in the forecast until a steady state is reached. 

# Notice that forecasts for unemployment are particularly unrealistic in the limit. 



################################################################################
# Interpretation
################################################################################


#--------------------------------------------------------------------------------
# Granger Causality
#--------------------------------------------------------------------------------

# Run all causality tests, for all variables, for both Granger causality and instantaneous causality.

# Check for employment.
causality(var_2c, cause = 'e')
# Reject no causality of e for both forms of causality.

# Check for productivity.
causality(var_2c, cause = 'prod')
# Reject no causality of prod for Granger causality only, not instantaneous.

# Check for the real wage.
causality(var_2c, cause = 'rw')
# Reject no causality of rw for Granger causality only, not instantaneous.


# Check for uneployment.
causality(var_2c, cause = 'U')
# Reject no causality of U for both forms of causality.

# Conclude that each variable has predictive value for future values
# but that only e and U have predictive value instantaneously. 



#--------------------------------------------------------------------------------
# Impulse response functions
#--------------------------------------------------------------------------------

# Default is to produce all sets of IRFs, with each variable as the response variable.
var_2c_irf <- irf(var_2c)

# Will display all 4 sets of IRF plots in series. Hit <Return> to see next plot.
plot(var_2c_irf)



#--------------------------------------------------------------------------------
# Forecast Error Variance Decomposition
#--------------------------------------------------------------------------------

# Bar charts show the proportion of forecast error variance that is due to impulses in each variable.


var_2c_fevd <- fevd(var_2c, n.ahead = 5)
plot(var_2c_fevd)

# Compare the variation explained by univariate relationships
# to that from cross variation between variables. 



################################################################################
# End
################################################################################

