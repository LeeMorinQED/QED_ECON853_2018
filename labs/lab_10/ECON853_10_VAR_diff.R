################################################################################
# 
# ECON 853: Time Series Econometrics
# Vector AutoRegression (VAR) models
# Example 2: Modeling First Differences.
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
# ADF tests on levels
#--------------------------------------------------------------------------------

adf.test(Canada[, 'e'])
adf.test(Canada[, 'prod'])
adf.test(Canada[, 'rw'])
adf.test(Canada[, 'U'])

# These ADF tests do not look promising.
# The null of a unit root cannot be rejected in either case. 
# However, some of the graphs (U, maybe rw) might appear stationary.



#--------------------------------------------------------------------------------
# Calculate first differences
#--------------------------------------------------------------------------------

# This time we won't take any chances and will first difference all the variables.

Canada_diff <- Canada[2:nrow(Canada), ] - Canada[1:(nrow(Canada) - 1), ]


plot(Canada_diff[, 'e'], type = 'l', col = 'black', lwd = 2)
plot(Canada_diff[, 'prod'], type = 'l', col = 'black', lwd = 2)
plot(Canada_diff[, 'rw'], type = 'l', col = 'black', lwd = 2)
plot(Canada_diff[, 'U'], type = 'l', col = 'black', lwd = 2)
# All appear stationary.

#--------------------------------------------------------------------------------
# ADF tests on first differences
#--------------------------------------------------------------------------------

adf.test(Canada_diff[, 'e'])
adf.test(Canada_diff[, 'prod'])
adf.test(Canada_diff[, 'rw'])
adf.test(Canada_diff[, 'U'])

# At least this time the p-values are reasonably low. 


#--------------------------------------------------------------------------------
# Analysis of Autocovariance
#--------------------------------------------------------------------------------

acf(Canada_diff[, 'e'])
acf(Canada_diff[, 'prod'])
acf(Canada_diff[, 'rw'])
acf(Canada_diff[, 'U'])

# Much more reassuring of stationarity.


################################################################################
# VAR Order Selection
################################################################################

# Calculate information criteria and select the lag order.

# AIC might specify more lags than necessary. Performance improves for large samples.
# HQ places a larger penalty on the number of parameters, so it might suggest a smaller model.
# SC/BIC places an even larger penalty on the number of parameters, so it might suggest an even smaller model. 
# Forecast prediction might still improve with an overspecified model but this might be overfitting. 

VARselect(Canada_diff, lag.max = 5, type="const")
# 1 or 2 lags. 

VARselect(Canada_diff, lag.max = 5, type="both")
# 1 or 2 lags.

# Stick with 2 lags, could adjust later for sensitivity.


################################################################################
# Estimation
################################################################################

var_2n <- VAR(Canada_diff, p = 2, type = "none")
var_2c <- VAR(Canada_diff, p = 2, type = "const")
var_2t <- VAR(Canada_diff, p = 2, type = "trend")
var_2b <- VAR(Canada_diff, p = 2, type = "both")


# Model with constants.
summary(var_2c)
# Constants are significant.

# Model with trends.
summary(var_2b)
# Constants remain, while only one variable has a statistically significant trend. 
# Note that now that we are dealing with differenced data, the constant would imply
# a trend in the levels of the variable. 
# The trend variable for the differenced variable would imply a quadratic function
# in the deterministics of the level when the differences are integrated back up to levels. 

# Better to go with the model with a constant only, unless the evidence were overwhelming.


# Also, notice the R^2 values are reasonably high but plausible. 




################################################################################
# Postestimation Diagnostics
################################################################################

#--------------------------------------------------------------------------------
# Roots of the chaeracteristic equation
#--------------------------------------------------------------------------------

# Eigenvalues of the companion coefficient matrix of a VAR(p)-process

roots(var_2c)
# All roots are well within the unit circle.

roots(var_2b)
# All roots are well within the unit circle.


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
# Errors appear normal, with maybe a small degree of skewness.



# Analyze predictions.
var_2c_fcst <- predict(var_2c, n.ahead = 100, ci = 0.95)

plot(var_2c_fcst$fcst$e[, 'fcst'], type = 'l', col = 'blue', lwd = 2)
plot(var_2c_fcst$fcst$prod[, 'fcst'], type = 'l', col = 'blue', lwd = 2)
plot(var_2c_fcst$fcst$rw[, 'fcst'], type = 'l', col = 'blue', lwd = 2)
plot(var_2c_fcst$fcst$U[, 'fcst'], type = 'l', col = 'blue', lwd = 2)
# Notice that these forecasts are much more boring that the ones from the levels.
# Persistence is much lower and the forecasted differences quickly converge to a constant
# (and the constant would imply a trend in the levels).


# Calculate forecasts of the levels from the integrated forecasts of the differences.
Canada_fcst <- data.frame(e = cumsum(c(Canada[nrow(Canada), 'e'], var_2c_fcst$fcst$e[, 'fcst'])),
                          prod = cumsum(c(Canada[nrow(Canada), 'prod'], var_2c_fcst$fcst$prod[, 'fcst'])),
                          rw = cumsum(c(Canada[nrow(Canada), 'rw'], var_2c_fcst$fcst$rw[, 'fcst'])),
                          U = cumsum(c(Canada[nrow(Canada), 'U'], var_2c_fcst$fcst$U[, 'fcst'])))


# Append these to the original series.
plot(c(Canada[, 'e'], Canada_fcst[, 'e']), 
     type = 'l', col = 'blue', lwd = 2)
plot(c(Canada[, 'prod'], Canada_fcst[, 'prod']), 
     type = 'l', col = 'blue', lwd = 2)
plot(c(Canada[, 'rw'], Canada_fcst[, 'rw']), 
     type = 'l', col = 'blue', lwd = 2)
plot(c(Canada[, 'U'], Canada_fcst[, 'U']), 
     type = 'l', col = 'blue', lwd = 2)

# Since it is a stationary model, the series eventually converge to a long run mean.
# However, there is a great deal of growth in the forecast until a steady state is reached. 

# Notice that forecasts for unemployment are now very optimistic.
# Still has unrealistic implications in the limit but better than for levels. 



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
# Reject no causality of rw for Granger causality only, not instantaneous (but close, investigate further).


# Check for uneployment.
causality(var_2c, cause = 'U')
# Reject no causality of U for instantaneous of causality but not for Granger causality.




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

