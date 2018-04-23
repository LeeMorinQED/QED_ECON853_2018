################################################################################
# 
# ECON 853: Time Series Econometrics
# Cointegrated Vector AutoRegression (VAR) models
# 
# 
# Lee Morin, Ph.D.
# Adjunct Assistant Professor
# Queen's University
# 
# March 22, 2018
# 
################################################################################
# 
# This program provides examples of estimating cointegrated Vector 
# AutoRegression (VAR) models.
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
library(urca)

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


# Load Danish Dataset.
data("denmark")
summary(denmark)


# A data frame with 55 observations on the following 6 variables.
# period Time index from 1974:Q1 until 1987:Q3.
# LRM Logarithm of real money, M2.
# LRY Logarithm of real income.
# LPY Logarithm of price deflator.
# IBO Bond rate.
# IDE Bank deposit rate.



################################################################################
# Motivating Example
################################################################################


#--------------------------------------------------------------------------------
# Generate data
#--------------------------------------------------------------------------------

# Simulated cointegrated series
set.seed(123)

# Generate a unit root
num_obs <- 10000
z <- cumsum(rnorm(num_obs))
plot(z, type="l")
# Looks like a unit root.
adf.test(z)

# Generate three series driven by that common (stochastic) trend.
p <- 0.3*z + rnorm(num_obs)
q <- 0.6*z + rnorm(num_obs)
r <- 0.8*z + rnorm(num_obs)


#--------------------------------------------------------------------------------
# Cointegration Analysis
#--------------------------------------------------------------------------------


# Estimate the cointegrated VAR model and inspect the results. 
jotest_sim <- ca.jo(data.frame(p,q,r), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest_sim)


# Decompose the elements of the vecm (the VECM is a Restricted Least Squares model).
jotest_sim_rls <- cajorls(jotest_sim, r = 1, reg.number = NULL)
attributes(jotest_sim_rls)
jotest_sim_rls


# Generate a series from the first cointegrating relation. 
coint_sim_1 = 1.000*p + 1.826906*q - 1.743881*r
plot(coint_sim_1, type="l")

# Verify that cointegrating relation is stationary.
adf.test(coint_sim_1)
# Reject null of unit root in residuals. 


# Generate a series from the second cointegrating relation. 
coint_sim_2 = 1.000*p - 0.51628593*q + 0.01112185*r
plot(coint_sim_2, type="l")

# Verify that cointegrating relation is stationary.
adf.test(coint_sim_2)
# Reject null of unit root in residuals. 


# Note that there are only two cointegrating relationships. 
# The third captures the common stochastic trend.
coint_sim_3 = 1.000*p + 1.938087*q + 2.764549*r
plot(coint_sim_3, type="l")
# Looks familiar.

# The original common stochastic trend.
plot(z, type="l")
# And we already know that this is a unit root.


#--------------------------------------------------------------------------------
# Analysis with linear regression models
#--------------------------------------------------------------------------------

# Try regressing all of them together.
lm_sim_full <- lm(formula = as.formula('p ~  -1 + q + r'), data = data.frame(p,q,r))
summary(lm_sim_full)
lm_resid_full <- p - predict(lm_sim_full)
plot(lm_resid_full, type="l")
adf.test(lm_resid_full)
# Residuals are stationary.

# Now consider pairs of regressors.

# p vs q.
lm_sim_q <- lm(formula = as.formula('p ~  -1 + q'), data = data.frame(p,q,r))
summary(lm_sim_q)
lm_resid_q <- p - predict(lm_sim_q)
plot(lm_resid_q, type="l")
adf.test(lm_resid_q)
# Residuals are stationary.

# p vs r.
lm_sim_r <- lm(formula = as.formula('p ~  -1 + r'), data = data.frame(p,q,r))
summary(lm_sim_r)
lm_resid_r <- p - predict(lm_sim_r)
plot(lm_resid_r, type="l")
adf.test(lm_resid_r)
# Residuals are stationary.

# Any linear combination of residuals from these bivariate models will be stationary.



#--------------------------------------------------------------------------------
# Testing
#--------------------------------------------------------------------------------

# Test that the cointegrating relation fits in a subspace.


# Verify that the first cointegrating relation is in the first space. 
H5_1_sim <- c(1, 1.826906, - 1.743881)
bh5_test_1_sim <- bh5lrtest(jotest_sim, H = H5_1_sim, r = 2)
summary(bh5_test_1_sim)
# Accept H_0. Note that the p-value is exactly 1, since I tested at the point estimates. 


# Test a vector far from the cointegrating space. 
H5_2_sim <- c(1, 1, 1)
bh5_test_2_sim <- bh5lrtest(jotest_sim, H = H5_2_sim, r = 2)
summary(bh5_test_2_sim)
# Reject H_0. Note that the p-value is exactly 0, since I tested a restriction 
# that is closer to the orthogonal complement. 



# Test a vector far from the cointegrating space. 
H5_3_sim <- c(1, 1.8, -1.75)
bh5_test_3_sim <- bh5lrtest(jotest_sim, H = H5_3_sim, r = 2)
summary(bh5_test_3_sim)



# Reject H_0. Note that the p-value is exactly 0, since I tested a restriction 
# that is still far from the cointegrating space. 




################################################################################
# The Danish Data
################################################################################


#--------------------------------------------------------------------------------
# Estimation
#--------------------------------------------------------------------------------


summary(denmark)

# Select the variables in Johansen's example (as is).
sj_denmark <- denmark[, c("LRM", "LRY", "LPY", "IBO", "IDE")]


sj_denmark_vecm <- ca.jo(sj_denmark, ecdet = "const", type="eigen", 
                         K=2, spec="longrun", season=4)
summary(sj_denmark_vecm)
# Seems to be one cointegrating relationship.


# Decompose the elements of the vecm.
sj_denmark_rls <- cajorls(sj_denmark_vecm, r = 1, reg.number = NULL)
attributes(sj_denmark_rls)
sj_denmark_rls


#--------------------------------------------------------------------------------
# Postestimation
#--------------------------------------------------------------------------------

# First attempt to calculate predictions. 
# sj_denmark_pred <- predict(sj_denmark_vecm)
# Fails. Need to convert to VAR model first. 

# Create VAR model object from VECM model. 
sj_denmark_var <- vec2var(sj_denmark_vecm, r = 1)
sj_denmark_var

summary(sj_denmark_var)

# Residuals.
summary(sj_denmark_var$resid)
nrow(sj_denmark_var$resid)
nrow(sj_denmark)
# Same number of rows, aside from lags.




# Calculate predictions from VAR object.
sj_denmark_pred <- predict(sj_denmark_var)
summary(sj_denmark_pred)

# See what is in the prediction object.
attributes(sj_denmark_pred)
sj_denmark_pred$model

# Default forecast is 10 lags. 
sj_denmark_pred$fcst

# Predictions for the variables. 
summary(sj_denmark_pred$endog)
nrow(sj_denmark_pred$endog)




#--------------------------------------------------------------------------------
# Are all variables I(1)?
#-------------------------------------------------------------------------------

# Take a closer look at LPY. 
plot(denmark[, 'LPY'], type="l")
adf.test(denmark[, 'LPY'])
# Clearly nonstationary.

# Take differences of LPY. 
plot(diff(denmark[, 'LPY']), type="l")
# Still looks nonstationary.
adf.test(diff(denmark[, 'LPY']))
# Still can't reject the null of unit root.

# Add this differenced variable to the model.
denmark[, 'DLPY'] <- c(NA, diff(denmark[, 'LPY']))


# Select the variables in the exact form used in Johansen's example.
sj_denmark_2 <- denmark[, c("LRM", "LRY", "DLPY", "IBO", "IDE")]
sj_denmark_2_vecm <- ca.jo(sj_denmark_2, ecdet = "const", type="eigen", 
                           K=2, spec="longrun", season=NULL)
summary(sj_denmark_2_vecm)
# Still seems to be one cointegrating relationship.






################################################################################
# Canadian Data
################################################################################

summary(Canada)

sj_canada_vecm <- ca.jo(Canada, ecdet = "const", type="eigen", 
                        K=2, spec="longrun", season=4)
summary(sj_canada_vecm)
# Looks like one, maybe two, cointegrating relationship(s).


# But wait: Recall that these data had a linear trend. 
sj_canada_vecm_tr <- ca.jo(Canada, ecdet = "trend", type="eigen", 
                           K=2, spec="longrun", season=4)
summary(sj_canada_vecm_tr)
# Seems to be one cointegrating relationship.


# Decompose the elements of the vecm.
sj_canada_rls <- cajorls(sj_canada_vecm_tr, r = 1, reg.number = NULL)
attributes(sj_canada_rls)
sj_canada_rls


#--------------------------------------------------------------------------------
# Postestimation
#--------------------------------------------------------------------------------

# Create VAR model object from VECM model. 
sj_canada_var <- vec2var(sj_canada_vecm_tr, r = 1)
sj_canada_var


# Residuals.
summary(sj_canada_var$resid)
nrow(sj_canada_var$resid)
nrow(Canada)
# Same number of rows, aside from lags.




# Calculate predictions from VAR object.
sj_canada_pred <- predict(sj_canada_var)
summary(sj_canada_pred)

# See what is in the prediction object.
attributes(sj_canada_pred)
sj_canada_pred$model

# Default forecast is 10 lags. 
sj_canada_pred$fcst

# Predictions for the variables. 
summary(sj_canada_pred$endog)
nrow(sj_canada_pred$endog)




################################################################################
# End
################################################################################
