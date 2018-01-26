################################################################################
# 
# ECON 853: Time Series Econometrics
# Identification with ACF and PACF
# 
# Lee Morin, Ph.D.
# Adjunct Assistant Professor
# Queen's University
# 
# January 25, 2018
# 
################################################################################
# 
# This program provides examples of identification with ACFs and PACFs.
# 
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


################################################################################
# Example 1: First-order AutoRegressive model - AR(1)
################################################################################

#--------------------------------------------------------------------------------
# Set Parameters and Generate Data
#--------------------------------------------------------------------------------

phi_1_1 <- 0.75     # Autoregressive parameter
sigma_2 <- 1        # Variance of innovations
num_obs <- 100      # Number of observations
# y_1_0 <- 7          # Initial value



# Initialize time series.
time_series <- data.frame(time = 1:num_obs,
                          epsilon_1 = rnorm(n = num_obs, mean = 0, sd = sigma_2),
                          ar_1 = numeric(num_obs))

# Generate a realization from this model.
time_series[, 'ar_1'] <- arima.sim(n = num_obs, list(ar = c(phi_1_1), 
                                                     ma = c(0)),
                                   sd = sqrt(sigma_2))



#--------------------------------------------------------------------------------
# Plot the series
#--------------------------------------------------------------------------------

# Start with a blank plot to set the axes and labels.
plot(time_series[, 'time'], 
     time_series[, 'ar_1'], 
     type = 'n',
     main = 'AR(1) Process', 
     xlab = 'Time', 
     ylab = 'Level')
# Fill in with a line plot with settings for line style.
lines(time_series[, 'time'], 
      time_series[, 'ar_1'],
      type = 'l', 
      lwd = 3,
      col = 'blue')


#--------------------------------------------------------------------------------
# Identification: Autocorrelation function
#--------------------------------------------------------------------------------

acf(time_series[, 'ar_1'])
# Notice the geometric decay, as predicted.


#--------------------------------------------------------------------------------
# Identification: Partial Autocorrelation function
#--------------------------------------------------------------------------------

pacf(time_series[, 'ar_1'])
# Notice only one lag is important.


#--------------------------------------------------------------------------------
# Estimation: First-order AutoRegressive model - AR(1)
# By linear regression
#--------------------------------------------------------------------------------

# Generate lagged values of the variable.
time_series[, 'ar_1_lag'] <- c(NA, time_series[1:(num_obs - 1), 'ar_1'])

head(time_series[, c('ar_1', 'ar_1_lag')])

# Specify the equation to estimate.
fmla_string <- 'ar_1 ~ ar_1_lag'
fmla <- as.formula(fmla_string)

ar_1_lm <- lm(data = time_series, formula = fmla)

summary(ar_1_lm)


#--------------------------------------------------------------------------------
# Estimation: First-order AutoRegressive model - AR(1)
# By maximum likelihood
#--------------------------------------------------------------------------------


ar_1_arima <- arima(time_series[, 'ar_1'], order = c(1,0,0))


# See a list of everything in the arima object.
summary(ar_1_arima)

# Display some of the values.
ar_1_arima$coef
ar_1_arima$var.coef
ar_1_arima$sigma2


# There are also functions designed to extract the usual suspects.
coef(ar_1_arima)
vcov(ar_1_arima)




################################################################################
# Example 2: Second-order AutoRegressive model - AR(2)
################################################################################

#--------------------------------------------------------------------------------
# Set Parameters and Generate Data
#--------------------------------------------------------------------------------

phi_2_1 <- 0.60     # Autoregressive parameter
phi_2_2 <- 0.35     # Autoregressive parameter


# Generate a realization from this model.
time_series[, 'ar_2'] <- arima.sim(n = num_obs, list(ar = c(phi_2_1, phi_2_2), 
                                                     ma = c(0)),
                                   sd = sqrt(sigma_2))


#--------------------------------------------------------------------------------
# Plot the series
#--------------------------------------------------------------------------------

# Start with a blank plot to set the axes and labels.
plot(time_series[, 'time'], 
     time_series[, 'ar_2'], 
     type = 'n',
     main = 'AR(2) Process', 
     xlab = 'Time', 
     ylab = 'Level')
# Fill in with a line plot with settings for line style.
lines(time_series[, 'time'], 
      time_series[, 'ar_2'],
      type = 'l', 
      lwd = 3,
      col = 'red')


#--------------------------------------------------------------------------------
# Identification: Autocorrelation function
#--------------------------------------------------------------------------------

acf(time_series[, 'ar_2'])
# Gradual decline indicates some AR structure.
# Declining more slowly than for AR(1).

#--------------------------------------------------------------------------------
# Identification: Partial Autocorrelation function
#--------------------------------------------------------------------------------

pacf(time_series[, 'ar_2'])
# Notice two lags are important.



#--------------------------------------------------------------------------------
# Estimation: Second-order AutoRegressive model - AR(2)
# By linear regression
#--------------------------------------------------------------------------------

# Generate lagged values of the variable.
time_series[, 'ar_2_lag_1'] <- c(NA, time_series[1:(num_obs - 1), 'ar_2'])
time_series[, 'ar_2_lag_2'] <- c(NA, NA, time_series[1:(num_obs - 2), 'ar_2'])

head(time_series[, c('ar_2', 'ar_2_lag_1', 'ar_2_lag_2')])


# Specify the equation to estimate.
fmla_string <- 'ar_2 ~ ar_2_lag_1 + ar_2_lag_2'
fmla <- as.formula(fmla_string)

ar_2_model <- lm(data = time_series, formula = fmla)

summary(ar_2_model)



#--------------------------------------------------------------------------------
# Estimation: Second-order AutoRegressive model - AR(2)
# By maximum likelihood
#--------------------------------------------------------------------------------

ar_2_arima <- arima(time_series[, 'ar_2'], order = c(2,0,0))


# Display some of the values.
ar_2_arima$coef
ar_2_arima$var.coef
ar_2_arima$sigma2





################################################################################
# Example 3: Moving Avergae Model - MA(3)
################################################################################

#--------------------------------------------------------------------------------
# Set Parameters and Generate Data
#--------------------------------------------------------------------------------

theta_3_1 <- 0.60     
theta_3_2 <- 0.25     
theta_3_3 <- 0.55

# Generate a realization from this model.
time_series[, 'ma_3'] <- arima.sim(n = num_obs, list(ma = c(theta_3_1, theta_3_2, theta_3_3)),
                                   sd = sqrt(sigma_2))



#--------------------------------------------------------------------------------
# Identification: Autocorrelation function
#--------------------------------------------------------------------------------

acf(time_series[, 'ma_3'])
# Abrupt drop to zero indicates some MA structure.


#--------------------------------------------------------------------------------
# Identification: Partial Autocorrelation function
#--------------------------------------------------------------------------------

pacf(time_series[, 'ma_3'])
# One lag of AR.



#--------------------------------------------------------------------------------
# Estimation: Auto-Regressive Moving Average - ARMA(1,4)
# Overfit model.
# By maximum likelihood
#--------------------------------------------------------------------------------

arma_1_4_arima <- arima(time_series[, 'ma_3'], order = c(1,0,4))


# Display some of the values.
arma_1_4_arima$coef
arma_1_4_arima$var.coef
arma_1_4_arima$sigma2


#--------------------------------------------------------------------------------
# Estimation: Moving Average - MA(3)
# Correctly specified model.
# By maximum likelihood
#--------------------------------------------------------------------------------

ma_3_arima <- arima(time_series[, 'ma_3'], order = c(0,0,3))


# Display some of the values.
ma_3_arima$coef
ma_3_arima$var.coef
ma_3_arima$sigma2





################################################################################
# End
################################################################################