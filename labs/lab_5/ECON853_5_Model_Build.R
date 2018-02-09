################################################################################
# 
# ECON 853: Time Series Econometrics
# Introductory Examples
# 
# Lee Morin, Ph.D.
# Adjunct Assistant Professor
# Queen's University
# 
# February 8, 2018
# 
################################################################################
# 
# This program provides examples of model specification tests and diagnostics 
# in ARMA models.
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

# install.packages('FitARMA')
# library(FitARMA)


################################################################################
# Realization of an ARMA Model
################################################################################

#--------------------------------------------------------------------------------
# Set Parameters and Generate Data for ARMA(2,3)
#--------------------------------------------------------------------------------

set.seed(1234)

# Set length of series.
num_obs <- 1000

# Autoregressive coefficients.
phi_1_1 <- 0.60     # Autoregressive parameter
phi_1_2 <- 0.35     # Autoregressive parameter

# Moving average coefficients.
theta_1_1 <- 0.60     
theta_1_2 <- 0.25     
theta_1_3 <- 0.55

# Variance of innovations.
sigma_2 <- 1


# Initialize time series.
time_series <- data.frame(time = 1:num_obs,
                          arma_2_3 = numeric(num_obs))

# Generate realization of ARMA model.
time_series[, 'arma_2_3'] <- arima.sim(n = num_obs, list(ar = c(phi_1_1, phi_1_2), 
                                                         ma = c(theta_1_1, theta_1_2, theta_1_3)),
                                       sd = sqrt(sigma_2))

colnames(time_series)

summary(time_series)


#--------------------------------------------------------------------------------
# Plot the series
#--------------------------------------------------------------------------------

# Start with a blank plot to set the axes and labels.
plot(time_series[, 'time'], 
     time_series[, 'arma_2_3'], 
     type = 'n',
     main = 'ARMA(2,3) Process', 
     xlab = 'Time', 
     ylab = 'Level')
# Fill in with a line plot with settings for line style.
lines(time_series[, 'time'], 
      time_series[, 'arma_2_3'],
      type = 'l', 
      lwd = 3,
      col = 'blue')




#--------------------------------------------------------------------------------
# Identification.
#--------------------------------------------------------------------------------

acf(time_series[, 'arma_2_3'])

pacf(time_series[, 'arma_2_3'])


#--------------------------------------------------------------------------------
# Estimation with correctly specified model.
#--------------------------------------------------------------------------------

arma_2_3_arima <- arima(time_series[, 'arma_2_3'], order = c(2,0,3))


# Individual z-statistics on the coefficients.
arma_2_3_stats <- data.frame(coef = arma_2_3_arima$coef,
                             std_err = sqrt(diag(arma_2_3_arima$var.coef)))

arma_2_3_stats[, 'z_stat'] <- arma_2_3_stats[, 'coef']/arma_2_3_stats[, 'std_err']
arma_2_3_stats[, 'p_value'] <- pnorm(q = abs(arma_2_3_stats[, 'z_stat']), 
                                     mean = 0, sd = 1, lower.tail = FALSE) * 2 # Two-tailed test.
arma_2_3_stats

# Note: p-values are calculated under normal assumption.

# Note: A z-test is a one-dimensional Wald statistic.


#--------------------------------------------------------------------------------
# Estimation with Overspecification.
#--------------------------------------------------------------------------------

arma_3_3_arima <- arima(time_series[, 'arma_2_3'], order = c(3,0,3))


# Individual z-statistics on the coefficients for this model.
arma_3_3_stats <- data.frame(coef = arma_3_3_arima$coef,
                             std_err = sqrt(diag(arma_3_3_arima$var.coef)))

arma_3_3_stats[, 'z_stat'] <- arma_3_3_stats[, 'coef']/arma_3_3_stats[, 'std_err']
arma_3_3_stats[, 'p_value'] <- pnorm(q = abs(arma_3_3_stats[, 'z_stat']), 
                                     mean = 0, sd = 1, lower.tail = FALSE) * 2 # Two-tailed test.
arma_3_3_stats



################################################################################
# Residual Diagnostics
################################################################################

# Graphical approach: Inspect the autorrelation in the residuals.

# ARMA(3,3) model.
acf(arma_3_3_arima$residuals)
pacf(arma_3_3_arima$residuals)
# Looks like there is no autocorrelation.

# Pare down to a smaller model - ARMA(2,3).
acf(arma_2_3_arima$residuals)
pacf(arma_2_3_arima$residuals)
# Similarly, it looks like there is no autocorrelation here either.



# Estimate one model smaller - ARMA(1,3).
acf(arima(time_series[, 'arma_2_3'], order = c(1,0,3))$residuals)
pacf(arima(time_series[, 'arma_2_3'], order = c(1,0,3))$residuals)
# Seems to be some degree of autocorrelation.


# Estimate another smaller model - ARMA(2,2).
acf(arima(time_series[, 'arma_2_3'], order = c(2,0,2))$residuals)
pacf(arima(time_series[, 'arma_2_3'], order = c(2,0,2))$residuals)
# Again, there seems to be some degree of autocorrelation.

# The samllest model with white noise residuals was ARMA(2,3).
# Which makes sense, since this is the true model.



################################################################################
# Tests for Residual Diagnostics
################################################################################

# Use the plot = FALSE argument to suppress the plot and return the autocorrelation estimates.

# Choose a number of lags to consider.
num_lags <- 20

# Estimate the model and extract the residuals.
arma_2_3_resids <- arima(time_series[, 'arma_2_3'], 
                         order = c(2,0,3))$residuals

# Calculate the ACF.
acf_2_3 <- acf(arma_2_3_resids, 
                 plot = FALSE, 
                 lag.max = num_lags + 1)

# First need a numeric vector of autocorrelations.
acf_2_3_vec <- as.numeric(unlist(acf_2_3)[2:(num_lags + 1)])

# Calculate the value of the Box-Pearce Q-statistic.
Q_stat <- num_obs*sum(acf_2_3_vec^2)


# Calculate the p-value.
Q_stat_p_value <- pchisq(Q_stat, df = num_lags, ncp = 0, lower.tail = FALSE)


# Appears to be very little evidence of serial correlation in the residuals.
Q_stat
Q_stat_p_value




# Try an automated function instead.
# Requires the forecast package.
Box.test(arma_2_3_resids, lag = num_lags, type="Box-Pierce")

# Instead, try the Ljung-Box statistics, a modified version of the Box-Pearce statistic.
Box.test(arma_2_3_resids, lag = num_lags, type="Ljung-Box")





################################################################################
# Model Specification with Information Criteria
################################################################################


# Calculate AIC criteria for a series of ARMA models.
ar_order_list <- seq(5)
ma_order_list <- seq(5)

# Initialize a data frame to store values of the AIC.
arma_aic_stats <- expand.grid(ar_order_list, ma_order_list)
colnames(arma_aic_stats) <- c('ar_order', 'ma_order')

# Append a blank column for the AIC statistic.
arma_aic_stats[, 'AIC'] <- NA

# Just in case, store the convergence code of the optimization.
# (In case of identification problems from severe overspecification.)
arma_aic_stats[, 'conv_cond'] <- NA


# Loop through list of model sizes and estimate ARMA(p,q).
for (model_num in 1:nrow(arma_aic_stats)) {
  
  # Estimate the specified ARMA model model.
  arma_model <- arima(time_series[, 'arma_2_3'], 
                      order = c(arma_aic_stats[model_num, 'ar_order'], 
                                0, 
                                arma_aic_stats[model_num, 'ma_order']))
  
  # Store the value of the information criterion.
  arma_aic_stats[model_num, 'AIC'] <- arma_model$aic
  
  # Store the convergence code of the optimization.
  arma_aic_stats[model_num, 'conv_cond'] <- arma_model$code
  
  
  
}

# Determine the winner.
arma_aic_stats
arma_aic_stats[which(arma_aic_stats[, 'AIC'] == min(arma_aic_stats[, 'AIC'])), ]



################################################################################
# End
################################################################################
