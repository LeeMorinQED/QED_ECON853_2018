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
# This program provides examples of hypothesis tests in ARMA models.
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

# install.packages('numDeriv')
library(numDeriv)


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


arma_2_3_arima$coef
arma_2_3_arima$var.coef

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


#--------------------------------------------------------------------------------
# Likelihood Ratio Test.
#--------------------------------------------------------------------------------

# Calculated from optimized values of likelihood functions from each model.

# Reminder of the attributes of the arima model object.
attributes(arma_2_3_arima)

# UNrestricted model is ARMA(3,3)
arma_3_3_arima$loglik

# Restricted model is ARMA(2,3)
arma_2_3_arima$loglik

# Calculate the value of the likelihood ratio statistic.
lr_stat <- - 2*(arma_2_3_arima$loglik - arma_3_3_arima$loglik)

# One restricted parameter implies one degree of freedom.
lr_df <- 1

# p-value for the likelihood ratio statistic.
lr_p_value <- pchisq(lr_stat, df = lr_df, ncp = 0, lower.tail = FALSE)


# Test provides no evidence against the null hypothesis that the restriction holds.
lr_stat
lr_p_value



#--------------------------------------------------------------------------------
# Wald Test.
#--------------------------------------------------------------------------------

# Test a restriction on the vector of parameter estimates from UNrestricted model: ARMA(3,3)
arma_3_3_arima$coef

# Construct a selection matrix and restriction vector for the restriction that phi_1_3 = 0.
# i.e. an ARMA(2,3) is the correct model.
restrict_val <- 0
restrict_mat <- matrix(rep(0,7), nrow = 1, ncol = 7)
colnames(restrict_mat) <- names(arma_3_3_arima$coef)
restrict_mat[1, 'ar3'] <- 1

# Trust but verify.
restrict_mat
restrict_mat * arma_3_3_arima$coef


# Note the data types.
class(restrict_mat) # A matrix
class(arma_3_3_arima$coef) # A vector
psi_hat <- as.matrix(arma_3_3_arima$coef)
restrict_mat %*% psi_hat


# Testing the restriction that the following is zero.
wald_test_rest <- restrict_mat %*% psi_hat - restrict_val
wald_test_rest


# Calculate quadratic form.
# The solve(A) function with one argument will invert the matrix A.
# i.e. it solves AX = I for X, with I the default second argument.
wald_test_stat <- t(wald_test_rest) * 
  solve(restrict_mat %*% arma_3_3_arima$var.coef %*% t(restrict_mat)) * 
  wald_test_rest
wald_test_stat



# Sense check against previously calculated z-statistics.
restrict_mat %*% arma_3_3_arima$var.coef %*% t(restrict_mat) - arma_3_3_arima$var.coef['ar3', 'ar3']
wald_test_stat - arma_3_3_stats['ar3', 'z_stat']^2
# Both are numerically zero, as expected.


# p-value for the likelihood ratio statistic.
wald_df <- nrow(restrict_mat)
wald_p_value <- pchisq(wald_test_stat, df = wald_df, ncp = 0, lower.tail = FALSE)


# Test provides no evidence against the null hypothesis that the restriction holds.
wald_test_stat
wald_p_value



#--------------------------------------------------------------------------------
# Lagrange Multiplier Test.
#--------------------------------------------------------------------------------

# Requires derivatives of the likelihood function.

# Write a function for the value of the likelihood function from the parameters.
# Note that the fixed argument allows us to constrain the value of a specific parameter.
#   In this case, we are fixing all of them.
#   Only the parameters with missing values will be estimated.
arma_3_3_arima_like <- function(params) {
  
  # 'Estimate' the model constrained to the estiamtes.
  arima_model <- arima(time_series[, 'arma_2_3'], order = c(3,0,3), 
                       fixed = params)
  
  # Extract and return the value of the likelihood function.
  return(arima_model$loglik)
  
}

# Evaluate it at the estimted coefficients.
arma_3_3_arima_like(arma_3_3_arima$coef)

# Compare with the estimated value.
arma_3_3_arima$loglik


# Check the derivative of this function at the estimaed coefficients.
grad(func = arma_3_3_arima_like, x = arma_3_3_arima$coef, method="Richardson")

# Calculate the derivatives at the restricted model.
# Start with the full model for correct dimensions.
arma_3_3_arima_rest_coef <- arma_3_3_arima$coef
# Zero out the coefficient dropped from the restriction.
arma_3_3_arima_rest_coef['ar3'] <- 0
# Overwrite the remaining coefficients with the restricted estimates.
arma_3_3_arima_rest_coef[names(arma_2_3_arima$coef)] <- arma_2_3_arima$coef


# Check for accuracy.
arma_3_3_arima_rest_coef
arma_2_3_arima$coef


# Check the derivative of the likelihood wrt the parameters at the restricted estimates. 
grad(func = arma_3_3_arima_like, x = arma_3_3_arima_rest_coef)
# Note that the largest partial derivative is at the restricted parameter.



# Score vector is the first term in the Lagrange multiplier statistic.
# Requires the gradient vector in matrix form.
d_L_d_psi <- as.matrix(grad(func = arma_3_3_arima_like, x = arma_3_3_arima_rest_coef))


# Calculate the Lagrange multiplier statistic.
lm_stat <- t(d_L_d_psi) %*% (arma_3_3_arima$var.coef) %*% d_L_d_psi



# p-value for the likelihood ratio statistic.
lm_df <- 1
lm_p_value <- pchisq(lm_stat, df = wald_df, ncp = 0, lower.tail = FALSE)


# Test provides no evidence against the null hypothesis that the restriction holds.
lm_stat
lm_p_value



################################################################################
# Exercise: Simulation of an ARMA Model
################################################################################

#--------------------------------------------------------------------------------
# Set Additional Parameters for Simulation
#--------------------------------------------------------------------------------

# Set number of replications.
num_reps <- 999

# Initialize a data fram to store the results.
# arima_sim_stats <- data.frame()



#--------------------------------------------------------------------------------
# Run Simulation
#--------------------------------------------------------------------------------

for (rep_num in 1:num_reps) {
  
  # Generate a realization.
  
  
  # Estimate the model.
  
  
  # Calculate desired test statistics.
  # value_of_stat <- 42
  
  # Store the estimation results.
  # arima_sim_stats[rep_num, 'name_of_stat'] <- value_of_stat
  
}



#--------------------------------------------------------------------------------
# Plot a histogram of the test statistics.
#--------------------------------------------------------------------------------

# Summary statistics
# summary(arima_sim_stats)


# Plot a histogram of the test statistics.

# Hint: hist() function will plot a histogram.



#--------------------------------------------------------------------------------
# Comparison of Realized Statistics with Distribution
#--------------------------------------------------------------------------------

# Determine the ranking of the statistic from the sample within the distribution
# produced through the simulation.







################################################################################
# End
################################################################################
