################################################################################
# 
# ECON 853: Time Series Econometrics
# Introductory Examples
# 
# Lee Morin, Ph.D.
# Adjunct Assistant Professor
# Queen's University
# 
# January 11, 2018
# 
################################################################################
# 
# This program provides introductory examples of R code for time series modeling.
# 
# 
# 
################################################################################


################################################################################
# Setup Workspace and Load Libraries
################################################################################

# Clear workspace.
rm(list=ls(all=TRUE))

# No libraries required.


################################################################################
# Example 1: First-order AutoRegressive model - AR(1)
################################################################################

#--------------------------------------------------------------------------------
# Set Parameters and Generate Data
#--------------------------------------------------------------------------------

phi_1_1 <- 0.75     # Autoregressive parameter
sigma_2 <- 1        # Variance of innovations
num_obs <- 100      # Number of observations
y_1_0 <- 7          # Initial value

# Initialize time series.
time_series <- data.frame(time = 1:num_obs,
                          epsilon_1 = rnorm(n = num_obs, mean = 0, sd = sigma_2),
                          ar_1 = numeric(num_obs))

time_series[1, 'ar_1'] <- y_1_0

# Generate series.
for (t in 2:num_obs) {
  
  time_series[t, 'ar_1'] <- phi_1_1*time_series[t - 1, 'ar_1'] + time_series[t, 'epsilon_1']
  
}


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




################################################################################
# Example 2: Second-order AutoRegressive model - AR(2)
################################################################################

#--------------------------------------------------------------------------------
# Set Parameters and Generate Data
#--------------------------------------------------------------------------------

phi_2_1 <- 0.60     # Autoregressive parameter
phi_2_2 <- 0.20     # Autoregressive parameter
y_2_0 <- c(4, 5)    # Initial values (first two observations)

# Append a new series of innovations.
time_series[, 'epsilon_2'] <- rnorm(n = num_obs, mean = 0, sd = sigma_2)

# Initialize time series.
time_series[1:2, 'ar_2'] <- y_2_0

# Generate series.
for (t in 3:num_obs) {
  
  time_series[t, 'ar_2'] <- phi_2_1*time_series[t - 1, 'ar_2'] + 
    phi_2_2*time_series[t - 2, 'ar_2'] + time_series[t, 'epsilon_2']
  
}


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



################################################################################
# Estimate the Coefficients
################################################################################


#--------------------------------------------------------------------------------
# Example 1: First-order AutoRegressive model - AR(1)
#--------------------------------------------------------------------------------

# Generate lagged values of the variable.
time_series[, 'ar_1_lag'] <- c(NA, time_series[1:(num_obs - 1), 'ar_1'])

head(time_series[, c('ar_1', 'ar_1_lag')])

# Specify the equation to estimate.
fmla_string <- 'ar_1 ~ ar_1_lag'
fmla <- as.formula(fmla_string)

ar_1_model <- lm(data = time_series, formula = fmla)

summary(ar_1_model)

#--------------------------------------------------------------------------------
# Example 2: Second-order AutoRegressive model - AR(2)
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



################################################################################
# End
################################################################################