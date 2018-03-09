################################################################################
# 
# ECON 853: Time Series Econometrics
# Introduction to Unit Roots
# 
# Lee Morin, Ph.D.
# Adjunct Assistant Professor
# Queen's University
# 
# February 8, 2018
# 
################################################################################
# 
# This program compares the empirical distribution, of OLS estimates of an AR(1)
# coefficient under a unit root, with the theoretical Dickey-Fuller distribution.
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
# OLS Estimation of the AR(1) Coefficient of a Unit Root
################################################################################

#--------------------------------------------------------------------------------
# Generate Unit root and estimate AR(1) coefficient
#--------------------------------------------------------------------------------

set.seed(1234)

# Generate realizations of the test statistic.
num_terms_ar1 <- 100
num_reps_ar1 <- 10000
t_stat_ar1 <- rep(NA, num_reps_ar1)

for (j in 1:num_reps_ar1){
  
  # Generate unit root.
  u <- rnorm(num_terms_ar1)
  y <- cumsum(u)
  
  # Run AR(1) regression.
  reg_ar1 <- lm(formula = y ~ + lag(y) - 1)
  
  # Calculate t-stat for the null hypothesis of a unit root.
  t_stat_ar1[j] <- (summary(reg_ar1)$coef[,"Estimate"] - 1 )/(summary(reg_ar1)$coef[,"Std. Error"])
  
}

hist(t_stat_ar1, breaks = 100,
     main = 'OLS AR(1) Coefficient for Unit Root',
     xlab = 'Realized Values of t-statistic')




#--------------------------------------------------------------------------------
# Generate Realizations from a Dickey-Fuller Distribution
#--------------------------------------------------------------------------------

num_reps_df <- 10000
num_terms_df <- 1000
t_stat_df <- rep(NA, num_reps_df)

for (j in 1:num_reps_df){
  
  # Generate standard Brownian motion.
  u <- rnorm(num_terms_df)/sqrt(num_terms_df)
  w <- cumsum(u)
  
  # Generate values of the DF distribution.
  df_num <- (w[num_terms_df]^2 - 1)/2
  df_denom <- sqrt(sum(w))
  
  # Calculate t-stat for the null hypothesis of a unit root.
  t_stat_df[j] <- df_num/df_denom
  
}

hist(t_stat_df, breaks = 100,
     main = 'Dickey-Fuller Distribution for Unit Root',
     xlab = 'Realized Values of t-statistic')



#--------------------------------------------------------------------------------
# Compare OLS Estimates with Dickey-Fuller Distribution
#--------------------------------------------------------------------------------


# Compare with (properly normalized) distribution of OLS t-statistic.
hist(t_stat_df, breaks = 100, freq = FALSE, col = 'blue',
     main = 'OLS AR(1) Coefficient and\nDickey-Fuller Distribution for Unit Root',
     xlab = 'Realized Values of t-statistic')
hist(t_stat_ar1/num_terms_ar1, breaks = 100, freq = FALSE, col = 'red',
     add = TRUE)



# Calculate Empirical CDF for comparison later.
edf_t_stat_ar1 <- ecdf(t_stat_ar1/num_terms_ar1)
edf_t_stat_df <- ecdf(t_stat_df)

plot(edf_t_stat_df,
     main = 'OLS AR(1) Coefficient and\nDickey-Fuller Distribution for Unit Root',
     xlab = 'Quantiles of t-statistic',
     col = 'blue', lwd = 3)
plot(edf_t_stat_ar1,
     col = 'red', lwd = 3,
     add = TRUE)


# For large samples, the empirical distribution will converge weakly to that of the DF distribution. 
# That is, the CDFs will be arbitrarily close in the limit.

# Try the OLS version with a series of longer length to see how slowly it converges. 
# This is something to think about when considering the sample size in your project. 
# Perhaps a simulation of the test statistic, under the null hypothesis that you are testing,
# is a worthwhile approach to make your results more convincing. 


################################################################################
# End
################################################################################
