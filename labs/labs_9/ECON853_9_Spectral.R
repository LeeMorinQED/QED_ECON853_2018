################################################################################
# 
# ECON 853: Time Series Econometrics
# Spectral Estimation
# 
# Lee Morin, Ph.D.
# Adjunct Assistant Professor
# Queen's University
# 
# March 5, 2018
# 
################################################################################
# 
# This program provides examples of spectral estimation, providing analysis from
# the frequency domain, instead of the time domain, which is usual for ARMA models.
# These techniques allow for diagnosis of cyclical patterns in the data. 
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

# Load libraries designed for nonstationary time series.
# install.packages('urca')
# library(urca)

# Load library designed for fractionally integrated (long memory) processes.
# install.packages('arfima')
library(arfima)


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
# Load S&P 500 levels and create required variables.
#--------------------------------------------------------------------------------

# Load raw data.
data_file_name <- 'spx.csv'
data_path_file_name <- sprintf('%s/%s', data_path, data_file_name)
spx <- read.csv(data_path_file_name, sep = '\t')

summary(spx)

# Gererate returns and absolute returns.
spx[2:nrow(spx), 'return'] <- log(spx[2:nrow(spx), 'Adj.Close']) - log(spx[1:(nrow(spx) - 1), 'Adj.Close'])
spx[, 'abs_return'] <- abs(spx[, 'return'])
head(spx)
summary(spx)



################################################################################
# Spectral Density Estimation
################################################################################


# Levels seemed like unit root.
spectrum(spx[, 'Adj.Close'])
# See the peak at lowest frequency (longest-range dependence).
# Gradual decline in density for increasing frequency.

# Returns seemed closest to white noise.
spectrum(spx[2:nrow(spx), 'return'])
# White noise: equal density across all frequencies. 

# Absolute returns were an in-between case, with fractional cointegration. 
spectrum(spx[2:nrow(spx), 'abs_return'])
# Most weight at low frequencies (long memory).
# Compared to a unit root, there is a quick decline with increasing frequency.





################################################################################
# Spectral Density Estimation: Artificial data
################################################################################

# Set parameters common to all models.
set.seed(1234)
num_obs <- 1200
sigma_2 <- 1


#--------------------------------------------------------------------------------
# White noise and unit root.
#--------------------------------------------------------------------------------


# True White Noise.
wn <- rnorm(num_obs)
spectrum(wn)
# A flat spectrum for white noise.

# Pure unit root.
ur <- cumsum(wn)
spectrum(ur)
# More weight on low frequencies, declining for higher frequencies. 


#--------------------------------------------------------------------------------
# ARMA processes to show short-run dynamics.
#--------------------------------------------------------------------------------


# Autoregressive coefficients.
ar_coef <- c(0.6, 0.35)

# Moving average coefficients.
ma_coef <- c(0.60, 0.25, 0.55)


# Initialize time series.
time_series <- data.frame(time = 1:num_obs,
                          arma_2_3 = numeric(num_obs))

# Generate realization of AR(1) model.
time_series[, 'ar_1'] <- arima.sim(n = num_obs, list(ar = ar_coef[1]),
                                       sd = sqrt(sigma_2))

# Generate realization of MA(3) model.
time_series[, 'ma_3'] <- arima.sim(n = num_obs, list(ma = ma_coef),
                                   sd = sqrt(sigma_2))

# Generate realization of ARMA model.
time_series[, 'arma_2_3'] <- arima.sim(n = num_obs, list(ar = ar_coef, 
                                                         ma = ma_coef),
                                       sd = sqrt(sigma_2))

# AR(1)
spectrum(time_series[, 'ar_1'])
# More weight on low frequencies, gradually declining.
# Peak is less pronounced than for unit root. 

# MA(3)
spectrum(time_series[, 'ma_3'])
# More variability in spectrum, with low density for medium and high frequencies. 

# ARMA(2,3)
spectrum(time_series[, 'arma_2_3'])
# Similar, but with more density on lower frequencies, with addition of AR component. 

#--------------------------------------------------------------------------------
# Seasonality.
#--------------------------------------------------------------------------------

# Quarterly seasonality (assuming quarterly data).
qt_rep <- c(2, 3, 1, 4)
qt_plus_wn <- rep(qt_rep, round(num_obs/4)) + rnorm(num_obs)
spectrum(qt_plus_wn)
# Notice the spike at about 0.25. 
# The cycle is quarterly, repeating every 4 observations, out of a total of 1200. 
# So the cycle repeats 300 times, which is 25% of the sample size.



# Annual seasonality (assuming monthly data).
ann_rep <- c(seq(4, by = -1), rep(0, 2), seq(4), rep(4, 2))
ann_plus_wn <- rep(ann_rep, round(num_obs/4)) + rnorm(num_obs)
spectrum(ann_plus_wn)
# Notice the spike at about 0.08. 
# The cycle is monthly, repeating every 12 observations, out of a total of 1200. 
# So the cycle repeats 100 times, which is 1/12 = 8.333% of the sample size.


# Centennial seasonality (assuming annual data).
cent_rep <- c(seq(25, by = -1), rep(0, 25), seq(25), rep(25, 25))
cent_plus_wn <- rep(cent_rep, round(num_obs/100)) + rnorm(num_obs)
spectrum(cent_plus_wn)
# Spike at about 0.01.
# The cycle repeats every 100 observations, once a century, out of a total of 1200. 
# So the cycle repeats only 12 times, which is 1% of the sample size.


# Implications for modeling:
# Investigate seasonality or dependence at indicated frequencies. 


################################################################################
# End
################################################################################
