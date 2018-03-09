################################################################################
# 
# ECON 853: Time Series Econometrics
# Long Memory Processes
# 
# Lee Morin, Ph.D.
# Adjunct Assistant Professor
# Queen's University
# 
# March 5, 2018
# 
################################################################################
# 
# This program provides examples of long memory processes.
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

# install.packages('fracdiff')
library(fracdiff)


# Set working directory.
wd_path <- 'C:/Users/iky155/Documents/Repos/ECON853w2018_repo_2'
# wdPath <- '/path/to/your/folder'
setwd(wd_path)


# Set path for S&P 500 dataset.
data_path <- sprintf('%s/Data', wd_path)



################################################################################
# Load S&P 500 levels.
################################################################################

data_file_name <- 'spx.csv'
data_path_file_name <- sprintf('%s/%s', data_path, data_file_name)
spx <- read.csv(data_path_file_name, sep = '\t')

summary(spx)


################################################################################
# Analysis of absolute returns.
################################################################################

# Gererate returns and absolute returns.
spx[2:nrow(spx), 'return'] <- log(spx[2:nrow(spx), 'Adj.Close']) - log(spx[1:(nrow(spx) - 1), 'Adj.Close'])
spx[, 'abs_return'] <- abs(spx[, 'return'])
head(spx)
summary(spx)

# Graphical inspection of series.
plot(spx[, 'abs_return'], 
     main = 'Absolute Returns on the S&P 500 Index, June 1991 - May 2012',
     ylab = 'S&P 500 Index Absolute Returns',
     xlab = 'Date Index',
     type = 'l', col = 'red', lwd = 3)


# Graphical inspection of autocorrelation.
acf(spx[2:nrow(spx), 'abs_return'], lag.max = 100, main = 'ACF of S&P 500 Index Absolute Returns')
# Very high persistence, except that coefficient does not appear to be very close to one. 
acf(spx[2:nrow(spx), 'abs_return'], lag.max = 500, main = 'ACF of S&P 500 Index Absolute Returns')
# Unusually high persistence that persists for hudreds of lags.
pacf(spx[2:nrow(spx), 'abs_return'], lag.max = 100, main = 'PACF of S&P 500 Index Absolute Returns')
# Partial autocorrelation declines fairly slowly, for nearly 20 lags.
# Still a high degree of persistence of shocks, although not as high as implied by the ACF.


# Interesting case, high persistence of shock lasting for hundreds of lags (like a unit root).
# However, AR coefficients seem to be much lower than 1, the (apparent) threshold for stationarity. 
# Recall that unit root tests implied that series is stationary.
# But it's not a perfect fit. 
# We will attempt to reproduce this behaviour below.


################################################################################
# Long memory: Fractional Integration.
################################################################################

#--------------------------------------------------------------------------------
# Inspection of artificial data.
# Using the fracdiff package.
#--------------------------------------------------------------------------------


set.seed(1234)
num_obs <- 1000

# Generate a fractionally differenced series.
long_mem_4 <- fracdiff.sim(n = num_obs, ar = NULL, ma = NULL, d = 0.4)

attributes(long_mem_4)

# Inspect the autocorrelation structure.
acf(long_mem_4$series, lag.max = 100)
# Long persistence, into nearly 100 lags. 
pacf(long_mem_4$series, lag.max = 100)
# Reasonably quick decline to zero.

# Estimate the fractional differencing order.
est_frac_diff_4 <- fracdiff(long_mem_4$series)

summary(est_frac_diff_4)
# Looks fairly accurate.

# Appears to generate the behaviour in the absolute return series.



#--------------------------------------------------------------------------------
# Using the arfima package.
#--------------------------------------------------------------------------------

# Another option is the arfima package, but I found it slower. 
# However, it seems to have more guardrails for reliable esimation.

set.seed(8564)
sim_arfima_2_d_1 <- arfima.sim(1000, model = list(phi = c(0.2, 0.1),
                                                  dfrac = 0.4, 
                                                  theta = 0.9))

# Inspect series for evidence of stationarity.
summary(sim_arfima_2_d_1)
acf(sim_arfima_2_d_1)
pacf(sim_arfima_2_d_1)


arfima_2_d_1 <- arfima(sim_arfima_2_d_1, order = c(2, 0, 1), back=T)
arfima_2_d_1



# Plot the theoretical ACF for the fitted long memory model.
tacfplot(fits = arfima_2_d_1, maxlag = 20, lag0 = TRUE)



################################################################################
# Fitting an ARFIMA model to the Absolute Return Series
################################################################################

# Estimate the fractionally integrated version of the ARMA model.
arfima_spx <- fracdiff(spx[2:nrow(spx), 'abs_return'], drange = c(0, 0.8))

summary(arfima_spx)

coef(arfima_spx)

# Fractionally difference the series with estimated parameter.
resid_spx <- diffseries(x = spx[2:nrow(spx), 'abs_return'], d = coef(arfima_spx))

# Analyze the residuals of the estimated model. 
plot(resid_spx, 
     main = 'Fractionally Differenced Absolute Returns on the S&P 500 Index',
     ylab = 'Residuals',
     xlab = 'Date Index',
     type = 'l', col = 'red', lwd = 3)


# Autocorrelation function has only short-run dynamics.
acf(resid_spx)
pacf(resid_spx)
# Model might include some MA terms.
# Much more well-behaved than before: appears to be stationary.




################################################################################
# End
################################################################################

