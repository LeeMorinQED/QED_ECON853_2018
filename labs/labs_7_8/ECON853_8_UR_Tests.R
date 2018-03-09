################################################################################
# 
# ECON 853: Time Series Econometrics
# Unit Root Testing
# 
# Lee Morin, Ph.D.
# Adjunct Assistant Professor
# Queen's University
# 
# March 5, 2018
# 
################################################################################
# 
# This program provides examples of unit root tests.
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
library(tseries)

# install.packages('forecast')
# library(forecast)

# install.packages('FitARMA')
# library(FitARMA)

# Load libraries designed for nonstationary time series.
# install.packages('urca')
library(urca)


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
# Unit root tests of levels.
################################################################################

# Graphical inspection of series.
plot(spx[, 'Adj.Close'], 
     main = 'Levels of the S&P 500 Index, June 1991 - May 2012',
     ylab = 'S&P 500 Index Level',
     xlab = 'Date Index',
     type = 'l', col = 'blue', lwd = 3)


# Graphical inspection of autocorrelation.
acf(spx[, 'Adj.Close'], lag.max = 100, main = 'ACF of S&P 500 Index Level')
# Very high persistence of shocks. 
pacf(spx[, 'Adj.Close'], lag.max = 100, main = 'PACF of S&P 500 Index Level')
# Nothing beyond first lag, which is very near unity.
# Unit root or near unit root behaviour.

#--------------------------------------------------------------------------------
# Unit root tests of levels: urca package
#--------------------------------------------------------------------------------


# Start with a simple test from the urca package.
urca_df_none_0 <- ur.df(spx[, 'Adj.Close'], type = "none", lags = 0)
urca_df_none_0

summary(urca_df_none_0)
# No evidence to reject the null hypothesis of a unit root.

# Try to improve with a linear trend (expect higher (lower?) bar for stationarity).
urca_df_drift_0 <- ur.df(spx[, 'Adj.Close'], type = "drift", lags = 0)
summary(urca_df_drift_0)
# No evidence to reject the null hypothesis of a unit root.
# Neither is there evidence for a drift.

# One more try with lags to account for stationary autocorrelation of innovations.
urca_df_drift_5 <- ur.df(spx[, 'Adj.Close'], type = "drift", lags = 5)
summary(urca_df_drift_5)
# Still no evidence against null of unit root. Conclude that levels are nonstationary.


#--------------------------------------------------------------------------------
# Augmented Dickey-Fuller Unit root tests of levels: tseries package
#--------------------------------------------------------------------------------

adf.test(spx[, 'Adj.Close'])
# Note the default lag length is as high as the series can support. 
# Still no evidence against null of unit root. Conclude that levels are nonstationary.


# Next step: take differences (or log differences, as makes sense for your data).



################################################################################
# Unit root tests of returns.
################################################################################

# Generate return series.
spx[2:nrow(spx), 'return'] <- log(spx[2:nrow(spx), 'Adj.Close']) - log(spx[1:(nrow(spx) - 1), 'Adj.Close'])
head(spx)
summary(spx)

# Graphical inspection of series.
plot(spx[, 'return'], 
     main = 'Returns on the S&P 500 Index, June 1991 - May 2012',
     ylab = 'S&P 500 Index Returns',
     xlab = 'Date Index',
     type = 'l', col = 'green', lwd = 3)


# Graphical inspection of autocorrelation.
acf(spx[2:nrow(spx), 'return'], lag.max = 100, main = 'ACF of S&P 500 Index Returns')
# No persistence, compared to variance of series itself. 
pacf(spx[2:nrow(spx), 'return'], lag.max = 100, main = 'PACF of S&P 500 Index Returns')
# Some evidence for serial correlation but series is very noisy (variance improperly estimated?).
# Clearly no strong evidence of a unit root, at least.


#--------------------------------------------------------------------------------
# Unit root tests of returns: urca package.
#--------------------------------------------------------------------------------


# Start with a simple test from the urca package.
urca_df_none_0 <- ur.df(spx[2:nrow(spx), 'return'], type = "none", lags = 0)
summary(urca_df_none_0)
# Strong evidence to reject the null hypothesis of a unit root.

# Try to improve with a linear trend (expect higher (lower?) bar for stationarity).
urca_df_drift_0 <- ur.df(spx[2:nrow(spx), 'return'], type = "drift", lags = 0)
summary(urca_df_drift_0)
# Very strong evidence to reject the null hypothesis of a unit root.
# No evidence for a drift.

# One more try with lags to account for stationary autocorrelation of innovations.
urca_df_drift_5 <- ur.df(spx[2:nrow(spx), 'return'], type = "drift", lags = 5)
summary(urca_df_drift_5)
# Still strong evidence against null of unit root. Conclude that returns are stationary.

#--------------------------------------------------------------------------------
# Augmented Dickey-Fuller Unit root tests of levels: tseries package
#--------------------------------------------------------------------------------

adf.test(spx[2:nrow(spx), 'return'])
# Still strong evidence against null of unit root. Conclude that returns are stationary.
# Warning message to alert you to the evidence that the series is stationary.



# Is it as simple as a unit root, from integrated whte noise?
# See what happens next.


################################################################################
# Unit root tests of absolute returns.
################################################################################

# Gererate absolute returns.
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
# What does the unit root test tell us?


#--------------------------------------------------------------------------------
# Unit root tests of absolute returns: urca package.
#--------------------------------------------------------------------------------


# Start with a simple test from the urca package.
urca_df_none_0 <- ur.df(spx[2:nrow(spx), 'abs_return'], type = "none", lags = 0)
summary(urca_df_none_0)
# Strong evidence to reject the null hypothesis of a unit root.

# Try to improve with a linear trend (expect higher (lower?) bar for stationarity).
urca_df_drift_0 <- ur.df(spx[2:nrow(spx), 'abs_return'], type = "drift", lags = 0)
summary(urca_df_drift_0)
# Very strong evidence to reject the null hypothesis of a unit root.
# No evidence for a drift.

# One more try with lags to account for stationary autocorrelation of innovations.
urca_df_drift_5 <- ur.df(spx[2:nrow(spx), 'abs_return'], type = "drift", lags = 5)
summary(urca_df_drift_5)
# Less strong evidence against null of unit root, but still strong evidence against. 
# Conclude that absolute returns are stationary.

# Try with an information-based lag selection criterion.
urca_df_none_BIC <- ur.df(spx[2:nrow(spx), 'abs_return'], type = "none", selectlags = 'BIC')
summary(urca_df_none_BIC)


#--------------------------------------------------------------------------------
# Augmented Dickey-Fuller Unit root tests of levels: tseries package
#--------------------------------------------------------------------------------

adf.test(spx[2:nrow(spx), 'abs_return'])
# Less strong evidence against null of unit root, but still strong evidence against. 
# Conclude that absolute returns are stationary.




################################################################################
# End
################################################################################