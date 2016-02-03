# Time series example in R 
# from https://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/index.html

# this code designed for RStudio

souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")

souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))
souvenirtimeseries

plot.ts(souvenirtimeseries)

# additive model = fluctuations in the data are roughly constant in size over time.

# In this case, it appears that an additive model is not appropriate 
# for describing this time series, since the size of the seasonal 
# fluctuations and random fluctuations seem to increase with the level 
# of the time series. Thus, we may need to transform the time series 
# in order to get a transformed time series that can be described using 
# an additive model. For example, we can transform the time series by 
# calculating the natural log of the original data:

logsouvenirtimeseries <- log(souvenirtimeseries)
plot.ts(logsouvenirtimeseries)

# ensure package 'TTR' for time series analysis is installed
if(!require("TTR")){
  install.packages("TTR")
  require("TTR")
}

# simple moving average, order 3
logsouvenirSMA3 <- SMA(logsouvenirtimeseries,n=3)
plot.ts(logsouvenirSMA3)

# simple moving average, order 8
logsouvenirSMA8 <- SMA(logsouvenirtimeseries,n=8)
plot.ts(logsouvenirSMA8)

# estimate the trend, seasonal and irregular components of this time series
logsouvenirTScomponents <- decompose(logsouvenirtimeseries)

# look at seasonal component. Can also do this with $trend and $random
logsouvenirTScomponents$seasonal
plot.ts(logsouvenirTScomponents$seasonal)

# plot the estimated trend, seasonal, and irregular components of 
# the time series by using the “plot()” function
plot(logsouvenirTScomponents)

# seasonally adjust the time series by estimating the seasonal component, 
# and subtracting the estimated seasonal component from the original time series.
souvenirSeasonallyAdjusted <- logsouvenirtimeseries - logsouvenirTScomponents$seasonal
plot(souvenirSeasonallyAdjusted, main="Seasonally Adjusted")






