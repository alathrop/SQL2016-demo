# Time series example in R 
# from https://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/index.html

# this code designed for RStudio

# there are 2 sections to view different time series
# manipulations and plotting: 1. basic and 2. ggplot

### package info
# ensure package 'pacman' for package management is installed
if (!require("pacman")) install.packages("pacman")

# Packages: "TTR" for time series, "forecast" for forecasting, ggplot2 for plotting
pacman::p_load(TTR, forecast, ggplot2)

### load data
# initial data are a vector of sales values
souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")

### basic wrangling, plotting, and forecasting
# uses 'TTR' and 'forecast' package
# convert data to ts (time series) object
souvenir.ts <- ts(souvenir, frequency=12, start=c(1987,1))
souvenir.ts
plot.ts(souvenir.ts)

# additive model = fluctuations in the data are roughly constant in size over time.
# In this case, it appears that an additive model is not appropriate 
# for describing this time series, since the size of the seasonal 
# fluctuations and random fluctuations seem to increase with the level 
# of the time series. Thus, we may need to transform the time series 
# in order to get a transformed time series that can be described using 
# an additive model. For example, we can transform the time series by 
# calculating the natural log of the original data:

logsouvenir.ts <- log(souvenir.ts)
plot.ts(logsouvenir.ts)

# simple moving average, order 3
logsouvenir.SMA3 <- SMA(logsouvenir.ts,n=3)
plot.ts(logsouvenir.SMA3)

# plot simple moving average (order 8), with original logsouvenir overlay 
logsouvenir.SMA8 <- SMA(logsouvenir.ts,n=8)
plot.ts(logsouvenir.SMA8, type="l", col="red")
lines(logsouvenir.ts, col="black")
points(logsouvenir.SMA8, col="red")

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






