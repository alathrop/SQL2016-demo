#
# Time series example in R 
# code and background information from 
# https://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/index.html
# by Avril Chohlan
# organized in this example by Andy Lathrop, BlueGranite, Inc.

# Version .90R
# this version designed for use in R 
# other versions are available to run in different environments like AzureML and SQL Server

###
# package info
# ensure package 'pacman' for package management is installed
if (!require("pacman")) install.packages("pacman")

# Packages: "TTR" for time series functions, "forecast" for forecasting, 
# "ggplot2" and "ggfortify" for plotting. "ggfortify" requires the package "devtools"
# and is installed from github
pacman::p_load(TTR, forecast, ggplot2, devtools, dplyr)

# library(devtools)
install_github('sinhrks/ggfortify')
require(ggfortify)

### 
# load data

# this section included to identify source of original data and export to CSV ---
# initial data are a vector of dollar values of souvenir sales 
# souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
##### write original data to CSV
# write.table(souvenir, file="souvenir-data.csv", row.names=FALSE, col.names=FALSE)
# -------------------------------------------------------------------------------

# read from local CSV in working-directory
souvenir <- read.csv("souvenir-data.csv", header=FALSE, check.names=FALSE, stringsAsFactors=FALSE)

#####
# EDA - exploratory data analysis prior to forecasting
# convert data to ts (time series) object with monthly values starting in JAN 2008
souvenir.ts <- ts(souvenir, frequency=12, start=c(2009,1))
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

# create data frame for later use
logsouvenir.df <- data.frame(
  year  = as.numeric(floor(time(logsouvenir.ts))),
  month = as.numeric(cycle(logsouvenir.ts)),
  actual.sales = as.numeric(logsouvenir.ts) 
)

##################################
### Holt’s Exponential Smoothing #
##################################
# If you have a time series that can be described using an additive model with increasing or decreasing trend
# and no seasonality, you can use Holt’s exponential smoothing to make short-term forecasts.

# Holt’s exponential smoothing estimates the level and slope at the current time point. Smoothing is controlled
# by two parameters, alpha, for the estimate of the level at the current time point, and beta for the estimate 
# of the slope b of the trend component at the current time point. As with simple exponential smoothing, the
# paramters alpha and beta have values between 0 and 1, and values that are close to 0 mean that little weight
# is placed on the most recent observations when making forecasts of future values.

# To use HoltWinters() for Holt’s exponential smoothing, we need to set the parameter gamma=FALSE (the gamma
# parameter is used for Holt-Winters exponential smoothing)
logsouvenir.Holtforecasts <- HoltWinters(logsouvenir.ts, gamma=FALSE)
logsouvenir.Holtforecasts

# view fitted in-sample values
logsouvenir.Holtforecasts$fitted
plot(logsouvenir.Holtforecasts, main="Holt's Exponential Smoothing (in-sample)")

# calculate forecast (out-of-sample) values
logsouvenir.Holtforecasts2 <- forecast.HoltWinters(logsouvenir.Holtforecasts, h=12)
plot(logsouvenir.Holtforecasts2, main="Holt's Exponential Smoothing (12 months ahead)")

# print in-sample values
logsouvenir.Holtforecasts2$fitted

# print forecast (out-of-sample) values
logsouvenir.Holtforecasts2$mean

# create time series object of in-sample and forecast values
logsouvenir.Holt.ts <- ts(c(logsouvenir.Holtforecasts2$fitted,logsouvenir.Holtforecasts2$mean), 
                          frequency=12, start=c(2009,3))

# create data frame for later use
logsouvenir.Holt.df <- data.frame(
  year  = as.numeric(floor(time(logsouvenir.Holt.ts))),
  month = as.numeric(cycle(logsouvenir.Holt.ts)),
  Holt.sales = as.numeric(logsouvenir.Holt.ts) 
)

########################################
### Holt-Winters Exponential Smoothing #
########################################
# If you have a time series that can be described using an additive model with increasing or 
# decreasing trend and seasonality, you can use Holt-Winters exponential smoothing to make 
# short-term forecasts.

# Holt-Winters exponential smoothing estimates the level, slope and seasonal component at 
# the current time point. Smoothing is controlled by three parameters: alpha, beta, and gamma, 
# for the estimates of the level, slope b of the trend component, and the seasonal component, 
# respectively, at the current time point. The parameters alpha, beta and gamma all have 
# values between 0 and 1, and values that are close to 0 mean that relatively little weight 
# is placed on the most recent observations when making forecasts of future values.

logsouvenir.HoltWintersforecasts <- HoltWinters(logsouvenir.ts)
logsouvenir.HoltWintersforecasts

# The estimated values of alpha, beta and gamma are 0.41, 0.00, and 0.96, respectively. The 
# value of alpha (0.41) is relatively low, indicating that the estimate of the level at the 
# current time point is based upon both recent observations and some observations in the more 
# distant past. The value of beta is 0.00, indicating that the estimate of the slope b of the 
# trend component is not updated over the time series, and instead is set equal to its initial 
# value. This makes good intuitive sense, as the level changes quite a bit over the time series, 
# but the slope b of the trend component remains roughly the same. In contrast, the value 
# of gamma (0.96) is high, indicating that the estimate of the seasonal component at the 
# current time point is just based upon very recent observations.

plot(logsouvenir.HoltWintersforecasts, main="Holt-Winters Exponential Smoothing (in-sample)")

# We see from the plot that the Holt-Winters exponential method is very successful in predicting 
# the seasonal peaks, which occur roughly in December every year.

# Forecast future values. Note: the forecast for 48 months in the future is for illustration purposes only;
# the further in the future you forecast, the larger the prediction error. A shorter forecast horizon is
# generally recommended
logsouvenir.HoltWintersforecasts2 <- forecast.HoltWinters(logsouvenir.HoltWintersforecasts, h=48)
plot.forecast(logsouvenir.HoltWintersforecasts2, 
              main="Holt-Winters Exponential Smoothing (48 months ahead)")

# calculate forecast (out-of-sample) values
logsouvenir.HoltWintersforecasts2 <- forecast.HoltWinters(logsouvenir.HoltWintersforecasts, h=48)
plot(logsouvenir.HoltWintersforecasts2, main="Holt's Exponential Smoothing (12 months ahead)")

# print in-sample values
logsouvenir.HoltWintersforecasts2$fitted

# print forecast (out-of-sample) values
logsouvenir.HoltWintersforecasts2$mean

# create time series object of in-sample and forecast values
logsouvenir.HoltWinters.ts <- ts(c(logsouvenir.HoltWintersforecasts2$fitted,logsouvenir.HoltWintersforecasts2$mean), 
                          frequency=12, start=c(2009,13))

# create data frame for later use
logsouvenir.HoltWinters.df <- data.frame(
  year  = as.numeric(floor(time(logsouvenir.HoltWinters.ts))),
  month = as.numeric(cycle(logsouvenir.HoltWinters.ts)),
  HoltWinters.sales = as.numeric(logsouvenir.HoltWinters.ts) 
)

# -------------------
# create output table

insample.yr <- as.numeric(unique(floor(time(logsouvenir.ts))))
insample.mo <- as.numeric(unique(cycle(time(logsouvenir.ts))))

fcst.yr <- as.numeric(unique(floor(time(logsouvenir.HoltWinters.ts))))
fcst.mo <- as.numeric(unique(cycle(time(logsouvenir.HoltWinters.ts))))

yr.list <- sort(Reduce(union, c(insample.yr, fcst.yr)))
mo.list <- sort(Reduce(union, c(insample.mo, fcst.mo)))

yrs     <- unlist(lapply(yr.list, function(x) rep(x, length(mo.list))))
mos     <- rep(mo.list, length(yr.list))

# data frame of all dates
my.dates <- data.frame(
  year  = yrs,
  month = mos
)

# ------------------------------------------------------------
# write actuals and forecast from both methods to csv

# left_join function from dplyr package
actuals <- left_join(my.dates, logsouvenir.df)
actuals.Holt <- left_join(actuals, logsouvenir.Holt.df)
actuals.HoltWinters <- left_join(actuals.Holt, logsouvenir.HoltWinters.df)

write.csv(actuals.HoltWinters, file="souvenir-fcst.csv", row.names=FALSE)


