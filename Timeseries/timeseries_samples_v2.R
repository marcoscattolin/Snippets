####
# http://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/src/timeseries.html
####

library(TTR)
library(forecast)

##### NON SEASONAL DATA #####


kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kingstimeseries <- ts(kings)
plot.ts(kingstimeseries)
# this time series could probably be described using an additive model, 
#since the random fluctuations in the data are roughly constant in size over time

#smoothing using a simple moving average of order 8
kingstimeseriesSMA8 <- SMA(kingstimeseries,n=8)
plot.ts(kingstimeseriesSMA8)


##### SEASONAL DATA ##########


births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
plot.ts(birthstimeseries)
### this time series could probably be described using an additive model, since the random fluctuations in the data are roughly constant in size over time

birthstimeseriescomponents <- decompose(birthstimeseries)
plot(birthstimeseriescomponents)

#the seasonal variation has been removed from the seasonally adjusted time series. The seasonally adjusted time series now just contains the trend component and an irregular component.
birthstimeseriesseasonallyadjusted <- birthstimeseries - birthstimeseriescomponents$seasonal
plot(birthstimeseriesseasonallyadjusted)


###### NON ADDITIVE MODEL ##########
souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))
plot.ts(souvenirtimeseries)
#an additive model is not appropriate for describing this time series, since the size
#of the seasonal fluctuations and random fluctuations seem to increase with the level
#of the time series. Thus, we may need to transform the time series
logsouvenirtimeseries <- log(souvenirtimeseries)
plot.ts(logsouvenirtimeseries)





##### Constant mean and no seasonality #########

#If you have a time series that can be described using an additive model
#with constant level and no seasonality, you can use simple exponential
#smoothing to make short-term forecasts

rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rainseries <- ts(rain,start=c(1813))
plot.ts(rainseries)
rainseriesforecasts <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
rainseriesforecasts
plot(rainseriesforecasts)
rainseriesforecasts2 <- forecast.HoltWinters(rainseriesforecasts, h=8)

#the 80% prediction interval as dark gray, and the 95% light gray
plot.forecast(rainseriesforecasts2)


#if there are correlations between forecast errors for successive predictions,
#it is likely that the simple exponential smoothing forecasts could be improved
#upon by another forecasting technique -- 
acf(rainseriesforecasts2$residuals, lag.max=20)



#The Ljung-Box test shows that there is little evidence
#of non-zero autocorrelations in the in-sample forecast errors
Box.test(rainseriesforecasts2$residuals, lag=20, type="Ljung-Box")
#test statistic is 17.4, and the p-value is 0.6,
#so we fail to reject H0: the data are independently distributed

#check whether the forecast errors are normally distributed
qqnorm(scale(rainseriesforecasts2$residuals))
abline(0,1,col="red")





##### Increasing or decreasing trend and no seasonality ####

#time series that can be described using an additive
#model with increasing or decreasing trend and no seasonality,
#you can use Holt???s exponential smoothing to make short-term forecasts
skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)
skirtsseries <- ts(skirts,start=c(1866))
plot.ts(skirtsseries)

skirtsseriesforecasts <- HoltWinters(skirtsseries, gamma=FALSE)
skirtsseriesforecasts
plot(skirtsseriesforecasts)

skirtsseriesforecasts2 <- forecast.HoltWinters(skirtsseriesforecasts, h=19)
plot.forecast(skirtsseriesforecasts2)


#check errors
acf(skirtsseriesforecasts2$residuals, lag.max=20)
Box.test(skirtsseriesforecasts2$residuals, lag=20, type="Ljung-Box")
qqnorm(scale(skirtsseriesforecasts2$residuals))
abline(0,1,col="red")




##### Increasing or decreasing trend and seasonality ####

#If you have a time series that can be described using an 
#additive model with increasing or decreasing trend and seasonality, 
#you can use Holt-Winters exponential smoothing to make short-term forecasts.

logsouvenirtimeseries <- log(souvenirtimeseries)
souvenirtimeseriesforecasts <- HoltWinters(logsouvenirtimeseries)
souvenirtimeseriesforecasts
plot(souvenirtimeseriesforecasts)
souvenirtimeseriesforecasts2 <- forecast.HoltWinters(souvenirtimeseriesforecasts, h=48)
plot.forecast(souvenirtimeseriesforecasts2)
