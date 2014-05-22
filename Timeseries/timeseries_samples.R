library(forecast)

#generate data
x <- 1:(12*4)
t <- seq(0,2*pi,length.out = 13)
y <- 0.05*sin(t)
y <- y[1:12]
y <- append(y,c(y,y,y))
lines(x,y)
tseries <- ts(y, frequency=11)

plot(tseries)

#decompose with seasonality
fit <- stl(tseries, s.window="periodic")
plot(fit)


#Exponential smoothing forecast
fcst <- ets(tseries)
accuracy(fcst)
prediction <- forecast(fcst,3)
plot(prediction)

#ARIMA
fit <- auto.arima(tseries)
plot(forecast(fit, 5))

#HoltWinters
fit <- auto.arima(tseries)
plot(forecast(fit, 15))



#Autocorrelation
a <- acf(tseries)
