library(forecast)

data("AirPassengers")

class(AirPassengers)

start(AirPassengers)

end(AirPassengers)

frequency(AirPassengers)

sum(is.na(AirPassengers)) # check missing value

summary(AirPassengers)

AirPassengers

# Explore Data Analysis
tsdata <- ts(AirPassengers,frequency = 12)

ddata <- decompose(tsdata, "multiplicative")

plot(ddata)

plot(ddata$trend)

plot(ddata$random) # nilai yang ngga bisa dibuat persamaan.

##
plot(AirPassengers) # ini data original
abline(reg=lm(AirPassengers~time(AirPassengers))) # garis trend
cycle(AirPassengers)

# get boxplot by cycle
boxplot(AirPassengers~cycle(AirPassengers, xlab= "Date", ylab = "Passenger Numbers (1000's)",
                            main="Monthly air passenger boxplot from 1949-1960"))

# stationarity
plot(AirPassengers)

# ask R for the best model
mymodel <- auto.arima(AirPassengers) # AIC = accuracy for the model
mymodel

# run trace to compare information
auto.arima(AirPassengers, ic="aic", trace = T)

# instal.pacakges (tseries)
library(tseries)

plot.ts(mymodel$residuals)

acf(ts(mymodel$residuals), main="ACF Residual")
pacf(ts(mymodel$residuals), main="PACF Residual")

# use model to forecast for the next 10 years
myforecast <- forecast(mymodel,level = c(95),h=10*12)
plot(myforecast)

# validasi model dengan "Ljung-Box"
Box.test(mymodel$residuals,lag=5,type="Ljung-Box")
Box.test(mymodel$residuals,lag=10,type="Ljung-Box")
Box.test(mymodel$residuals,lag=15,type="Ljung-Box")

# karena nilai p-value nya kecil maka model bisa diterima

# coba dengan data dari nanjung
summary(nanjung_rename$Value)
tsdata <- ts(nanjung_rename$Value, frequency = 12) 
ddata <- decompose(tsdata, "multiplicative")
plot(ddata)
plot(ddata$trend)
plot(ddata$seasonal)
