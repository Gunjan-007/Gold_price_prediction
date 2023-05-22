# Import and view the Data

# in my case i cleaned and converted the data from USD per OZ to INR per Gram 

gold_data<-read.csv(file="C:/Users/Gunjan/Desktop/project/monthly.csv")
gold_data

# Load the Libraries

library(forecast)
library(tseries)

# convert this data to a time-series data

gold.ts=ts(gold_data$gram,start=c(2000,1,1),end=c(2021,1,1),freq=12)

# Plot the Data as a line diagram and see how the data looks.

plot(gold.ts)

# check the auto-correlation function

acf(gold.ts)

# check the partial auto-correlation function

pacf(gold.ts)

# Augmented Dickey-Fuller Test (adf test)

adf.test(gold.ts)

# Cheeck if data is not stationary. convert the non stationary data to a stationary data.

# create a new model say gold.model

gold.model=auto.arima(gold.ts,ic="aic",trace=TRUE)

# check whether the data is stationary or not again.

gold.model

# check the acf(autocorrelation function) and pacf(partial autocorrelation function) for the residuals.

acf(ts(gold.model$residuals))

pacf(ts(gold.model$residuals))

# forecast we will use level of confidence =95 and 12 months each for next 5 years

my_gold_forecast=forecast(gold.model,level=c(95),h=5*12)

my_gold_forecast

# plot this data into the graph

plot(my_gold_forecast)

# validate the data using Box-Ljung test.

Box.test(my_gold_forecast$resid, lag=5, type= "Ljung-Box")

