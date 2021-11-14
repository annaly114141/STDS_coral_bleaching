library(dplyr)
library(tidyverse)

data <- read.csv("Reef_Check_with_cortad_variables_with_annual_rate_of_SST_change1.csv")


data$New_date <- format(as.Date(data$Date, format="%d/%m/%y"),"%Y/%m/%d")
New_asdate <- as.Date(data$New_date, "%Y/%m/%d")

#convert to Date Format
myDate <- as.Date(data$New_date)
str(myDate)

data$ClimSST_Celcius <- data$ClimSST-273.15
data$Temp_Celcius <- data$Temperature_Kelvin-273.15
data$Temp_Max <- data$Temperature_Maximum-273.15
data$Temp_Min <- data$Temperature_Minimum-273.15

data <-data %>%filter(ClimSST_Celcius >0)
data <-data %>%filter(Temp_Celcius >0)

subdata <-subset(data,select=c('Month_Yr','Temp_Celcius')) %>% na.omit()


#Declare as TS
Y <- ts(data$Temp_Celcius, start = 1998, end = 2017, freq = 12) #12months per year

#Time Plot
autoplot(Y) +
  ggtitle("Time Plot: Change in Temperature Per Day") +
  ylab("Temperature Celcius")

#Take first difference of the data to remove the trend
DY <- diff(Y1)

# Preliminary analysis
autoplot(DY) +
  ggtitle("Time Plot: Change in Temperature Per Day") +
  ylab("Temperature Celcius")

#So we can see some seasonal patterns in the data
library(fpp2)
ggseasonplot(DY) +ggtitle("Seasonal Plot: Change in Daily Temperature")+
  ylab("Temperature_Celcius")


#Let's look at another seasonal plot, the subseries plot
#it is connecting year range 1998-2017 with a line, blue lines are the averages
#we can see there were significant perks of average change in temperature in November and December are more positive and negative compared to other months. Along with the positioning of the peaks, theese spikes were around in 2002 and 2010 when the mass coral bleaching events occurred

#the mean of the data is quite different
ggsubseriesplot(DY)

####################
# Our series, Y(adjusted) has trend and some seasonality
# To remove the trend, we take the first difference
#the first differences series has seasonality
#forecast with various methods
###################

#is or data stationary or non stationary
adf.test(data$Temp_Celcius)
adf.test(data$Temp_Celcius, k=2) #where k=lag
adf.test(data$Temp_Celcius, k=1)
adf.test(Y, k=1)

dY1<- diff(Y)
adf.test(dY1, k=12)

ndiffs(Y)

pp.test(Y)
pp.test(dY1)

#Use a benchmark method to forecast

#1. seasonal naive method, value of the data: y_t = y_{t-s} + e_t, use the differenced data
#we want acf to fall between the two dotted lines for 95 CI, otherwise there is better model out there
fit <- snaive(DY) #Residual SD = 5.492, this is our benchmark, a value in the current month is the smae as the value in the same month in the previous year. It is missing on average by 5.492 degrees
print(summary(fit))
checkresiduals(fit)

#2. Fit ETS method
fit_ets <- ets(Y) # Residual SD = 4.6401, better model
print(summary(fit_ets))
checkresiduals(fit_ets)

fc_ets <- forecast (Y,12)
autoplot(fc_ets)#plot forecast

#3. Fit ARIMA method
fit_arima <- auto.arima(Y, d=1, D=1, stepwise=FALSE, approximation = FALSE, trace=TRUE) # D=1 is first difference of data. fit the first difference of the data, D= remove seaspnolaity to remove the first deasonal difference
print(summary(fit_arima2))
checkresiduals(fit_arima2) #sigma^2 estimated as 12.31, sqrt(12.31) = 3.508561, look at residuals and SD
checkresiduals(fit_arima2)

#forecast with arima model
fcst <- forecast(fit_arima1, h= 48) #48 months ahead
autoplot(fcst, include=10)
print(summary(fcst))







