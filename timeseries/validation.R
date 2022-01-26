
library(Rssa)
library(forecast)
library(dplyr)
library(ggplot2)
library(Metrics)


data <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat") %>% as.ts()

validation_period = 3


head(data)

data_i = data[-c((length(data)-validation_period) :length(data))]
valid_data_i = data[c((length(data)-validation_period+1) :length(data))]

s <- ssa(data)
f <- forecast(s, groups = list(1:6), method = "recurrent" ,len=validation_period, R = 10)

mape(f$mean %>% as.numeric(), valid_data_i) * 100 #Error absoluto medio porcentual

