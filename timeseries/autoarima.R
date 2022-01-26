#install.packages('forecast')
#install.packages('Metrics')
#install.packages('measures')


library(forecast)
library(dplyr)
library(ggplot2)
library(Metrics)
library(measures)

data <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
data_ts <- as.ts(data)

plot.ts(data_ts)

auto.arima(data_ts) %>% forecast(h=5) %>% autoplot()  #metodo autoarima

# Tratar series antes de usar forecast ----
# Para arima es necesario que la serie muestral que utilizamos para la estimación 
# sea estacionaria en media y varianza, osea precisamos que la serie no tenga tendencia, 
# y que presente un grado de dispersión similar en cualquier momento de tiempo (homogénea en varianza).
# A efectos prácticos, el cumplimiento de esta propiedad pasa por tomar logaritmos para homogeneizar 
# en varianza (no elimina la tendencia) y diferenciar adecuadamente la serie original objeto de estudio 
# para que sea estacionaria en media (si elimina la tendencia).


boxplot(data_ts)
hist(data_ts)

# log para quitar dispercion (estacionario en varianza)

data_ts_log <- log(data_ts)
plot.ts(data_ts_log)
hist(data_ts_log) #se observa menor dispercion, ya que los datos estan mas concentrados 


# diff para que sea estacionaria en media (quita tendencia)
data_ts_log_diff <- diff(data_ts_log)
plot.ts(data_ts_log_diff)

hist(data_ts_log_diff)



auto.arima(data_ts_log_diff) %>% forecast(h=5) %>% autoplot() 

data_ts_log_diff_forec <- auto.arima(data_ts_log_diff) %>% forecast(h=5)

forec_points = data_ts_log_diff_forec %>% as.data.frame() %>% select(fpoints='Point Forecast')

#revertir diff
# ultimo dato de la serie e ir sumando 

undiff <- function(a, d){
  #a = vector antes de hacer diff
  #d = vector de diff que sigue
  return(c(a,cumsum(c(a[length(a)], d))[-1]))
}

forec_points_undiff = undiff(a=data_ts_log, d=forec_points$fpoints)

#revetir log
plot.ts(exp(forec_points_undiff))


# arima sobre datos sin tratar
a = auto.arima(data_ts) %>% forecast(h=5) %>% as.data.frame() %>% select(x='Point Forecast')

# difencia entre los forecast (no son significativas en los 5 ultimos periodos)
exp(forec_points_undiff) - c(data, a$x)



# validacion ----

h=5

valid = head(data_ts, h)
model_data = head(data_ts, -h)


forec_points = auto.arima(data_ts) %>% forecast(h=h) %>% as.data.frame() %>% select(x='Point Forecast')


mae(valid, forec_points$x) #Scale-dependent errors
rmse(valid, forec_points$x) #Scale-dependent errors
MAPE(valid, forec_points$x) #Percentage errors
smape(valid, forec_points$x) #Percentage errors
mase(valid, forec_points$x) #Scaled errors


