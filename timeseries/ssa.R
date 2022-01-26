#install.packages('Rssa')

library(Rssa)
library(forecast)
library(dplyr)
library(ggplot2)

data <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat") %>% as.ts()
plot(data)

# La SSA secuencial consta de dos etapas, en la primera etapa se extrae la 
# tendencia con una longitud de ventana pequeña y luego se detectan los componentes 
# periódicos y se extraen del residual con L = N / 2

s <- ssa(data, L = 120)
# L = longitudes de ventana que son divisibles por el período

plot(s)
plot(s,type="vector")
plot(s,type="paired")
plot(s,type="wcor")


recon <- reconstruct(s, groups = list(c(1,4), c(2, 3), c(5, 6)))
res <- residuals(recon) #errores al estimar 

plot(recon)
plot(recon, type = "cumsum")
plot(wcor(s, groups = list(c(1,4), c(2,3), c(5, 6))))



f <- forecast(s, groups = list(1:6), method = "recurrent" ,len=24, R = 10)
#f <- forecast(s, groups = list(1:6), method = "vector" ,len=24, R = 10)

# Plot the result including the last 24 points of the series
plot(f, include = 24, shadecols = "green", type = "l")


#plot
a = data %>% as.numeric() %>% as.data.frame() %>% mutate(type = 'original') %>% select(x='.', 'type')
b <- f %>% as.data.frame() %>% select(x = 'Point Forecast') %>% mutate(type = 'forecast')

c <- rbind(a, b)


ggplot(data = c) + 
  geom_point(aes(x = c(1:dim(c)[1]), y = x, colour = type)) +
  xlab("X") + ylab("Y")
  

