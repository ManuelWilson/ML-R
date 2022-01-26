#install.packages('R.matlab')
#install.packages('solitude')
#install.packages('MLmetrics')

library(R.matlab)   # Lectura de archivos .mat
library(h2o)        # Modelo isolation forest
library(solitude)   # Modelo isolation forest
library(tidyverse)  # Preparación de datos y gráficos
library(MLmetrics)  # Para matriz de confucion


cardio_mat  <- readMat("C:\\Users\\mwils\\Documents\\R\\using-ml\\anomalias\\cardio.mat")
df_cardio   <- as.data.frame(cardio_mat$X)
df_cardio$y <- as.character(cardio_mat$y) # anomalo = 1


datos <- df_cardio
#plot(datos[1,-length(datos)], type="b") # una serie



# Isolation forest H2O ----

h2o.init(ip = "localhost",
         # Todos los cores disponibles.
         nthreads = -1,
         # Máxima memoria disponible para el cluster.
         max_mem_size = "4g")

h2o.removeAll()
h2o.no_progress()

datos_h2o <- as.h2o(x = datos)


isoforest <- h2o.isolationForest(
  model_id = "isoforest",
  training_frame = datos_h2o,
  x              = colnames(datos_h2o)[-22],
  max_depth      = 350, # Profundidad máxima de los árboles
  ntrees         = 500, # Número de los árboles
  sample_rate    = 0.9 # Ratio de observaciones empleadas en cada árbol
)



predicciones_h2o <- h2o.predict(
  object  = isoforest,
  newdata = datos_h2o
)
predicciones <- as.data.frame(predicciones_h2o)
# En la práctica, suelen considerarse como potenciales outliers aquellas observaciones 
# cuya distancia predicha está por debajo de un determinado cuantil.

q <- quantile(x = predicciones$mean_length, probs = seq(0, 1, 0.05))

ggplot(data = predicciones, aes(x = mean_length)) +
  geom_histogram(color = "gray40") +
  geom_vline(
    xintercept = q,
    color      = "red",
    linetype   = "dashed") +
  labs(
    title = "Distribución de las distancias medias del Isolation Forest",
    subtitle = "Cuantiles marcados en rojo"  ) +
  theme_bw()



#Clasificar como anomalia 

datos <- datos %>%
  bind_cols(predicciones)



ggplot(data = datos,
       aes(x = y, y = mean_length)) +
  geom_jitter(aes(color = y), width = 0.03, alpha = 0.3) + 
  geom_violin(alpha = 0) +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0) +
  stat_summary(fun = "mean", colour = "darkgreen", size = 3, geom = "point") +
  labs(title = "Distancia promedio en el modelo Isolation Forest",
       x = "clasificación (0 = normal, 1 = anomalía)",
       y = "Distancia promedio") +
  theme_bw() + 
  theme(legend.position = "none")


# ordena y corta los 176 anomalos que se sabe de antemano
resultados <- datos %>%
  select(y, mean_length) %>%
  arrange(mean_length) %>%
  mutate(clasificacion = if_else(row_number() <= 176, "1", "0"))



mat_confusion <- MLmetrics::ConfusionMatrix(
  y_pred = resultados$clasificacion,
  y_true = resultados$y
)



#Isolation forest package Solitude ----

# install.packages('solitude')

library(solitude)
library(dplyr)
library(ggplot2)



# Modelo isolation forest
isoforest <- isolationForest$new(
  sample_size = as.integer(nrow(datos)/2),
  num_trees   = 500, 
  replace     = TRUE,
  seed        = 123
)
isoforest$fit(dataset = datos %>% select(-y))


predicciones <- isoforest$predict(
  data = datos %>% select(-y)
)
head(predicciones)


ggplot(data = predicciones, aes(x = average_depth)) +
  geom_histogram(color = "gray40") +
  geom_vline(
    xintercept = quantile(predicciones$average_depth, seq(0, 1, 0.1)),
    color      = "red",
    linetype   = "dashed") +
  labs(
    title = "Distribución de las distancias medias del Isolation Forest",
    subtitle = "Cuantiles marcados en rojo"  ) +
  theme_bw()


cuantiles <- quantile(x = predicciones$average_depth, probs = seq(0, 1, 0.05))
cuantiles


datos <- datos %>%
  bind_cols(predicciones)


ggplot(data = datos,
       aes(x = y, y = average_depth)) +
  geom_jitter(aes(color = y), width = 0.03, alpha = 0.3) + 
  geom_violin(alpha = 0) +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0) +
  stat_summary(fun = "mean", colour = "orangered2", size = 3, geom = "point") +
  labs(title = "Distancia promedio en el modelo Isolation Forest",
       x = "clasificación (0 = normal, 1 = anomalía)",
       y = "Distancia promedio") +
  theme_bw() + 
  theme(legend.position = "none")


resultados <- datos %>%
  select(y, average_depth) %>%
  arrange(average_depth) %>%
  mutate(clasificacion = if_else(row_number() <= 176, "1", "0"))


mat_confusion <- MLmetrics::ConfusionMatrix(
  y_pred = resultados$clasificacion,
  y_true = resultados$y
)
mat_confusion
