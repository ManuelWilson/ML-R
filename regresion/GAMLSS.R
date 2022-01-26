# GAMLSS: Los modelos aditivos generalizados para posición, escala y forma. 
# son modelos de regresión semi-paramétricos.
# Paramétricos en cuanto a que requieren asumir que la variable respuesta sigue 
# una determinada distribución paramétrica definida por hasta 4 parámetros (??,??,??,??)
# que determinan su posición (p.ej. media), escala (p.ej. desviación estándar) y forma 
# (p. ej. skewness y kurtosis). y semi porque los parámetros de esta distribución pueden 
# ser modelados, cada uno de forma independiente, siguiendo funciones no paramétricas 
# (lineales, aditivas o no lineales).

# Son muchas las distribución que pueden emplearse para la variable respuesta y, f(yi|??i,??i,??i,??i), 
# la única condición es que el logaritmo de la función de densidad (logf(yi|??i,??i,??i,??i)) y 
# su primera derivada respecto a cada uno de los parámetros puedan calcularse.

#install.packages('gamlss')
#install.packages('skimr')

library(gamlss)
library(tidyverse)
library(ggpubr)
library(skimr)


data("rent")
datos <- rent
datos <- datos %>% select(R, Fl, A, H, loc)
# Se renombran las variables para que sean más explicativas
colnames(datos) <- c("precio", "metros", "anyo", "calefaccion", "situacion")
skim(datos)



p1 <- ggplot(data = datos, aes(x = metros, y = precio)) + 
  geom_point(alpha = 0.4) +
  labs(ttile = "Precio vs metros") +
  theme_bw()

p2 <- ggplot(data = datos, aes(x = anyo, y = precio)) + 
  geom_point(alpha = 0.4) +
  labs(ttile = "Precio vs año") +
  theme_bw()

p3 <- ggplot(data = datos, aes(x = calefaccion, y = precio)) + 
  geom_violin() +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  labs(ttile = "Precio vs calefacción") +
  theme_bw()

p4 <- ggplot(data = datos, aes(x = situacion, y = precio)) + 
  geom_violin() +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  labs(ttile = "Precio vs situación") +
  theme_bw()

ggpubr::ggarrange(
  plotlist = list(p1, p2, p3, p4)
) %>%
  ggpubr::annotate_figure(
    top = text_grob("Relación entre el precio y el resto de variables",
                    color = "Black",
                    face  = "bold",
                    size  = 14,
                    x     = 0.3)
  )



ggplot(data = datos, aes(x = precio)) +
  geom_density(alpha = 0.5, fill = "gray50") +
  geom_rug(alpha = 0.2) +
  labs(title = "Distribución del precio de los pisos") +
  theme_bw()


# LM 

modelo_lm <- gamlss(
  formula = precio ~ metros + anyo + calefaccion + situacion,
  family  = NO,
  data    = datos,
  trace   = FALSE
)

paste("El valor estimado de la varianza es:", exp(5.73165))

# Grafico Q-Q: método gráfico para el diagnóstico de diferencias entre la distribución de probabilidad 
# de una población de la que se ha extraído una muestra aleatoria y una distribución usada para la comparación.
# De dos conjuntos de datos a1, ...,an y b1, ..., bm se extraen cuartiles
# q_a_01, ..., q_a_99% y q_b_o1, ..., q_b_99% y se plotean los puntos 
# (q_a_01, q_b_o1) , ...,  (q_a_99%, q_b_99%). Entre mas cercano a una recta, las distribucions mas se parecen.



plot(modelo_lm)



# Worm plot de los residuos
# similar al grafico q-q, pero este muestra la desviacion vs quartiles 

wp(modelo_lm, ylim.all = 1)





