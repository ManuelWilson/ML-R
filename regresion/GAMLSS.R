# GAMLSS: Los modelos aditivos generalizados para posici�n, escala y forma. 
# son modelos de regresi�n semi-param�tricos.
# Param�tricos en cuanto a que requieren asumir que la variable respuesta sigue 
# una determinada distribuci�n param�trica definida por hasta 4 par�metros (??,??,??,??)
# que determinan su posici�n (p.ej. media), escala (p.ej. desviaci�n est�ndar) y forma 
# (p. ej. skewness y kurtosis). y semi porque los par�metros de esta distribuci�n pueden 
# ser modelados, cada uno de forma independiente, siguiendo funciones no param�tricas 
# (lineales, aditivas o no lineales).

# Son muchas las distribuci�n que pueden emplearse para la variable respuesta y, f(yi|??i,??i,??i,??i), 
# la �nica condici�n es que el logaritmo de la funci�n de densidad (logf(yi|??i,??i,??i,??i)) y 
# su primera derivada respecto a cada uno de los par�metros puedan calcularse.

#install.packages('gamlss')
#install.packages('skimr')

library(gamlss)
library(tidyverse)
library(ggpubr)
library(skimr)


data("rent")
datos <- rent
datos <- datos %>% select(R, Fl, A, H, loc)
# Se renombran las variables para que sean m�s explicativas
colnames(datos) <- c("precio", "metros", "anyo", "calefaccion", "situacion")
skim(datos)



p1 <- ggplot(data = datos, aes(x = metros, y = precio)) + 
  geom_point(alpha = 0.4) +
  labs(ttile = "Precio vs metros") +
  theme_bw()

p2 <- ggplot(data = datos, aes(x = anyo, y = precio)) + 
  geom_point(alpha = 0.4) +
  labs(ttile = "Precio vs a�o") +
  theme_bw()

p3 <- ggplot(data = datos, aes(x = calefaccion, y = precio)) + 
  geom_violin() +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  labs(ttile = "Precio vs calefacci�n") +
  theme_bw()

p4 <- ggplot(data = datos, aes(x = situacion, y = precio)) + 
  geom_violin() +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  labs(ttile = "Precio vs situaci�n") +
  theme_bw()

ggpubr::ggarrange(
  plotlist = list(p1, p2, p3, p4)
) %>%
  ggpubr::annotate_figure(
    top = text_grob("Relaci�n entre el precio y el resto de variables",
                    color = "Black",
                    face  = "bold",
                    size  = 14,
                    x     = 0.3)
  )



ggplot(data = datos, aes(x = precio)) +
  geom_density(alpha = 0.5, fill = "gray50") +
  geom_rug(alpha = 0.2) +
  labs(title = "Distribuci�n del precio de los pisos") +
  theme_bw()


# LM 

modelo_lm <- gamlss(
  formula = precio ~ metros + anyo + calefaccion + situacion,
  family  = NO,
  data    = datos,
  trace   = FALSE
)

paste("El valor estimado de la varianza es:", exp(5.73165))

# Grafico Q-Q: m�todo gr�fico para el diagn�stico de diferencias entre la distribuci�n de probabilidad 
# de una poblaci�n de la que se ha extra�do una muestra aleatoria y una distribuci�n usada para la comparaci�n.
# De dos conjuntos de datos a1, ...,an y b1, ..., bm se extraen cuartiles
# q_a_01, ..., q_a_99% y q_b_o1, ..., q_b_99% y se plotean los puntos 
# (q_a_01, q_b_o1) , ...,  (q_a_99%, q_b_99%). Entre mas cercano a una recta, las distribucions mas se parecen.



plot(modelo_lm)



# Worm plot de los residuos
# similar al grafico q-q, pero este muestra la desviacion vs quartiles 

wp(modelo_lm, ylim.all = 1)





