
# kmeans de stats----

# Se simulan datos aleatorios con dos dimensiones
datos <- matrix(rnorm(n = 100*2), nrow = 100, ncol = 2,
                dimnames = list(NULL,c("x", "y")))
datos <- as.data.frame(datos)

set.seed(101)
km_clusters <- kmeans(x = datos[, c("x", "y")], centers = 4, nstart = 50)

datos <- datos %>% mutate(cluster = km_clusters$cluster)
datos <- datos %>% mutate(cluster = as.factor(cluster),
                          grupo   = as.factor(grupo))

ggplot(data = datos, aes(x = x, y = y)) +
  geom_text(aes(label = cluster), size = 5, colour = datos$cluster) +
  theme_bw() +
  theme(legend.position = "none")


# Elbow method para obtencion de k y pca con 2 componentes para graficar clusters ====

# install.packages('factoextra')

data("USArrests") #data set de delitos
head(USArrests) 
str(USArrests)
datos <- scale(USArrests) #se escala porque las magnitudes son distintas

# se obtiene el k con elbow method (usar k means con un rango de k y ver donde se 
# da una reducción en la suma total de varianza intra-cluster)
library(factoextra)
fviz_nbclust(x = datos, FUNcluster = kmeans, method = "wss", k.max = 15, 
             diss = get_dist(datos, method = "euclidean"), nstart = 50)


set.seed(123)
km_clusters <- kmeans(x = datos, centers = 4, nstart = 50)

# El paquete factoextra permite visualizar los grupos, 
# automáticamente realiza un PCA y representa las dos primeras componentes principales 
# cuando la dimension del conjunto es mayor que dos.


# Las funciones del paquete factoextra emplean el nombre de las filas del
# dataframe que contiene los datos como identificador de las observaciones.
# Esto permite añadir labels a los gráficos.
fviz_cluster(object = km_clusters, data = datos, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +
  theme(legend.position = "none")

# PAM o kmedoids ----
# En PAM cada cluster está representado por una observación presente en el cluster (medoid), util cuando hay outliers


library(cluster)
library(factoextra)
fviz_nbclust(x = datos, FUNcluster = pam, method = "wss", k.max = 15,
             diss = dist(datos, method = "manhattan"))

pam_clusters <- pam(x = datos, k = 4, metric = "manhattan")
pam_clusters


fviz_cluster(object = pam_clusters, data = datos, ellipse.type = "t",
             repel = TRUE) +
  theme_bw() +
  labs(title = "Resultados clustering PAM") +
  theme(legend.position = "none")

#Grafico on los medoids

# Como hay más de 2 variables, se están representando las 2 primeras componentes
# de un PCA. Se tienen que calcular el PCA y extraer las proyecciones almacenadas
# en el elemento x.
medoids <- prcomp(datos)$x

# Se seleccionan únicamente las proyecciones de las observaciones que son medoids
medoids <- medoids[rownames(pam_clusters$medoids), c("PC1", "PC2")]
medoids <- as.data.frame(medoids)

# Se emplean los mismos nombres que en el objeto ggplot
colnames(medoids) <- c("x", "y")

# Creación del gráfico
fviz_cluster(object = pam_clusters, data = datos, ellipse.type = "t",
             repel = TRUE) +
  theme_bw() +
  # Se resaltan las observaciones que actúan como medoids
  geom_point(data = medoids, color = "firebrick", size = 2) +
  labs(title = "Resultados clustering PAM") +
  theme(legend.position = "none")

# Clara ----
# Clara es un pam pero con resampling, ocupa menos ram, util para cuando hay muchos datos


set.seed(1234)
grupo_1 <- cbind(rnorm(n = 200, mean = 0, sd = 8),
                 rnorm(n = 200, mean = 0, sd = 8))
grupo_2 <- cbind(rnorm(n = 300, mean = 30, sd = 8),
                 rnorm(n = 300, mean = 30, sd = 8))
datos <- rbind(grupo_1, grupo_2)
colnames(datos) <- c("x", "y")


library(cluster)
library(factoextra)
clara_clusters <- clara(x = datos, k = 2, metric = "manhattan", stand = TRUE,
                        samples = 50, pamLike = TRUE)


fviz_cluster(object = clara_clusters, ellipse.type = "t", geom = "point",
             pointsize = 2.5) +
  theme_bw() +
  labs(title = "Resultados clustering CLARA") +
  theme(legend.position = "none")





# Herarchical ----

set.seed(101)

data(USArrests)
datos <- scale(USArrests)

# Matriz de distancias euclídeas
mat_dist <- dist(x = datos, method = "euclidean")

# linkage: cuantifica la similitud entre dos clusters, puede ser: 
#   Complete or Maximum: La mayor de las distancias de todos los pares de puntos de dos clusters.
#   Single or Minimum: La menor de las distancias de todos los pares de puntos de dos clusters.
#   Average: promedio de todoas las distancias entre pares de puntos de dos clusters
#   Centroid: distancia entre centroides de dos clusters.
#   Ward: Se basa en una funcion objetivo para seleccionar los clusters a combinar. Por ejemplo: 
#         Ward’s minimum variance es un caso particular en el que el objetivo es minimizar la suma 
#         total de varianza intra-cluster


# Cluster jerarquico aglomerativo con linkage complete y average
hc_euclidea_complete <- hclust(d = mat_dist, method = "complete")
hc_euclidea_average  <- hclust(d = mat_dist, method = "average")

# Cophenetic es el coeficiente de correlación entre las distancias del dendrograma
# La correlacion entre el cophenetic y la matriz de distancia original, permite evaluar 
# hasta qué punto su estructura refleja las distancias originales entre observaciones.
# corr entre mas cercana a 1 mejor, 0.75 en adelante es bueno.

cor(x = mat_dist, cophenetic(hc_euclidea_complete))
cor(x = mat_dist, cophenetic(hc_euclidea_average)) #representar mejor la similitud entre observaciones.

# plot dendograma

hc_euclidea_completo <- hclust(d = dist(x = datos, method = "euclidean"),
                               method = "complete")

fviz_dend(x = hc_euclidea_completo, k = 2, cex = 0.6) +
  geom_hline(yintercept = 5.5, linetype = "dashed") +
  labs(title = "Herarchical clustering",
       subtitle = "Distancia euclídea, Lincage complete, K=2")


# cortar el arbol

# Donde cortar?: No hay una respuesta definitiva ya que el análisis de conglomerados es esencialmente 
# un enfoque exploratorio; La interpretación de la estructura jerárquica resultante depende del contexto 
# y, a menudo, varias soluciones son igualmente buenas desde un punto de vista teórico.

clusters <- cutree(tree = hc_euclidea_completo, k = 4) # por numero de grupo
clusters <- cutree(tree = hc_euclidea_completo, h = 6) # por altura del dendograma

# plot cluster jerarquico pca 2 componentes 

fviz_cluster(object = list(data=datos, cluster=cutree(hc_euclidea_completo, k=4)),
             ellipse.type = "convex", repel = TRUE, show.clust.cent = FALSE,
             labelsize = 8)  +
  labs(title = "Hierarchical clustering + Proyección PCA",
       subtitle = "Distancia euclídea, Lincage complete, K=4") +
  theme_bw() +
  theme(legend.position = "bottom")



# Cluster jerarquico divisivo

matriz_distancias <- dist(x = datos[, c("x", "y")], method = "euclidean")
hc_diana <- diana(x = matriz_distancias, diss = TRUE, stand = FALSE)

fviz_dend(x = hc_diana, cex = 0.5) +
  labs(title = "Hierarchical clustering divisivo",
       subtitle = "Distancia euclídea")



# Fuzzy c-means (FCM)
# La definición de centroide empleada por c-means es: la media de todas las observaciones del 
# set de datos ponderada por la probabilidad de pertenecer a al cluster

data("USArrests")
datos <- scale(USArrests)
library(cluster)

fuzzy_cluster <- fanny(x = datos, diss = FALSE, k = 3, metric = "euclidean",
                       stand = FALSE)

head(fuzzy_cluster$membership) #matriz con el grado de pertenencia a cada cluster

# coeficiente de partición Dunn normalizado y sin normalizar: 
# Normalizado próximos a 0 indican que la estructura tiene un alto nivel fuzzy.
fuzzy_cluster$coeff

#plot
library(factoextra)
fviz_cluster(object = fuzzy_cluster, repel = TRUE, ellipse.type = "norm",
             pallete = "jco") + theme_bw() + labs(title = "Fuzzy Cluster plot")



# Model based clustering ----
# clustering basado en modelos es de tipo fuzzy, considera que las observaciones proceden 
# de una distribución que es a su vez una combinación de dos o más componentes (clusters), 
# cada uno con una distribución propia.

# Para estimar los parámetros que definen la función de distribución de cada cluster 
# (media y matriz de covarianza si se asume que son de tipo normal) se recurre al algoritmo 
# de Expectation-Maximization (EM). Este resuelve distintos modelos en los que el volumen, 
# forma y orientación pueden ser distintos o iguales para cada cluster

install.packages('mclust')

library(mclust)
data("diabetes")
head(diabetes)

datos <- scale(diabetes[, -1])

# Model-based-clustering
model_clustering <- Mclust(data = datos, G = 1:10)
summary(model_clustering)

model_clustering$z # matriz de pertenencia a cada grupo

model_clustering$classification #clasificacion final


library(factoextra)
# Curvas del valor BIC en función del número de clusters para cada modelo.
# Atención al orden en el que se muestra la variable horizontal, por defecto es
# alfabético.
fviz_mclust(object = model_clustering, what = "BIC", pallete = "jco") +
  scale_x_discrete(limits = c(1:10))


fviz_mclust(model_clustering, what = "classification", geom = "point",
            pallete = "jco")

# Certeza de las clasificaciones. Cuanto mayor el tamaño del punto menor la
# seguridad de la asignación
fviz_mclust(model_clustering, what = "uncertainty", pallete = "jco")



# Density-based spatial clustering of applications with noise (DBSCAN) ----
# identificando regiones con alta densidad de observaciones separadas por regiones de baja densidad

#install.packages('fpc')
#install.packages('dbscan')

library(fpc)
library(dbscan)
library(factoextra)


data("multishapes")
datos <- multishapes[, 1:2]
set.seed(321)

#kmeans (no es efectivo aqui)
km_clusters <- kmeans(x = datos, centers = 5, nstart = 50)
fviz_cluster(object = km_clusters, data = datos, geom = "point", ellipse = FALSE,
             show.clust.cent = FALSE, pallete = "jco") +
  theme_bw() +
  theme(legend.position = "none")


# Selección del valor óptimo de epsilon. Como valor de minPts se emplea 5.
# epsilon (ϵ-neighborhood): radio que define la región vecina a una observación.

# Selección del valor óptimo de epsilon. Como valor de minPts se emplea 5.
dbscan::kNNdistplot(datos, k = 5) #punto de inflexión en torno a 0.15

# DBSCAN con epsilon = 0.15 y minPts = 5
dbscan_cluster <- fpc::dbscan(data = datos, eps = 0.15, MinPts = 5)

# Resultados de la asignación
head(dbscan_cluster$cluster)

# Visualización de los clusters
fviz_cluster(object = dbscan_cluster, data = datos, stand = FALSE,
             geom = "point", ellipse = FALSE, show.clust.cent = FALSE,
             pallete = "jco") +
  theme_bw() +
  theme(legend.position = "bottom")


# Comparación de dendrogramas ----
# puede ser: comparación visual y el cálculo de correlación entre dendrogramas


library(dendextend)
set.seed(123)
datos <- USArrests[sample(1:50, 10), ]

# Cálculo matriz de distancias
mat_dist <- dist(x = datos, method = "euclidean")

# Cálculo de hierarchical clustering
hc_average <- hclust(d = mat_dist, method = "average")
hc_ward    <- hclust(d = mat_dist, method = "ward.D2")

# Las funciones del paquete dendextend trabajan con objetos de tipo dendrograma,
# para obtenerlos se emplea la función as.dendogram()
dend_1 <- as.dendrogram(hc_average)
dend_2 <- as.dendrogram(hc_ward)


# tanglegram() representa dos dendrogramas a la vez, enfrentados uno al otro, y 
# conecta las hojas terminales con líneas. Los nodos que aparecen solo en uno de 
# los dendrogramas, es decir, que están formados por una combinación de observaciones 
# que no se da en el otro, aparecen destacados con líneas discontinuas.


tanglegram(dend1 = dend_1, dend2 = dend_2, highlight_distinct_edges = TRUE,
           common_subtrees_color_branches = TRUE)



# cor.dendlist() se puede calcular la matriz de correlación entre dendrogramas 
# basada en las distancias de Cophenetic o Baker.


# Se almacenan los dendrogramas a comparar en una lista
list_dendrogramas <- dendlist(dend_1, dend_2)
cor.dendlist(dend = list_dendrogramas, method = "cophenetic")



# Se pueden obtener comparaciones múltiples incluyendo más de dos dendrogramas en 
# la lista pasada como argumento
dend_1 <- datos %>% dist(method = "euclidean") %>% hclust(method = "average") %>%
  as.dendrogram()
dend_2 <- datos %>% dist(method = "euclidean") %>% hclust(method = "ward.D2") %>%
  as.dendrogram()
dend_3 <- datos %>% dist(method = "euclidean") %>% hclust(method = "single") %>%
  as.dendrogram()
dend_4 <- datos %>% dist(method = "euclidean") %>% hclust(method = "complete") %>%
  as.dendrogram()
list_dendrogramas <- dendlist("average" = dend_1, "ward.D2" = dend_2,
                              "single" = dend_3, "complete" = dend_4)
cor.dendlist(dend = list_dendrogramas, method = "cophenetic") %>% round(digits= 3)

# Si solo se comparan dos dendrogramas se puede emplear la función cor_cophenetic
cor_cophenetic(dend1 = dend_1, dend2 = dend_2)


# Estudio de la tendencia de clustering ----
# Permite evaluar si hay indicios de que realmente existe algún tipo de agrupación de los datos.
# puede ser por: Hopkins statistic o por Visual Assessment of cluster Tendency. 

library(purrr)

# Se elimina la columna que contiene la especie de planta
datos_iris <- iris[, -5]

# Se generan valores aleatorios dentro del rango de cada variable. Se utiliza la
# función map del paquete purrr.
datos_simulados <- map_df(datos_iris,
                          .f = function(x){runif(n = length(x),
                                                 min = min(x),
                                                 max = max(x))
                          }
)

# Estandarización de los datos
datos_iris      <- scale(datos_iris)
datos_simulados <- scale(datos_simulados)


# El estadístico Hopkins permite evaluar la tendencia de clustering de un conjunto de datos 
# mediante el cálculo de la probabilidad de que dichos datos procedan de una distribución uniforme, 
# es decir, estudia la distribución espacial aleatoria de las observaciones

# install.packages('clustertend')

library(clustertend)
set.seed(321)

# Estadístico H para el set de datos iris
hopkins(data = datos_iris, n = nrow(datos_iris) - 1)

# Estadístico H para el set de datos simulado
hopkins(data = datos_simulados, n = nrow(datos_simulados) - 1)

# el valor del estadístico H obtenido para el set de datos simulado es muy próximo a 0.5, 
# lo que indica que los datos están uniformemente distribuidos y desaconseja la utilización 
# de métodos de clustering.



# Visual Assessment of cluster Tendency (VAT) 
# Se representa gráficamente la matriz de distancias ordenada, empleando un gradiente de color 
# para el valor de las distancias. 


library(factoextra)
library(ggpubr)
dist_datos_iris      <- dist(datos_iris, method = "euclidean")
dist_datos_simulados <- dist(datos_simulados, method = "euclidean")

p1 <- fviz_dist(dist.obj = dist_datos_iris, show_labels = FALSE) +
  labs(title = "Datos iris") + theme(legend.position = "bottom")
p2 <- fviz_dist(dist.obj = dist_datos_simulados, show_labels = FALSE) +
  labs(title = "Datos simulados") + theme(legend.position = "bottom")

ggarrange(p1, p2)


# Número óptimo de clusters ----

# Elbow method: La idea general es probar un rango de valores del hiperparámetro k, 
# Calcula la varianza total intra-cluster en función del número de clusters y escoge 
# como óptimo aquel valor a partir del cual añadir más clusters apenas consigue mejoría. 
# osea minimizar el total inter-cluster sum of squares (wss).


library(factoextra)
datos <- scale(USArrests)
fviz_nbclust(x = datos, FUNcluster = kmeans, method = "wss", k.max = 15) +
  labs(title = "Número óptimo de clusters")

# La curva indica que a partir de 4 clusters la mejora es mínima.


# Average silhouette method: Parecido a Elbow, solo que ahora maximiza la media de 
# los silhouette coeficient. 
# Este coeficiente cuantifica cómo de buena es la asignación que se ha hecho de una observación 
# comparando su similitud con el resto de observaciones de su cluster frente a las de los otros 
# clusters. Su valor puede estar entre -1 y 1, siendo valores altos un indicativo de que la observación 
# se ha asignado al cluster correcto.

library(factoextra)
datos <- scale(USArrests)
fviz_nbclust(x = datos, FUNcluster = kmeans, method = "silhouette", k.max = 15) +
  labs(title = "Número óptimo de clusters")



# Gap statistic method: Este estadístico compara, para diferentes valores de k, la varianza total 
# intra-cluster observada frente al valor esperado acorde a una distribución uniforme de referencia.


library(factoextra)
datos <- scale(USArrests)
set.seed(896)
fviz_nbclust(x = datos, FUNcluster = kmeans, method = "gap_stat", nboot = 500,
             k.max = 15, verbose = FALSE, nstart = 50) +
  labs(title = "Número óptimo de clusters") 

# Ojo: muy denso de computar 

library(cluster)

set.seed(896)
kmeans_gap <- clusGap(x = datos, 
                      FUNcluster = kmeans,
                      K.max = 15,
                      B = 500,
                      verbose = FALSE,
                      nstart = 50)
kmeans_gap




# varios indices: NbClust entrega 30 índices distintos, identifica el valor en el 
# que coinciden más índices

# install.packages('NbClust')
library(factoextra)
library(NbClust)

datos <- scale(USArrests)
numero_clusters <- NbClust(data = datos, distance = "euclidean", min.nc = 2,
                           max.nc = 10, method = "kmeans", index = "alllong")

fviz_nbclust(numero_clusters)


# Calidad de los clusters ----


# Validación interna de los clusters: Emplean únicamente información interna del 
#   proceso de clustering para evaluar la bondad de las agrupaciones generadas. 
#   Se trata de un proceso totalmente unsupervised ya que no se incluye ningún tipo 
#   de información que no estuviese ya incluida en el clustering.'
#   para esto usar: silhouette, Índice Dunn, Medidas de estabilidad


#   Silhouette: Su valor puede estar entre -1 y 1, siendo 
#   valores altos un indicativo de que la observación se ha asignado al cluster correcto.
#   valores negativos apuntan a una posible asignación incorrecta.



library(factoextra)
# Se emplean los datos iris excluyendo la variable Species
datos <- scale(iris[, -5])
km_clusters <- eclust(x = datos, FUNcluster = "kmeans", k = 3, seed = 123,
                      hc_metric = "euclidean", nstart = 50, graph = FALSE)
fviz_silhouette(sil.obj = km_clusters, print.summary = TRUE, palette = "jco",
                ggtheme = theme_classic()) 

# En rojo el silhouette promedio, medida de bondad de las asignaciones del cluster. 


# Índice Dunn: ratio entre la separacion minima intercluster y la separacion maxima intracluster.
# Si la estructura contiene clusters compactos y bien separados, el numerador es grande y el denominador pequeño.
# Se busca maximizar este ratio.

library(fpc)
# Se emplean los datos iris excluyendo la variable Species
datos <- scale(iris[, -5])

# K-means clustering con k = 3
set.seed(321)
km_clusters <- kmeans(x = dist(datos, method = "euclidean"), centers = 3,
                      nstart = 50)
# Cálculo de índices (se calculan un total de 34 índices y parámetros)
km_indices <- cluster.stats(d = dist(datos, method = "euclidean"), 
                            clustering = km_clusters$cluster)

# Medidas de homogeneidad y separación
km_indices$average.within
km_indices$average.between

km_indices$dunn



# Validación externa de los clusters (ground truth): Combinan los resultados del clustering (unsupervised) 
# con información externa (supervised), como puede ser un set de validación en el que se conoce el verdadero 
# grupo al que pertenece cada observación. Permiten evaluar hasta qué punto el clustering es capaz de agrupar 
# correctamente las observaciones. Se emplea principalmente para seleccionar el algoritmo de clustering más adecuado, aunque su uso está limitado a escenarios en los que se dispone de un set de datos de validación.






# Significancia de los clusters: Calculan la probabilidad (p-value) de que los clusters generados se deban 
# únicamente al azar.




