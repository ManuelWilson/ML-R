library(ggplot2)
library(dplyr)
library(h2o)

x = c(5, 6, 5, 3, 4, 2, 3, 5, 3, 7, 
      20, 18, 19, 21, 17, 18, 19, 22)
y = c(20, 21, 23, 22, 20, 23, 24, 25, 20, 21, 
      5, 3, 6, 4, 7, 5, 2, 4)
e = c('a', 'a', 'a','b','a','b', 'b', 'b', 'b', 'a', 
      'a', 'a', 'a', 'b', 'b', 'b', 'b', 'b')

dataset = data.frame(x, y , e = e %>% as.factor())


ggplot(data = dataset) + 
  geom_point(aes(x, y, colour = factor(e))) 


# h2o clustering

h2o.init()

dataset_h2o = dataset %>% as.h2o()

h2o_kmeans <- h2o.kmeans(k = 4,
                          estimate_k = FALSE,
                          standardize = FALSE,
                          seed = 1234,
                          x = colnames(dataset),
                          training_frame = dataset_h2o, 
                          score_each_iteration = TRUE)


pred <- h2o.predict(h2o_kmeans, newdata = dataset_h2o) %>% as.data.frame()

centroids = h2o_kmeans@model$centers %>% as.data.frame()


ggplot() + 
  geom_point(aes(dataset$x, dataset$y, colour = dataset$e)) + 
  geom_point(aes(centroids$x, centroids$y), colour = 'black') +
  geom_point(
    aes(dataset$x, dataset$y, colour = pred$predict  %>% as.factor(), size = 5), shape=1) 


#conclusion:  al parecer le esta dando mas importancia a la distancia 'numerica' que a la categorica, 
#             falta una forma de ponderar ambas distancias, ni idea como realiza este proceso h2o.  






