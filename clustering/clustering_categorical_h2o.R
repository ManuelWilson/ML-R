a = c('a', 'a', 'a', 'a', 'b', 'b', 'b', 'x')
b = c('c', 'd', 'c', 'd', 'c', 'd', 'd', 'y')
c = c('e', 'e', 'f', 'f', 'e', 'e', 'f', 'z')

dataset = data.frame(a = a %>% as.factor(), b = b %>% as.factor(), c = c %>% as.factor())


# h2o clustering

h2o.init()

dataset_h2o = dataset %>% as.h2o()

h2o_kmeans <- h2o.kmeans(k = 3,
                         estimate_k = FALSE, # Cannot estimate k if data has no numeric columns
                         standardize = FALSE,
                         seed = 1234,
                         x = colnames(dataset),
                         training_frame = dataset_h2o, 
                         score_each_iteration = TRUE)

pred <- h2o.predict(h2o_kmeans, newdata = dataset_h2o) %>% as.data.frame()

dataset %>% cbind(pred)


#Conclusion: asuo que esta usando distancias miss matching