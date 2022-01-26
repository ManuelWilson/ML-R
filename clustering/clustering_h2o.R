#install.packages('h2o')
#install.packages('ggplot2')
#install.packages('dplyr')

library(ggplot2)
library(dplyr)
library(h2o)

h2o.init()

# Import the iris dataset into H2O:
iris <- h2o.importFile("http://h2o-public-test-data.s3.amazonaws.com/smalldata/iris/iris_wheader.csv")

# Set the predictors:
predictors <- c("sepal_len", "sepal_wid", "petal_len", "petal_wid")


# supervise data to valid ----

# Split the dataset into a train and valid set:
iris_split <- h2o.splitFrame(data = iris, ratios = 0.8, seed = 1234)
train <- iris_split[[1]]
valid <- iris_split[[2]]

# Build and train the model:
iris_kmeans <- h2o.kmeans(k = 10,
                          estimate_k = TRUE, #Use PRE error to stop (calculate based on SSW)
                          standardize = FALSE,
                          seed = 1234,
                          x = predictors,
                          training_frame = train,
                          validation_frame = valid)

#use view(iris_kmeans) to view element of s4 object, for example how get centroids iris_kmeans@model$centers

# Eval performance:
perf <- h2o.performance(iris_kmeans)

# Generate predictions on a validation set (if necessary):
pred <- h2o.predict(iris_kmeans, newdata = valid)



# no supervise data to valid ----

iris_kmeans <- h2o.kmeans(k = 10,
                          estimate_k = TRUE,
                          standardize = FALSE,
                          seed = 1234,
                          x = predictors,
                          training_frame = iris)


View(iris_kmeans@model$model_summary)




#within_cluster_sum_of_squares; is a measure of the variability of the observations WHITIN each cluster
#total_sum_of_squares
#between_cluster_sum_of_squares; measures how far apart the centroids of the clusters in the final partition are from one another.



#plot number of iterations vs within cluster sum of squares

within_cluster_sum_of_squares = iris_kmeans@model$scoring_history$within_cluster_sum_of_squares %>% as.data.frame() 

ggplot(data = within_cluster_sum_of_squares) + 
  geom_point(aes(y = ., x = c(1:dim(within_cluster_sum_of_squares)[1])), colour = 'black') +
  geom_line(aes(y = ., x = c(1:dim(within_cluster_sum_of_squares)[1])), colour = 'blue') +
  scale_x_continuous(labels = as.character(c(1:dim(within_cluster_sum_of_squares)[1])), breaks = c(1:dim(within_cluster_sum_of_squares)[1]))+
  xlab('number of iterations') + ylab('within cluster sum of squares')


pre = (diff(within_cluster_sum_of_squares$.)*-1)[-1] / within_cluster_sum_of_squares$.[- c(1, (dim(within_cluster_sum_of_squares)[1]))]
sum(pre) #threshold min(0.8, [0.02 + 10/number_of_training_rows + 2.5/number_of_model_features**2 ])


pred <- h2o.predict(iris_kmeans, newdata = iris)

h2o.hist(pred)
h2o.table(pred)



#cluster_size_constraints is used with estimate_k = FALSE (the kmeans problem is now Minimum-cost flow)
#https://core.ac.uk/download/pdf/61217069.pdf


iris_kmeans <- h2o.kmeans(k = 6,
                          estimate_k = FALSE,
                          standardize = TRUE,
                          seed = 1234,
                          x = predictors,
                          training_frame = iris, 
                          cluster_size_constraints = c(25, 25, 25, 25, 25, 25),
                          score_each_iteration = TRUE)

pred <- h2o.predict(iris_kmeans, newdata = iris)

h2o.table(pred) #each cluster have approx a size of 25 elements :(


