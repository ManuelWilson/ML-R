library(dplyr)
library(h2o)
h2o.init()

# Import the USArrests dataset into H2O:
arrests_df <- h2o.importFile("https://s3.amazonaws.com/h2o-public-test-data/smalldata/pca_test/USArrests.csv") %>% as.data.frame()
arrests_df <- arrests_df %>% rbind(c(10.5, NA, 44, 20.3)) 
arrests <- as.h2o(arrests_df)


# Split the dataset into a train and valid set:
arrests_splits <- h2o.splitFrame(data = arrests, ratios = 0.8, seed = 1234)
train <- arrests_splits[[1]]
valid <- arrests_splits[[2]]

# Build and train the model:
glrm_model = h2o.glrm(training_frame = train,
                      k = 2,
                      loss = "Quadratic",
                      gamma_x = 0.5,
                      gamma_y = 0.5,
                      max_iterations = 700,
                      recover_svd = TRUE,
                      init = "SVD",
                      transform = "STANDARDIZE")

# Eval performance:
arrests_perf <- h2o.performance(glrm_model)

# Generate predictions on a validation set (if necessary):
arrests_pred <- h2o.predict(glrm_model, newdata = valid)


# Genero un vector numerico para cada entrada, inclusive las que tenian NA
arrests_pred <- h2o.predict(glrm_model, newdata = arrests)

View(arrests_pred %>% as.data.frame())



