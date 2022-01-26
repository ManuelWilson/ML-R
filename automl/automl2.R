# La idea era hacer folks aleatoreos para ver que modelos se repite mas con los datos
# elegir el modelo mas repetido, y replicarlo optimizado
# Al final, se concluyo que esto no era necesario, auto ml, ya tiene kfold que usa para 
# obtener el mejor modelo de cada tipo



library(h2o)
require(caret)

h2o.init()

# Import the prostate dataset
data_h2o_frame <- h2o.importFile("http://s3.amazonaws.com/h2o-public-test-data/smalldata/prostate/prostate_complete.csv.zip")

y <- "CAPSULE"
x <- setdiff(names(data_h2o_frame), c("ID")) #names including y 

k = 5

folds <- createFolds(c(1:dim(data_h2o_frame)[1]), k = k, list = TRUE, returnTrain = FALSE)

best_models_name = list()

for (valid in folds) {
  train <- data_h2o_frame[c(1:dim(data_h2o_frame)[1])[-fold] , ]

  
  # Train AutoML
  aml <- h2o.automl(x = x,
                    y = y,
                    training_frame = train,
                    validation_frame = valid,
                    seed = 1234,
                    max_models = 5,
                    max_runtime_secs = 200,
                    max_runtime_secs_per_model = 40)
  
  
  
  #lb <- aml@leaderboard
  
  # get best model
  m <- h2o.get_best_model(aml)
  #m <- h2o.get_best_model(aml, criterion = "logloss")
  #m <- h2o.get_best_model(aml, algorithm = "xgboost")
  #m <- h2o.get_best_model(aml, algorithm = "xgboost", criterion = "logloss")
  
  best_models_name = append(best_models_name, m@algorithm)
  
  
}

#split solo una vez
frame_split <- h2o.splitFrame(data = data_h2o_frame, ratios = 0.8, seed = 1234)
train <- frame_split_test[[1]]
valid <- frame_split[[2]]



# Train AutoML
aml <- h2o.automl(x = x,
                  y = y,
                  training_frame = train,
                  validation_frame = valid,
                  seed = 1234,
                  max_models = 5,
                  max_runtime_secs = 200,
                  max_runtime_secs_per_model = 40)

#lb <- aml@leaderboard

# get best model
m <- h2o.get_best_model(aml)
#m <- h2o.get_best_model(aml, criterion = "logloss")
#m <- h2o.get_best_model(aml, algorithm = "xgboost")
#m <- h2o.get_best_model(aml, algorithm = "xgboost", criterion = "logloss")

m@algorithm


# perf <- h2o.performance(m, valid) #requiere conjunto de validacion (en realidad test)

# folds <- createFolds(mtcars$am, k = 5, returnTrain = TRUE)

