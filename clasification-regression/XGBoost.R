
# XGBoost no es compatible con Windows.

# XGBoost es un algoritmo de aprendizaje supervisado que implementa un proceso llamado 
# impulso para producir modelos precisos. Impulsar se refiere a la técnica de aprendizaje
# conjunto de construir muchos modelos secuencialmente, con cada nuevo modelo intentando
# corregir las deficiencias del modelo anterior. En el refuerzo de árboles, cada nuevo 
# modelo que se agrega al conjunto es un árbol de decisiones. XGBoost proporciona refuerzo 
# de árbol paralelo (también conocido como GBDT, GBM) que resuelve muchos problemas de 
# ciencia de datos de una manera rápida y precisa. Para muchos problemas, XGBoost es 
# uno de los mejores marcos de máquinas impulsoras de gradientes (GBM) de la actualidad.

library(h2o)
h2o.init()

# Import the iris dataset into H2O:
titanic <- h2o.importFile("https://s3.amazonaws.com/h2o-public-test-data/smalldata/gbm_test/titanic.csv")

# Set the predictors and response; set the response as a factor:
titanic['survived'] <- as.factor(titanic['survived'])
predictors <- setdiff(colnames(titanic), colnames(titanic)[2:3])
response <- "survived"

# Split the dataset into a train and valid set:
titanic_splits <- h2o.splitFrame(data =  titanic, ratios = 0.8, seed = 1234)
train <- titanic_splits[[1]]
valid <- titanic_splits[[2]]

# Build and train the model:
titanic_xgb <- h2o.xgboost(x = predictors,
                           y = response,
                           training_frame = train,
                           validation_frame = valid,
                           booster = "dart",
                           normalize_type = "tree",
                           seed = 1234)

# Eval performance:
perf <- h2o.performance(titanic_xgb)

# Generate predictions on a test set (if necessary):
pred <- h2o.predict(titanic_xgb, newdata = valid)

# Extract feature interactions:
feature_interactions = h2o.feature_interaction(titanic_xgb)
