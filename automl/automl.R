# Import the prostate dataset
data_h2o_frame <- h2o.importFile("http://s3.amazonaws.com/h2o-public-test-data/smalldata/prostate/prostate_complete.csv.zip")

y <- "CAPSULE"
x <- setdiff(names(data_h2o_frame), c("ID")) #names including y 

aml <- h2o.automl(x = x,
                  y = y,
                  training_frame = data_h2o_frame,
                  nfolds = 10,
                  seed = 1234,
                  max_models = 5,
                  max_runtime_secs = 200,
                  max_runtime_secs_per_model = 40)

m <- h2o.get_best_model(aml)

# m@model #CV METRIC

# include_algos ----
# 
# DRF:  Distributed Random Fores (clasificación y regresión).
#       Genera un bosque de árboles (cada arbol es un aprendiz débil - weak learner 
#       construido sobre un subconjunto de filas y columnas)
#       Tanto la clasificación como la regresión toman la predicción promedio de 
#       todos sus árboles para hacer una predicción final

# XGBoost:  (clasificación y regresión) hace boosting de arboles de decicion, 
#           boosting es una la técnica de aprendizaje conjunto de construir 
#           muchos modelos secuencialmente, con cada nuevo modelo intentando corregir 
#           las deficiencias del modelo anterior. XGBoost proporciona refuerzo de árbol 
#           paralelo (también conocido como GBDT, GBM (Gradient boosting))

# GBM:  Gradient Boosting Machine (clasificación y regresión). 
#       es un método de conjunto de aprendizaje hacia adelante (forward learning). 
#       La heurística rectora es que se pueden obtener buenos resultados predictivos 
#       mediante aproximaciones cada vez más refinadas.


# GLM:  Global Linear Models, estiman los modelos de regresión para los resultados (outcomes)
#       que siguen distribuciones exponenciales. Además de la distribución gaussiana (es decir, normal), 
#       se incluyen las distribuciones de Poisson, binomial y gamma. Cada uno tiene un propósito 
#       diferente y, según la distribución y la elección de la función de enlace, se puede utilizar 
#       para la predicción o la clasificación.
#       La suite GLM incluye:
#       Regresiónes gaussiana, de Poisson, binomial (clasificación), binomial fraccional, cuasibinomial, 
#       gamma, ordinal, binomial negativa, Clasificación multinomial y Distribución Tweedie


# StackedEnsemble:  regresión, clasificación binaria y clasificación multiclase. 
#                   Es una clase de algoritmos que implica entrenar a un "metaaprendiz" de segundo nivel 
#                   para encontrar la combinación óptima de los aprendices básicos. 
#                   A diferencia del bagging y el boosting, el objetivo del stacking es reunir 
#                   conjuntos de modelos aprendices fuertes y diversos.
#                   Para los L algoritmos, y N valores predichos con CV, se construye la matriz 
#                   NxL+ vector de respuesta = datos de nivel uno
#                   Luego construye un modelo de metaaprendizaje con los datos de nivel uno.
#                   Un nuevo patron se estima generando predicciones de conjunto a partir de los modelos base y
#                   Luego se introducen estas predicciones en el metaaprendiz para obtener la 
#                   prediccion de conjunto.



# DeepLearning: 






# sort_metric: metrica para ordenar el Leaderboard ----
#
# AUC: para clasificacion binaria, area bajo la curva roc (sensivilidad vs 1-specificidad)
# mean_per_class_error: para clasifiacion multinomial  (members of class identified correctly)/(number of members of class)
# deviance: para regresion, mean residual deviance (y-y_pred)

# logloss: para clasificacion 

# MSE: para regresion, Mean square error
# RMSE: para regresion, Root MSE
# MAE: para regresion,  Mean absolute error
# RMSLE: Root Mean Squared Logarithmic Error (use wheen predictions have large deviations)
# AUCPR (area under the Precision-Recall curve)

# NOTA: 
# sensitivity = recall = tp / t = tp / (tp + fn)
# specificity = tn / n = tn / (tn + fp)
# precision = tp / p = tp / (tp + fp)




