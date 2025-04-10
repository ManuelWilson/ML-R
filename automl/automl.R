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
# DRF:  Distributed Random Fores (clasificaci�n y regresi�n).
#       Genera un bosque de �rboles (cada arbol es un aprendiz d�bil - weak learner 
#       construido sobre un subconjunto de filas y columnas)
#       Tanto la clasificaci�n como la regresi�n toman la predicci�n promedio de 
#       todos sus �rboles para hacer una predicci�n final

# XGBoost:  (clasificaci�n y regresi�n) hace boosting de arboles de decicion, 
#           boosting es una la t�cnica de aprendizaje conjunto de construir 
#           muchos modelos secuencialmente, con cada nuevo modelo intentando corregir 
#           las deficiencias del modelo anterior. XGBoost proporciona refuerzo de �rbol 
#           paralelo (tambi�n conocido como GBDT, GBM (Gradient boosting))

# GBM:  Gradient Boosting Machine (clasificaci�n y regresi�n). 
#       es un m�todo de conjunto de aprendizaje hacia adelante (forward learning). 
#       La heur�stica rectora es que se pueden obtener buenos resultados predictivos 
#       mediante aproximaciones cada vez m�s refinadas.


# GLM:  Global Linear Models, estiman los modelos de regresi�n para los resultados (outcomes)
#       que siguen distribuciones exponenciales. Adem�s de la distribuci�n gaussiana (es decir, normal), 
#       se incluyen las distribuciones de Poisson, binomial y gamma. Cada uno tiene un prop�sito 
#       diferente y, seg�n la distribuci�n y la elecci�n de la funci�n de enlace, se puede utilizar 
#       para la predicci�n o la clasificaci�n.
#       La suite GLM incluye:
#       Regresi�nes gaussiana, de Poisson, binomial (clasificaci�n), binomial fraccional, cuasibinomial, 
#       gamma, ordinal, binomial negativa, Clasificaci�n multinomial y Distribuci�n Tweedie


# StackedEnsemble:  regresi�n, clasificaci�n binaria y clasificaci�n multiclase. 
#                   Es una clase de algoritmos que implica entrenar a un "metaaprendiz" de segundo nivel 
#                   para encontrar la combinaci�n �ptima de los aprendices b�sicos. 
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




