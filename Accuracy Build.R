#Load Libraries Required

library(readxl)
library(tidyverse)
library(lubridate)
library(ranger)
library(caret)
library(e1071)
library(xgboost)
library(kernlab)
library(nnet)
library(C50)
library(rpart)
library(DMwR)

load("formodellingmode.RData")

# MOVED to Data Prep
# # Create Training and Testing Partition
# 
# set.seed(123)
# index <- createDataPartition(formodelling$LOSclass, p = 0.80, times = 1)
# training <- formodelling[index$Resample1,]
# testing <- formodelling[-index$Resample1,]
# 
# trainingsim <- SMOTE(LOSclass ~. , data = as.data.frame(training), perc.over = 300, perc.under = 130)
# 
# save(index, training, testing, trainingsim, file = "accuracy/data.RData")

# set training controls for all models

contrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, verboseIter = TRUE)

# Create and evaluate Random Forest Model ----

forestmodel <- train(LOSclass ~ . ,
                     data = training,
                     method = "ranger",
                     num.trees = 500,
                     trControl = contrl,
                     metric="Accuracy")

forest_predictions <- predict(forestmodel, testing)
forestCM <- confusionMatrix(forest_predictions, testing$LOSclass)
forestCM
save(forestmodel, file = "accuracy/forestmodel.RData")

# Create and evaluate a Naive Bayes Model ----

nbmodel <- train(LOSclass ~ .,
                 data = training,
                 method = 'nb',
                 trControl = contrl,
                 metric = "Accuracy")

nb_predictions <- predict(nbmodel, testing)
nbCM <- confusionMatrix(nb_predictions, testing$LOSclass)
nbCM

save(nbmodel, file = "accuracy/nbmodel.RData")

# Create and evaluate a Neural Network Model ----

nnetgrid <- expand.grid(.decay = c(0.5, 0.1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7), .size = c(3, 5, 10, 20))

nnetmodel <- train(LOSclass ~ .,
                   data = training,
                   method = "nnet",
                   trContrl = contrl,
                   tuneGrid = nnetgrid,
                   metric = "Accuracy")

nnetpredictions <- predict(nnetmodel, testing)
nnetCM <- confusionMatrix(nnetpredictions, testing$LOSclass)
nnetCM

save(nnetmodel, file = "accuracy/nnetmodel.RData")
# Create and evaluate a Support Vector Machine Model first for model identification and then for fine tuning ----

svmmodel <- train(LOSclass ~ .,
                   data = training,
                   method = "svmRadial",
                   trControl = contrl,
                   preProcess = c("center", "scale"),
                   tuneGrid = ,
                   metric = "Accuracy",
                   tuneLength = 9)

svmpredictions <- predict(svmmodel, testing)
svmCM <- confusionMatrix(svmpredictions, testing$LOSclass)
svmCM

save(svmmodel, file = "accuracy/svmmodel.RData")

# Create and evaluate Boosted Decision Tree Model ----
xgGrid <- expand.grid(nrounds = 100,
                      eta = c(0.01, 0.001, 0.0001),
                      max_depth = c(2, 4, 6, 8, 10),
                      min_child_weight = c(1L, 10L),
                      subsample = c(0.5, 1),
                      colsample_bytree = c(0.1, 0.4),
                      gamma = 1)

xgboostmodel <- train(as.factor(LOSclass) ~ .,
                      data = training,
                      method = "xgbTree",
                      trControl = contrl,
                      tuneGrid = xgGrid,
                      metric = "Accuracy")

xgboostpredictions <- predict(xgboostmodel, testing)
xgbCM <- confusionMatrix(xgboostpredictions, testing$LOSclass)
xgbCM

save(xgboostmodel, file = "accuracy/xgboostmodel.RData")



# Create and evaluate a Linear Model ----

linmodel <- glm(LOSclass ~ ., data = training, family = binomial(link = "logit"))
linpredictions <- predict(linmodel, testing, type = "response")
linpredictions <- as.data.frame(linpredictions)
linpredictions <- linpredictions %>%
  mutate(linpredictions = case_when(linpredictions <= 0.50 ~ "Good",
                                    linpredictions > 0.50 ~ "Long Stay")) %>%
  mutate(linpredictions = as.factor(linpredictions))

linCM <- confusionMatrix(linpredictions$linpredictions, testing$LOSclass)
linCM

save(linmodel, file = "accuracy/linmodel.RData")

#Create and evaluate a C50 Decision Tree model ----

C50grid <- expand.grid( .winnow = c(TRUE,FALSE), .trials=c(1,5,10,15,20), .model="tree" )

C50model <- train(LOSclass ~ .,
                  data = training,
                  method = "C5.0",
                  trContrl = contrl,
                  tuneGrid = C50grid,
                  metric = "Accuracy")

C50predictions <- predict(C50model, testing)
C50CM <- confusionMatrix(C50predictions, testing$LOSclass)
C50CM
C50model$bestTune

save(C50model, file = "accuracy/C50model.RData")

# MOVED to data prep 
# #set up an ensemble using the simulated larger training set
# 
# ensembleindex <- createDataPartition(trainingsim$LOSclass, p = 0.5 , times = 1)
# ensembletrain <- trainingsim[ensembleindex$Resample1,]
# ensembletinnertest <- trainingsim[-ensembleindex$Resample1,]
# 
# save(ensembletinnertest, ensembletrain, ensembleindex, file = "accuracy/ensembledata.RData")
# Train the models (based on the best tunes of the above models) on the first half of the set - then train the ensemble on the second.

# Ensemble Train ----

ensemblecontrl <- trainControl(method = "repeatedcv", number = 10, repeats = 1, verboseIter = TRUE)


ensembleRFTuneGrid <- forestmodel$bestTune

# Ensemble RF ----

ensembleRFmodel <- train(LOSclass ~ .,
                         data = ensembletrain,
                         method = "ranger",
                         num.trees = 500,
                         tuneGrid = ensembleRFTuneGrid)

ensembleNBTuneGrid <- nbmodel$bestTune

# Ensemble NB ----

ensembleNBmodel <- train(LOSclass ~ .,
                         data = ensembletrain,
                         method = "nb",
                         tuneGrid = ensembleNBTuneGrid)

ensembleNNETTuneGrid <- nnetmodel$bestTune

# Ensemble NNET ----

ensembleNNETmodel <- train(LOSclass ~ .,
                           data = ensembletrain,
                           method = "nnet",
                           tuneGrid = ensembleNNETTuneGrid)

ensembleSVMTuneGrid <- svmmodel$bestTune

ensembleSVMmodel <- train(LOSclass ~ .,
                          data = ensembletrain,
                          method = "svmRadial",
                          preProcess = c("center", "scale"),
                          tuneGrid = ensembleSVMTuneGrid)

ensemblexgGrid <- xgboostmodel$bestTune

ensemblexgboostmodel <- train(as.factor(LOSclass) ~ .,
                              data = ensembletrain,
                              method = "xgbTree",
                              tuneGrid = ensemblexgGrid)


ensembletraining <- bind_cols(LOSclass = ensembletinnertest$LOSclass,
                              RFPredict = predict(ensembleRFmodel, ensembletinnertest),
                              NBPredict = predict(ensembleNBmodel, ensembletinnertest),
                              NNETPredict = predict(ensembleNNETmodel, ensembletinnertest, type = "prob")$'Good',
                              SVMPredict = predict(ensembleSVMmodel, ensembletinnertest),
                              XGPredict = predict(ensemblexgboostmodel, ensembletinnertest, type = "prob")$'Good')

# Ensemble Main Train ----

EnsembleXGBoostGrid <- expand.grid(nrounds = 100,
                                   eta = c(0.01, 0.001, 0.0001),
                                   max_depth = c(2, 4, 6, 8, 10),
                                   min_child_weight = c(1L, 10L),
                                   subsample = c(0.5, 1),
                                   colsample_bytree = c(0.1, 0.4),
                                   gamma = 1)

EnsembleXGBoost <- train(as.factor(LOSclass) ~ . ,
                         data = ensembletraining,
                         method = "xgbTree",
                         trControl = ensemblecontrl,
                         tuneGrid = EnsembleXGBoostGrid,
                         metric = "Accuracy")

save(forestmodel, nbmodel, nnetmodel, svmmodel, xgboostmodel, C50model, file = "accuracy/modelsACC.RData")
save(ensembleRFmodel, ensembleNBmodel,
     ensembleNNETmodel,
     ensembleSVMmodel, ensemblexgboostmodel,
     file = "accuracy/ensembleACC.RData")
save(EnsembleXGBoost, file = "accuracy/finalensemble.RData")

ensemblepredict <- function(x) {
  tempdf <- bind_cols(RFPredict = predict(ensembleRFmodel, x),
                      NBPredict = predict(ensembleNBmodel, x),
                      NNETPredict = predict(ensembleNNETmodel, x, type = "prob")$'Good',
                      SVMPredict = predict(ensembleSVMmodel, x),
                      XGPredict = predict(ensemblexgboostmodel, x, type = "prob")$'Good')
  predict(EnsembleXGBoost, tempdf)
}

ensemblepredictions <- ensemblepredict(testing)
confusionMatrix(ensemblepredictions, testing$LOSclass)

save.image(file = "accuracy.RData")


### Deep Learning - Post Thesis Experiment ----
library(keras)


DLTraining <- select(training, -LOSclass)
DLTraining$SEX <- as.numeric(DLTraining$SEX)
#DLTraining <- as.matrix(DLTraining)
DLLabels <- training$LOSclass

DLTesting <- select(testing, - LOSclass)
DLTesting$SEX <- as.numeric(DLTesting$SEX)
DLTestLabels <- testing$LOSclass




X_train <- DLTraining %>%
  scale()

y_train <- to_categorical(as.integer(DLLabels))

X_test <- DLTesting %>%
  scale()

y_test <- to_categorical(as.integer(DLTestLabels))



DLmodel <- keras_model_sequential() 

DLmodel %>% 
  layer_dense(units = 100, activation = 'relu', input_shape = ncol(X_train)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 25, activation = 'relu') %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 3, activation = 'sigmoid')

history <- DLmodel %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy')
)

DLmodel %>% fit(
  X_train, y_train, 
  epochs = 1000, 
  batch_size = 512,
  validation_split = 0.1
)



predictions <- DLmodel %>% predict_classes(X_test)
predictions <- factor(predictions, levels = c(1,2), labels = c("Good", "Long Stay"))
confusionMatrix(predictions, testing$LOSclass)


