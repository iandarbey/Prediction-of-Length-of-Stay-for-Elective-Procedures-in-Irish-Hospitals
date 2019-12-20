library(caret)
library(tidyverse)

load("formodellingmode.RData")

#load("kappa/data.RData")
load("kappa/forestmodel.RData")
load("kappa/c50model.RData")
load("kappa/limodel.RData")
load("kappa/nbmodel.RData")
load("kappa/nnetmodel.RData")
load("kappa/svmmodel.RData")
load("kappa/xgboostmodel.RData")
load("kappa/finalensemble.RData")
load("kappa/modelsACC.RData")
load("kappa/ensemble.RData")

forestpredictions <- predict(forestmodel, testing)
confusionMatrix(forestpredictions, testing$LOSclass, mode = "everything")

C50predictions <- predict(C50model, testing)
confusionMatrix(C50predictions, testing$LOSclass, mode = "everything")

linpredictions <- predict(linmodel, testing, type = "response")
linpredictions <- as.data.frame(linpredictions)
linpredictions <- linpredictions %>%
  mutate(linpredictions = case_when(linpredictions <= 0.50 ~ "Good",
                                    linpredictions > 0.50 ~ "Long Stay")) %>%
  mutate(linpredictions = as.factor(linpredictions))

confusionMatrix(linpredictions$linpredictions, testing$LOSclass)


nbpredictions <- predict(nbmodel, testing)
confusionMatrix(nbpredictions, testing$LOSclass, mode = "everything")


nnetpredictions <- predict(nnetmodel, testing)
confusionMatrix(nnetpredictions, testing$LOSclass, mode = "everything")

svmpredictions <- predict(svmmodel1, testing)
confusionMatrix(svmpredictions, testing$LOSclass)

xgboostpreds <- predict(xgboostmodel, testing)
confusionMatrix(xgboostpreds, testing$LOSclass, mode = "everything")



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
