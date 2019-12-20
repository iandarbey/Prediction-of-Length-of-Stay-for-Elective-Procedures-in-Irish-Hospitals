library(caret)
library(tidyverse)

load("accuracy/data.RData")
load("accuracy/forestmodel.RData")
load("accuracy/C50model.RData")
load("accuracy/linmodel.RData")
load("accuracy/nbmodel.RData")
load("accuracy/nnetmodel.RData")
load("accuracy/svmmodel.RData")
load("accuracy/xgboostmodel.RData")
load("formodellingmode.RData")
load("ensembleACC.RData")


forestpredictions <- predict(forestmodel, testing)
confusionMatrix(forestpredictions, testing$LOSclass)

C50predictions <- predict(C50model, testing)
confusionMatrix(C50predictions, testing$LOSclass)

linpredictions <- predict(linmodel, testing, type = "response")
linpredictions <- as.data.frame(linpredictions)
linpredictions <- linpredictions %>%
  mutate(linpredictions = case_when(linpredictions <= 0.50 ~ "Good",
                                    linpredictions > 0.50 ~ "Long Stay")) %>%
  mutate(linpredictions = as.factor(linpredictions))

confusionMatrix(linpredictions$linpredictions, testing$LOSclass)


nbpredictions <- predict(nbmodel, testing)
confusionMatrix(nbpredictions, testing$LOSclass)


nnetpredictions <- predict(nnetmodel, testing)
confusionMatrix(nnetpredictions, testing$LOSclass)

svmpredictions <- predict(svmmodel, testing)
confusionMatrix(svmpredictions, testing$LOSclass)

xgboostpreds <- predict(xgboostmodel, testing)
confusionMatrix(xgboostpreds, testing$LOSclass)


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


xgplot <- xgb.plot.multi.trees(xgboostmodel$finalModel, render = FALSE)
render_graph(xgplot, "xgplot.jpeg", width = 1500, height = 7500)

xgb.plot.multi.trees(xgboostmodel$finalModel)
