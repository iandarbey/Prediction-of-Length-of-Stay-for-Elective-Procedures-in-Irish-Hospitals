library(corrplot)
library(tidyverse)
library(rcompanion)
library(corrr)
library(ranger)
library(ggQC)
library(qcc)

formodellingnumeric <- sapply(formodelling, as.numeric)


formodellingcorr <- cor(formodellingnumeric)

formodellingcorr
corrplot(formodellingcorr, method = "circle")

corrplot(formodellingcorr, method = "ellipse")

corrplot(formodellingcorr, method = "pie")

corrplot(formodellingcorr, method = "shade")

corrplot.mixed(formodellingcorr, lower.col = "black", number.cex = .7, tl.cex = .6)

corrp


importanceforest <- ranger(LOSclass ~ .,
                           data = formodelling,
                           importance = "impurity",
                           num.trees = 10000)

variableimport <- importance(importanceforest)

class(variableimport)

variableimport  <- data.frame(Variable=names(variableimport), Value=variableimport, row.names=NULL)

variableimport <- variableimport %>%
  arrange(desc(Value))

ggplot(variableimport, aes(x = reorder(Variable, -Value), y = Value, fill = Variable))+
  geom_bar(stat = "identity")+
  coord_flip()+
  xlab("Variable")+
  ylab("Importance Value")+
  theme(text = element_text(size = 25), legend.position = "none")+
  labs(title = "Variable Importance - 10,000 Tree Random Forest")




ggplot(variableimport, aes(x= reorder(Variable, -Value), y = Value))+
  stat_pareto(point.color = "red",
              point.size = 3,
              line.color = "black",
              #size.line = 1,
              bars.fill = c("blue", "orange"))+
  ggtitle("Pareto Chart of Variable Importance - 1000 Tree Random Forest")+
  theme_light()



pareto.chart(variableimport$Value, 
             ylab = "Importance", 
             #x = variableimport$Variable, 
             main = "Pareto Chart of Variable Importance - 1000 Tree Random Forest Pareto Chart",
             cumperc = c(20,40,60,80,100)) # or = seq(0, 100, by =25)
  
