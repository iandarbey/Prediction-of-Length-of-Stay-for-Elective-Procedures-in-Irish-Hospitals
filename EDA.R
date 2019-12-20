# Exploratory Data Analysis
library(tidyverse)
library(cowplot)

load(file = "formodellingmode.RData")


SEX <- formodelling %>%
  group_by(SEX) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = SEX, y = Count, fill = SEX, col = SEX))+
  theme_minimal()+
  theme(text = element_text(size = 25), legend.position = "none")+
  geom_bar(stat = "identity")



LOSclass <- formodelling %>%
  group_by(LOSclass) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = LOSclass, y = Count, fill = LOSclass, col = LOSclass))+
  theme_minimal()+
  theme(text = element_text(size = 25), legend.position = "none")+
  geom_bar(stat = "identity")

plot_grid(SEX, LOSclass, labels = "AUTO", label_size = 25)



ageplot <- formodelling %>%
  ggplot(aes(x = AGE))+
  theme_minimal()+
  theme(text = element_text(size = 25), legend.position = "none")+
  geom_histogram(binwidth = 5, fill = 'grey', col = 'black')

meanLOS <- formodelling %>%
  ggplot(aes(x = meanLOS))+
  theme_minimal()+
  theme(text = element_text(size = 25), legend.position = "none")+
  geom_histogram(binwidth = 5, fill = 'lightblue', col = 'black')

plot_grid(ageplot, meanLOS, labels = "AUTO", label_size = 25)

