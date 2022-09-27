
library(tidyverse)


##Continent model
continent_model <- read_csv("continent_model.csv")
View(continent_model)

summary(continent_model)

continent_model$continent <- as.factor(continent_model$continent)
continent_model$doi <- as.factor(continent_model$doi)
continent_model$farm_type <- as.factor(continent_model$farm_type)
continent_model$host <- as.factor(continent_model$host)
continent_model$antimicrobial_compound<- as.factor(continent_model$antimicrobial_compound)
continent_model$pathogen <- as.factor(continent_model$pathogen)


summary(continent_model)


attach(continent_model)
names(continent_model)

library(nlme)

continet.model1 <- lme(percent_resistant ~ farm_type*host,random= ~
               1|continent/doi/antimicrobial_compound/pathogen)

summary(continet.model1)


detach(continent_model)

plot(continet.model1)
