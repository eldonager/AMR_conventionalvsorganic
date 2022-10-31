

library(tidyverse)
library(nlme)

country_model <- read_csv("country_model.csv")
View(country_model)


country_model$country <- as.factor(country_model$country)
country_model$doi <- as.factor(country_model$doi)
country_model$farm_type <- as.factor(country_model$farm_type)
country_model$host <- as.factor(country_model$host)
country_model$antimicrobial_class <- as.factor(country_model$antimicrobial_class)
country_model$pathogen <- as.factor(country_model$pathogen)


summary(country_model)


attach(country_model)
names(country_model)


country.model1 <- lme(percent_resistant ~ farm_type*host,random= ~
                         1|country/doi/antimicrobial_class/pathogen)

summary(country.model1 )

detach(country_model)


