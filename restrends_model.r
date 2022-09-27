

library(tidyverse)
library(nlme)
library(lmerTest)

restrends_model <- read_csv("restrends_model.csv")
View(restrends_model)
names(restrends_model)

restrends_model$doi <- as.factor(restrends_model$doi)
restrends_model$farm_type <- as.factor(restrends_model$farm_type)
restrends_model$sampling_end_date <- as.factor(restrends_model$sampling_end_date)

attach(restrends_model)


restrends.model1 <- lme(mean ~ farm_type*sampling_end_date,random= ~
                        1|doi)


summary(restrends.model1)

detach(restrends_model)
