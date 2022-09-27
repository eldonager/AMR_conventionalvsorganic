##Host model
library(tidyverse)
library(nlme)
host_model <- read_csv("host_model.csv")


host_model$doi <- as.factor(host_model$doi)
host_model$farm_type <- as.factor(host_model$farm_type)
host_model$host <- as.factor(host_model$host)

attach(host_model)

host.model1 <- lme(percent_resistant ~ farm_type*host,random= ~
                     1|doi)

summary(host.model1)

detach(host_model)

host <- read_csv("mean.data.csv")
