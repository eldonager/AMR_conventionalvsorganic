
library(tidyverse)
library(nlme)
library(lmerTest)

df1 <- read.csv("mean.data.csv")



View(df1)
names(df1)

df1$doi <- as.factor(df1$doi)
df1$farm_type <- as.factor(df1$farm_type)
df1$sampling_end_date <- as.factor(df1$sampling_end_date)
df1$continent <- as.factor(df1$continent)
df1$country <- as.factor(df1$country)
df1$host <- as.factor(df1$host)
df1$antimicrobial <- as.factor(df1$antimicrobial)
df1$antimicrobial_class <- as.factor(df1$antimicrobial_class)
df1$pathogen <- as.factor(df1$pathogen)
df1$who_classification <- as.factor(df1$who_classification)
attach(df1)



model.1 <- lme(mean ~ farm_type*host,random= ~
                          1|doi/continent/country/sampling_end_date/
                 who_classification/pathogen/antimicrobial_class/
                 antimicrobial)


summary(model.1)



