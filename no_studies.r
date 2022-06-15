library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(wesanderson)

#Load data
main_data <- read_csv("conv.csv")


data1 <- main_data %>%
  select(doi, continent, year)

data2 <- data1 %>%
  group_by(continent)%>%
  count(doi, year)

data3 <- data2 %>%
  select(continent, year)


data4<- data3 %>%
  count(continent, year)

p1<- ggbarplot(data4, "year", 
          "n", 
          fill = "continent",
          subtitle = "",
          xlab = "Year of publication", ylab = "Number of publications",
          legend.title = "Continent",
          font.subtitle = "italic")+
  scale_fill_manual(values = wes_palette( "Darjeeling1", n=5))+
  rotate_x_text(60)

p1

ggsave(filename = "no_studies.png",p1)

