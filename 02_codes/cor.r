library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(patchwork)
library(corrplot)

#Load data
main_data <- read_csv("conv.csv")

data1 <- main_data %>%
  group_by(country) %>%
  select("country",
         "doi",
         "farm_type",
         "host",
         "antimicrobial_compound",
         "antimicrobial_class",
         "pathogen",
         "percent_resistant",
         "no_isolates_resistant",
         "no_isolates_susceptible",
         "no_isolates_intermidiate",
         "no_isolates" )

#changing percentage resistance into numeric class and rounding off  
data1$percent_resistant <- as.numeric(as.character
                                      (data1$percent_resistant))

data1$percent_resistant <- round(data1$percent_resistant, digits = 0)

data2 <- data1 %>%
  select("doi",
         "country",
         "farm_type",
         "host",
         "antimicrobial_compound",
         "antimicrobial_class",
         "pathogen",
         "percent_resistant",
         "no_isolates_resistant",
         "no_isolates_susceptible",
         "no_isolates_intermidiate",
         "no_isolates" )






data3 <- data2 %>%select("country",
                         "doi",
                         "farm_type",
                         "host",
                         "antimicrobial_class",
                         "pathogen",
                         "percent_resistant", "no_isolates")%>%
  filter(pathogen != "Arcanobacterium pyogenes", pathogen != "Corynebacterium bovis",
         pathogen != "Environmental streptococci")


#changing percentage resistance into numeric class and rounding off  
data3$percent_resistant <- as.numeric(as.character
                                      (data3$percent_resistant))





data4 <- aggregate(percent_resistant~country+farm_type, data = data3, FUN = mean)

data4$percent_resistant <- round(data4$percent_resistant, digits = 0)




data5 <- data4 %>%
  filter(country == "United Kingdom" | country == "Sweden" | country == "Germany" |
           country == "Denmark" | country == "United States of America" | country == "France" |
           country == "Italy" | country == "Portugal" | country == "Spain" | country == "New Zealand" |
           country == "Austria")




#Antimicrobial usage data

df1 <- read_csv("Antimicrobial_usage.csv")

df2 <- left_join(data5, df1, by = "country")

 df3 <- df2 %>%
   select(country, farm_type, percent_resistant, `sales(tonnes)`)

 ##Recode farm type
 
 df3$farm_type[df3$farm_type == "Conventional"] <- "1"
 df3$farm_type[df3$farm_type == "Organic"] <- "2"

 ##Change to numeric
 
 df3$farm_type <- as.numeric(df3$farm_type)

 
##Record country
 df3$country[df3$country == "United Kingdom"] <- "1"
 df3$country[df3$country == "Belgium"] <- "2"
 df3$country[df3$country == "Denmark"] <- "3"
 df3$country[df3$country == "France"] <- "4"
 df3$country[df3$country == "Germany"] <- "5"
 df3$country[df3$country == "Italy"] <- "6"
 df3$country[df3$country == "Portugal"] <- "7"
 df3$country[df3$country == "Spain"] <- "8"
 df3$country[df3$country == "Sweden"] <- "10"
 df3$country[df3$country == "New Zealand"] <- "11"
 df3$country[df3$country == "United States of America"] <- "12"
 df3$country[df3$country == "Austria"] <- "13"
 
 ##Change to numeric
 df3$country <- as.numeric(df3$country)
 
 ##Corelation plot
 correl<-corrplot(cor(as.matrix(df3),method = "pearson"),
                  method = "color",
                  tl.cex = 0.9,
                  number.cex = 0.95,
                  addCoef.col = "black")
 