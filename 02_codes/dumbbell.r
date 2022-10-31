library(tidyverse)
library(ggplot2)
library(dplyr)
library(patchwork)
library(ggalt)
#Load data
main_data <- read_csv("conv.csv")

#changing percentage resistance into numeric class and rounding off  
main_data$percent_resistant <- as.numeric(as.character
                                          (main_data$percent_resistant))

main_data$percent_resistant <- round(main_data$percent_resistant, digits = 2)


conv_data <- main_data %>%
group_by(country) %>%
  filter(farm_type == "Conventional") %>%
  select("country",
        "sampling_start_date",
         "sampling_end_date",
         "percent_resistant") %>%
    rename(start_year = "sampling_start_date",
           end_year = "sampling_end_date",) %>%
  drop_na()

conv_data1 <- conv_data %>%
  pivot_longer(cols = c("start_year",
               "end_year"), names_to = "year", 
               values_to = "year_name")
 conv_data1
  
 
 
 conv_data2 <- aggregate(percent_resistant~country+year+year_name,conv_data1, mean)
 
 conv_data2 %>%
   ggplot(aes(x= percent_resistant, y= country, color= year)) +
   geom_line(color= "black")+
   geom_point()
   
 ##finding means
  aggregate(percent_resistant~country+year+year_name,conv_data1, mean)%>%
view()
  
  aggregate(percent_resistant~year+country+year_name,conv_data1, mean)%>%
    view()
  
  aggregate(percent_resistant~year_name+country+year,conv_data1, mean)%>%
    view()
  ##extracting data
usa <- conv_data1 %>%
  filter(country == "United States of America" & year_name == "2000" |
           year_name == "2015")

  us_data <- aggregate(percent_resistant~year_name+country+year,usa, mean)

  us_data
  
  ## USA dumbbell plot(2000-2015)
  us_data %>%
    ggplot(aes(x= percent_resistant, y= country, color= year)) +
    geom_line(color= "black")+
    geom_point()+
    geom_text(aes(label= percent_resistant))+
    labs(title = "AMR percentages in the USA at the beginning of 2000 and at the end of 2015")
  
  