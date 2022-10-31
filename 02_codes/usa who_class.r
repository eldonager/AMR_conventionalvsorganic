library(tidyverse)
library(dplyr)
library(ggpubr)
library(patchwork)



#Load data
main_data <- read_csv("conv.csv")

#changing percentage resistance into numeric class and rounding off  
main_data$percent_resistant <- as.numeric(as.character
                                          (main_data$percent_resistant))

main_data$percent_resistant <- round(main_data$percent_resistant, digits = 0)

usa_crit <- main_data %>%
  filter(country == "United States of America" &
           who_classification == "Critically important") %>%
  select("author",
         "country",
         "iso_3",
         "locality",
         "farm_type",
         "sample_type",
         "host",
         "antimicrobial",
         "antimicrobial_compound",
         "who_classification",
         "pathogen",
         
         "percent_resistant",
         
         "no_isolates",
         "sampling_start_date", 
         "sampling_end_date"
  )


usa_data1 <- aggregate(percent_resistant~host+farm_type+host+antimicrobial+
                         who_classification+percent_resistant, usa_crit, mean) 




usa_data2<- usa_data1 %>%
  relocate(antimicrobial, .after = "host")%>%
  relocate(percent_resistant, .after = "who_classification")%>%
  arrange(host)

usa_data2$percent_resistant <- round(usa_data2$percent_resistant, digits = 0)
##stacked
us_plot <- ggbarplot(usa_data2, "antimicrobial", "percent_resistant", 
                     fill = "farm_type",
                     subtitle = "AMR of critically important antimicrobials in the USA farms",
                     xlab = "Antimicrobial", ylab = "Percentage resistance",
                     legend.title = "Farm type", font.x = "bold", font.y = "bold",
                     font.legend = "bold", font.subtitle = "bold")+
  rotate_x_text(90)

c_importantplot <- us_plot+facet_wrap(~host)

##Highly important

usa_high <- main_data %>%
  filter(country == "United States of America" &
           who_classification == "Highly important") %>%
  select("author",
         "country",
         "iso_3",
         "locality",
         "farm_type",
         "sample_type",
         "host",
         "antimicrobial",
         "antimicrobial_compound",
         "who_classification",
         "pathogen",
         
         "percent_resistant",
         
         "no_isolates",
         "sampling_start_date", 
         "sampling_end_date"
  )




usa_data10 <- aggregate(percent_resistant~host+farm_type+host+antimicrobial+
                         who_classification+percent_resistant, usa_high, mean) 



usa_data10$percent_resistant <- round(usa_data10$percent_resistant, digits = 0)

usa_data11<- usa_data10 %>%
  relocate(antimicrobial, .after = "host")%>%
  relocate(percent_resistant, .after = "who_classification")%>%
  arrange(host)

##stacked
us_plot1 <- ggbarplot(usa_data11, "antimicrobial", "percent_resistant", 
                     fill = "farm_type",
                     subtitle = "AMR of highly important antimicrobials in the USA farms",
                     xlab = "Antimicrobial", ylab = "Percentage resistance",
                     legend.title = "Farm type", font.x = "bold", font.y = "bold",
                     font.legend = "bold", font.subtitle = "bold")+
  rotate_x_text(90)

high_impplot <- us_plot1+facet_wrap(~host)



