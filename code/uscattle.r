library(tidyverse)
library(dplyr)
library(ggpubr)
library(dplyr)



#Load data
main_data <- read_csv("conv.csv")

#changing percentage resistance into numeric class and rounding off  
main_data$percent_resistant <- as.numeric(as.character
                                          (main_data$percent_resistant))

main_data$percent_resistant <- round(main_data$percent_resistant, digits = 0)


main_data<- main_data%>%
  mutate(
    host = 
      ifelse(host == "Cows and Calves", 
             "Cattle", host))
      
main_data%>%
  count(host)%>%
  view()

usa_cattle <- main_data %>%
  filter(country == "United States of America" &
           host == "Cattle") %>%
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
                         who_classification+percent_resistant, usa_cattle, mean) 


usa_data1$percent_resistant <- round(usa_data1$percent_resistant, digits = 0)

##Stacked bar
us_plot <- ggbarplot(usa_data1, "antimicrobial", "percent_resistant", 
                     fill = "farm_type",
                     subtitle = "AMR in cattle in the USA farms",
                     xlab = "Antimicrobial", ylab = "Percentage resistance",
                     legend.title = "Farm type", font.x = "bold", font.y = "bold",
                     font.legend = "bold", font.subtitle = "bold")+
  rotate_x_text(90)

us_plot <- us_plot+facet_wrap(~who_classification)
