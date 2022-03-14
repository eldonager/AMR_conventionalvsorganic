library(tidyverse)
library(dplyr)
library(ggpubr)
library(dplyr)
library(patchwork)



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
         "antimicrobial_class",
         
         "percent_resistant",
         
         "no_isolates",
         "sampling_start_date", 
         "sampling_end_date"
  )


##USA cattle critically important

usa_crit <- usa_cattle %>%
  filter(who_classification == "Critically important")



usa_crit1 <- aggregate(percent_resistant~host+farm_type+host+antimicrobial+
                         antimicrobial_class+antimicrobial_compound+
                         who_classification+percent_resistant, usa_crit, mean) 


usa_crit1$percent_resistant <- round(usa_crit1$percent_resistant, digits = 0)

##grouped bar
crit_plot <- ggbarplot(usa_crit1, "antimicrobial_compound", "percent_resistant", 
                     fill = "farm_type", position = position_dodge(0.7),
                     subtitle = "Critically important",
                     xlab = "Antimicrobial", ylab = "Percentage resistance",
                     legend.title = "Farm type", font.x = "bold", font.y = "bold",
                     font.legend = "bold", font.subtitle = "bold")+
  rotate_x_text(60)

##Highly important


usa_high <- usa_cattle %>%
  filter(who_classification == "Highly important")



usa_high1 <- aggregate(percent_resistant~host+farm_type+host+antimicrobial+
                         antimicrobial_class+antimicrobial_compound+
                         who_classification+percent_resistant, usa_high, mean) 


usa_high1$percent_resistant <- round(usa_high1$percent_resistant, digits = 0)

##grouped bar
high_plot <- ggbarplot(usa_high1, "antimicrobial_compound", "percent_resistant", 
                       fill = "farm_type", position = position_dodge(0.7),
                       subtitle = "Highly important drugs",
                       xlab = "Antimicrobial", ylab = "Percentage resistance",
                       legend.title = "Farm type", font.x = "bold", font.y = "bold",
                       font.legend = "bold", font.subtitle = "bold")+
  rotate_x_text(60)



p1 <- (crit_plot | high_plot) + plot_annotation(tag_levels = "a",
                                              tag_prefix = "(",
                                              tag_suffix = ")")+
  plot_layout(guides = "collect")&
  theme(plot.tag = element_text(face = "bold"),
        legend.position = "top")