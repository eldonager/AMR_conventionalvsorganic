library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(patchwork)
library(scales)
library(randomcoloR)

#Load data
main_data <- read_csv("mean.data.csv")

data1 <- main_data %>%
  select("antimicrobial", "doi", "antimicrobial_compound",
         "farm_type",
          
         "sampling_end_date", "mean")


data2 <- filter(data1, !is.na(data1$sampling_end_date))
 


data3 <-aggregate(mean~doi+farm_type+sampling_end_date, data2, mean)


#changing percentage resistance into numeric class and rounding off  
data3$mean <- as.numeric(as.character(data3$mean))

data3$mean <- round(data3$mean, digits = 0)

data3<- data3[which(data3$mean>= 1),]



##Organic farm plot

organic.data<- data3 %>%
  filter(farm_type == "Organic")
   
        
##Organic box plot

organic.p1<-ggboxplot(organic.data, "sampling_end_date", "mean", fill = "farm_type",
        xlab = "Year", ylab = "Percent resistance", 
           palette = c("#00AFBB")) +
  geom_boxplot(fill = "#00AFBB") +
  ggbeeswarm::geom_quasirandom(
    
    size = 1.5,
    alpha = .4,
    width = .2
  ) +
  rotate_x_text(60)

organic.plot <-ggpar(organic.p1, legend.title = "Farm type")

##Conventional farm plot

conv.data<- data3 %>%
  filter(farm_type == "Conventional")


##Conventional box plot

conv.p1<-ggboxplot(conv.data, "sampling_end_date", "mean", fill = "farm_type",
                      xlab = "Year", ylab = "Percent resistance", 
                      palette = c("#FC4E07")) +
  geom_boxplot(fill = "#FC4E07") +
  ggbeeswarm::geom_quasirandom(
    
    size = 1.5,
    alpha = .4,
    width = .2
  ) +
  rotate_x_text(60)

conv.plot <-ggpar(conv.p1, legend.title = "Farm type")


##Merging plots together using patchwork


trend.plot <- (organic.plot | conv.plot)+ 
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")")+
  plot_layout(guides = "keep")&
  theme(plot.tag = element_text(),
        legend.position = "top")


ggsave(filename = "trends_plot.png",trend.plot)
