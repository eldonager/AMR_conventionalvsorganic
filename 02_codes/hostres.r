library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(patchwork)
library(wesanderson)
library(ggpubr)
#Load data
main_data <- read_csv("mean.data.csv")

#changing percentage resistance into numeric class and rounding off  
main_data$percent_resistant <- as.numeric(as.character
                                          (main_data$percent_resistant))

main_data$percent_resistant <- round(main_data$percent_resistant, digits = 0)


data2 <- main_data %>%
  group_by(host) %>%
  select("doi",
         "farm_type",
        "host",
        "percent_resistant")
         
         





data3 <-aggregate(percent_resistant~doi+farm_type+host, data2, mean)

data3 %>%
  count(doi)%>%
  View()

#changing percentage resistance into numeric class and rounding off  
data3$percent_resistant <- as.numeric(as.character
                                      (data3$percent_resistant))

data3$percent_resistant <- round(data3$percent_resistant, digits = 0)

data3<- data3[which(data3$percent_resistant>= 1),]

#subseting organic farm data
Organic.data <- data3 %>%
  filter(farm_type == "Organic")


##Organic farm raincloud plots



org.plot<-ggplot(Organic.data, aes(x = host, y = percent_resistant, fill = host))+
                        
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA
  ) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA
  ) +
  ## add justified jitter from the {gghalves} package
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .3
  ) + scale_fill_manual(values = wes_palette( "Darjeeling1", n=5))+
  coord_cartesian(xlim = c(1.2, NA), clip = "off")+
 
  theme_pubr()

##Changing x axis label order
org.plot <- org.plot + scale_x_discrete(name ="host", 
                     limits=c("Cattle","Chicken","Pigs", "Turkey", "Environment"))

org.1 <-org.plot+labs(x = "Host", y = "Percent resistance", 
                     title = "Organic farms")+
  theme(legend.position = "none")
              
org.1

##Conventional plots

#subseting conventionalfarm data

conv.data <- data3 %>%
  filter(farm_type == "Conventional")



conv.plot<-ggplot(conv.data, aes(x = host, y = percent_resistant, fill = host))+
  
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA
  ) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA
  ) +
  ## add justified jitter from the {gghalves} package
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .3
  ) + scale_fill_manual(values = wes_palette( "Darjeeling1", n=5))+
  coord_cartesian(xlim = c(1.2, NA), clip = "off")+
  theme_pubr()

##Changing x axis label order
conv.plot <- conv.plot + scale_x_discrete(name ="host", 
                                        limits=c("Cattle","Chicken","Pigs", "Turkey", "Environment"))



conv.1<- conv.plot+labs(x = "Host", y = "Percent resistance",
                        title = "Conventional farms")+
  theme(legend.position = "none")

conv.1

##Merge pathogen plots

host.plot <- (org.1 |conv.1)+ 
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")")+
  plot_layout(guides = "keep")&
  theme(plot.tag = element_text(),
        legend.position = "top")


ggsave(filename = "host.png",host.plot)

