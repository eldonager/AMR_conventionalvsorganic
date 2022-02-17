library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(patchwork)
#Load data
main_data <- read_csv("conv.csv", na = "empty")

#changing percentage resistance into numeric class and rounding off  
main_data$percent_resistant <- as.numeric(as.character
                                          (main_data$percent_resistant))

main_data$percent_resistant <- round(main_data$percent_resistant, digits = 2)


host_data <- main_data %>%
  group_by(host) %>%
  select("country",
         "farm_type",
         "antimicrobial_compound",
         "pathogen",
         "percent_resistant"
         
         )


##Cows and calves  plot

cows <- host_data %>%
  filter(host == "Cows and Calves") %>%
  select("country",
         "farm_type",
         "antimicrobial_compound",
         "pathogen",
         "percent_resistant"
         
         )

##Cows and calves bar plot

cows_plot <- ggplot(cows, aes(x = antimicrobial_compound, 
                                  y = percent_resistant, 
                                  fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(subtitle = "Cows and calves, E.coli",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type") + 
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
cows_plot


##Chicken  plot

chicken <- host_data %>%
  filter(host == "Chicken") %>%
  select("country",
         "farm_type",
         "antimicrobial_compound",
         "pathogen",
         "percent_resistant"
         
  )

##Chicken bar plot

chicken_plot <- ggplot(chicken, aes(x = antimicrobial_compound, 
                              y = percent_resistant, 
                              fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(subtitle = "Chicken",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type") + 
  facet_wrap(~country, nrow = 7)+
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
chicken_plot



##Chicken  plot

chicken <- host_data %>%
  filter(host == "Chicken") %>%
  select("country",
         "farm_type",
         "antimicrobial_compound",
         "pathogen",
         "percent_resistant"
         
  )

##Chicken bar plot

chicken1_plot <- ggplot(chicken, aes(x = antimicrobial_compound, 
                                    y = percent_resistant, 
                                    fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(subtitle = "Chicken",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type") + 
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
chicken1_plot

##Environment  plot

env <- host_data %>%
  filter(host == "Environment") %>%
  select("country",
         "farm_type",
         "antimicrobial_compound",
         "pathogen",
         "percent_resistant"
         
  )

##Environment bar plot

env_plot <- ggplot(env, aes(x = antimicrobial_compound, 
                                    y = percent_resistant, 
                                    fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(subtitle = "Environment, E.coli",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type") + 
  facet_wrap(~country)+
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
env_plot



##Environment  plot

env <- host_data %>%
  filter(host == "Environment") %>%
  select("country",
         "farm_type",
         "antimicrobial_compound",
         "pathogen",
         "percent_resistant"
         
  )

##Environment bar plot

env1_plot <- ggplot(env, aes(x = antimicrobial_compound, 
                            y = percent_resistant, 
                            fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(subtitle = "Environment, E.coli",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type") + 
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
env1_plot

##Pig plot

pig <- host_data %>%
  filter(host == "Pigs") %>%
  select("country",
         "farm_type",
         "antimicrobial_compound",
         "pathogen",
         "percent_resistant"
         
  )

##Pig bar plot

pig_plot <- ggplot(pig, aes(x = antimicrobial_compound, 
                            y = percent_resistant, 
                            fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(subtitle = "Pigs",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type") + 
  facet_wrap(~country, nrow = 3)+
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
pig_plot


##Pig plot

pig <- host_data %>%
  filter(host == "Pigs") %>%
  select("country",
         "farm_type",
         "antimicrobial_compound",
         "pathogen",
         "percent_resistant"
         
  )

##Pig bar plot

pig1_plot <- ggplot(pig, aes(x = antimicrobial_compound, 
                            y = percent_resistant, 
                            fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(subtitle = "Pigs",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type") + 
    theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
pig1_plot


##Turkey plot

turk <- host_data %>%
  filter(host == "Turkey") %>%
  select("country",
         "farm_type",
         "antimicrobial_compound",
         "pathogen",
         "percent_resistant"
         
  )

##Turkey bar plot

turk_plot <- ggplot(turk, aes(x = antimicrobial_compound, 
                            y = percent_resistant, 
                            fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(subtitle = "Turkey, campylobacter",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type") + 
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
turk_plot



##Cattle plot

cattle <- host_data %>%
  filter(host == "Cattle") %>%
  select("country",
         "farm_type",
         "antimicrobial_compound",
         "pathogen",
         "percent_resistant"
         
  )

##Cattle bar plot

cattle_plot <- ggplot(cattle, aes(x = antimicrobial_compound, 
                            y = percent_resistant, 
                            fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(subtitle = "Cattle",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type") + 
  facet_wrap(~country, nrow = 3)+
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
cattle_plot



##Cattle plot

cattle <- host_data %>%
  filter(host == "Cattle") %>%
  select("country",
         "farm_type",
         "antimicrobial_compound",
         "pathogen",
         "percent_resistant"
         
  )

##Cattle bar plot

cattle1_plot <- ggplot(cattle, aes(x = antimicrobial_compound, 
                                  y = percent_resistant, 
                                  fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(subtitle = "Cattle",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type") + 
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
cattle1_plot



##marging plots using patchwork

host_plot <- (cows_plot | chicken1_plot) /
  (cattle1_plot | pig1_plot) /
  (env1_plot | turk_plot) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a", tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(face = "bold"))

host_plot