library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(patchwork)


setwd("C:/Users/Moses/Desktop/conventionalvs organic project/AMR_conventionalvsorganic")

#Load data
main_data <- read_csv("conv.csv", na = "empty")

#changing percentage resistance into numeric class and rounding off  
main_data$percent_resistant <- as.numeric(as.character
                                          (main_data$percent_resistant))

main_data$percent_resistant <- round(main_data$percent_resistant, digits = 2)

bar_data <- main_data %>%
  group_by(author) %>%
  select("country",
         "farm_type",
         "sample_type",
         "host",
         "antimicrobial_compound",
         "pathogen",
         "strain",
         "percent_resistant",
         "no_isolates"
         
  )

bar_data <- bar_data %>%
  na.omit()
##Showing the countries in the data set
bar_data %>%
  count(country) %>%
  view()

##USA data

USA <- bar_data %>%
  filter(country == "United States of America") %>%
  select("country",
         "farm_type",
         "sample_type",
         "host",
         "antimicrobial_compound",
         "pathogen",
         "strain",
         "percent_resistant",
         "no_isolates"
         
  )
  
##USA bar plot

us_plot <- ggplot(USA, aes(x = antimicrobial_compound, 
                        y = percent_resistant, 
                        fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(subtitle = "USA, n=9,148",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type") + 
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
us_plot


#Greece data

Greece <- bar_data %>%
  filter(country == "Greece") %>%
  select("country",
         "farm_type",
         "sample_type",
         "host",
         "antimicrobial_compound",
         "pathogen",
         "strain",
         "percent_resistant",
         "no_isolates"
         
  )

##Greece bar plot

Greece_plot <- ggplot(Greece, aes(x = antimicrobial_compound, 
                           y = percent_resistant, 
                           fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(subtitle = "Greece, n=41",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type") + 
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
Greece_plot


#Germany data

Germany <- bar_data %>%
  filter(country == "Germany") %>%
  select("country",
         "farm_type",
         "sample_type",
         "host",
         "antimicrobial_compound",
         "pathogen",
         "strain",
         "percent_resistant",
         "no_isolates"
         
  )

##Germany bar plot

Germany_plot <- ggplot(Germany, aes(x = antimicrobial_compound, 
                                  y = percent_resistant, 
                                  fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(subtitle = "Germany, n=629",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type") + 
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
Germany_plot

#France data

France <- bar_data %>%
  filter(country == "France") %>%
  select("country",
         "farm_type",
         "sample_type",
         "host",
         "antimicrobial_compound",
         "pathogen",
         "strain",
         "percent_resistant",
         "no_isolates"
         
  )

##France bar plot

France_plot <- ggplot(France, aes(x = antimicrobial_compound, 
                                    y = percent_resistant, 
                                    fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(subtitle = "France, n=457",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type") + 
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
France_plot

#Sweden data

Sweden <- bar_data %>%
  filter(country == "Sweden") %>%
  select("country",
         "farm_type",
         "sample_type",
         "host",
         "antimicrobial_compound",
         "pathogen",
         "strain",
         "percent_resistant",
         "no_isolates"
         
  )

##Sweden bar plot

Sweden_plot <- ggplot(Sweden, aes(x = antimicrobial_compound, 
                                  y = percent_resistant, 
                                  fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(subtitle = "Sweden, n=1,070",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type") + 
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
Sweden_plot


#S.korea data

S.korea <- bar_data %>%
  filter(country == "South Korea") %>%
  select("country",
         "farm_type",
         "sample_type",
         "host",
         "antimicrobial_compound",
         "pathogen",
         "strain",
         "percent_resistant",
         "no_isolates"
         
  )

##S.korea bar plot

s.korea_plot <- ggplot(S.korea, aes(x = antimicrobial_compound, 
                                  y = percent_resistant, 
                                  fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(subtitle = "South Korea, n=204",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type") + 
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
s.korea_plot




#Spain data

Spain <- bar_data %>%
  filter(country == "Spain") %>%
  select("country",
         "farm_type",
         "sample_type",
         "host",
         "antimicrobial_compound",
         "pathogen",
         "strain",
         "percent_resistant",
         "no_isolates"
         
  )

##Spain bar plot

Spain_plot <- ggplot(Spain, aes(x = antimicrobial_compound, 
                                    y = percent_resistant, 
                                    fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(subtitle = "Spain, n=120",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type") + 
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
Spain_plot



#Austria data

Austria <- bar_data %>%
  filter(country == "Austria") %>%
  select("country",
         "farm_type",
         "sample_type",
         "host",
         "antimicrobial_compound",
         "pathogen",
         "strain",
         "percent_resistant",
         "no_isolates"
         
  )

##Austria bar plot

Austria_plot <- ggplot(Austria, aes(x = antimicrobial_compound, 
                                y = percent_resistant, 
                                fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(subtitle = "Austria, n=962",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type") + 
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
Austria_plot

#Italy data

Italy <- bar_data %>%
  filter(country == "Italy") %>%
  select("country",
         "farm_type",
         "sample_type",
         "host",
         "antimicrobial_compound",
         "pathogen",
         "strain",
         "percent_resistant",
         "no_isolates"
         
  )

##Italy bar plot

Italy_plot <- ggplot(Italy, aes(x = antimicrobial_compound, 
                                    y = percent_resistant, 
                                    fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(subtitle = "Italy, n=400",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type") + 
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
Italy_plot


#New zealand data

NZD <- bar_data %>%
  filter(country == "New Zealand") %>%
  select("country",
         "farm_type",
         "sample_type",
         "host",
         "antimicrobial_compound",
         "pathogen",
         "strain",
         "percent_resistant",
         "no_isolates"
         
  )

##New zealand bar plot

NZD_plot <- ggplot(NZD, aes(x = antimicrobial_compound, 
                                y = percent_resistant, 
                                fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(subtitle = "New Zealand, n=814",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type") + 
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
NZD_plot


#Denmark data

Denmark <- bar_data %>%
  filter(country == "Denmark") %>%
  select("country",
         "farm_type",
         "sample_type",
         "host",
         "antimicrobial_compound",
         "pathogen",
         "strain",
         "percent_resistant",
         "no_isolates"
         
  )

##Denmark bar plot

Denmark_plot <- ggplot(Denmark, aes(x = antimicrobial_compound, 
                            y = percent_resistant, 
                            fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(subtitle = "Denmark, n=52",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type") + 
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
Denmark_plot

#Thailand data

Thailand <- bar_data %>%
  filter(country == "Thailand") %>%
  select("country",
         "farm_type",
         "sample_type",
         "host",
         "antimicrobial_compound",
         "pathogen",
         "strain",
         "percent_resistant",
         "no_isolates"
         
  )

##Thailand bar plot

Thailand_plot <- ggplot(Thailand, aes(x = antimicrobial_compound, 
                            y = percent_resistant, 
                            fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(subtitle = "Thailand, n=162",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type") + 
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
Thailand_plot



##Marging plots together using patch work


(us_plot | Greece_plot | Germany_plot) /
    (France_plot | Sweden_plot | s.korea_plot) /
    (Spain_plot | Austria_plot | Italy_plot) /
    (NZD_plot | Denmark_plot | Thailand_plot) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a", tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(face = "bold"))

