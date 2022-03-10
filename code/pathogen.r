library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(patchwork)
#Load data
main_data <- read_csv("conv.csv")

#changing percentage resistance into numeric class and rounding off  
main_data$percent_resistant <- as.numeric(as.character
                                          (main_data$percent_resistant))

main_data$percent_resistant <- round(main_data$percent_resistant, digits = 2)


bar_data <- main_data %>%
  group_by(pathogen) %>%
  select("country",
         "farm_type",
         "antimicrobial_compound",
         "pathogen",
         "strain",
         "percent_resistant"
         
         )

##E.coli plot

e.coli <- bar_data %>%
  filter(pathogen == "E.coli") %>%
  select("country",
         "farm_type",
         "antimicrobial_compound",
         "pathogen",
         "strain",
         "percent_resistant"
         
         )

##e.coli bar plot

e.coli_plot <- ggplot(e.coli, aes(x = antimicrobial_compound, 
                           y = percent_resistant, 
                           fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(subtitle = "E.coli",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type") + 
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
e.coli_plot


##Salmonella plot

salmonella <- bar_data %>%
  filter(pathogen == "Salmonella") %>%
  select("country",
         "farm_type",
         "antimicrobial_compound",
         "pathogen",
         "strain",
         "percent_resistant"
         
  )

##Salmonella bar plot

salmonella_plot <- ggplot(salmonella, aes(x = antimicrobial_compound, 
                                  y = percent_resistant, 
                                  fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(subtitle = "Salmonella",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type") + 
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
salmonella_plot



##Campylobacter plot

campy <- bar_data %>%
  filter(pathogen == "Campylobacter") %>%
  select("country",
         "farm_type",
         "antimicrobial_compound",
         "pathogen",
         "strain",
         "percent_resistant"
         
  )

##Campylobacter bar plot

campy_plot <- ggplot(campy, aes(x = antimicrobial_compound, 
                                  y = percent_resistant, 
                                  fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(subtitle = "Campylobacter",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type") + 
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
campy_plot


##A.pyogenes plot

a.pyogn <- bar_data %>%
  filter(pathogen == "Arcanobacterium pyogenes") %>%
  select("country",
         "farm_type",
         "antimicrobial_compound",
         "pathogen",
         "strain",
         "percent_resistant"
         
  )

##A.pyogenes bar plot

a.pyogn_plot <- ggplot(a.pyogn, aes(x = antimicrobial_compound, 
                                  y = percent_resistant, 
                                  fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(subtitle = "Arcanobacterium pyogenes",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type") + 
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
a.pyogn_plot


##Corynebacterium bovis plot

c.bovis <- bar_data %>%
  filter(pathogen == "Corynebacterium bovis") %>%
  select("country",
         "farm_type",
         "antimicrobial_compound",
         "pathogen",
         "strain",
         "percent_resistant"
         
  )

##c.bovis bar plot

c.bovis_plot <- ggplot(c.bovis, aes(x = antimicrobial_compound, 
                                  y = percent_resistant, 
                                  fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(subtitle = "Corynebacterium bovis",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type") + 
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
c.bovis_plot


##E.streptococci plot

e.str <- bar_data %>%
  filter(pathogen == "Environmental streptococci") %>%
  select("country",
         "farm_type",
         "antimicrobial_compound",
         "pathogen",
         "strain",
         "percent_resistant"
         
  )

##E.streptococci bar plot

e.str_plot <- ggplot(e.str, aes(x = antimicrobial_compound, 
                                  y = percent_resistant, 
                                  fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(subtitle = "Environmental streptococci",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type") + 
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
e.str_plot

##Staphylococcus plot

staphyl <- bar_data %>%
  filter(pathogen == "Staphylococcus") %>%
  select("country",
         "farm_type",
         "antimicrobial_compound",
         "pathogen",
         "strain",
         "percent_resistant"
         
  )

##staphylococcus bar plot

staphyl_plot <- ggplot(staphyl, aes(x = antimicrobial_compound, 
                                  y = percent_resistant, 
                                  fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(subtitle = "Stapylococcus spp",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type") + 
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
staphyl_plot


##S.aureus plot

s.aureus <- bar_data %>%
  filter(pathogen == "S.aureus") %>%
  select("country",
         "farm_type",
         "antimicrobial_compound",
         "pathogen",
         "strain",
         "percent_resistant"
         
  )

##s.aureus bar plot

s.aureus_plot <- ggplot(s.aureus, aes(x = antimicrobial_compound, 
                                  y = percent_resistant, 
                                  fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(subtitle = "Staphylococcus aureus",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type") + 
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
s.aureus_plot


##Enterococcus plot

entero <- bar_data %>%
  filter(pathogen == "Enterococcus") %>%
  select("country",
         "farm_type",
         "antimicrobial_compound",
         "pathogen",
         "strain",
         "percent_resistant"
         
  )

##Enterococcus bar plot

entero_plot <- ggplot(entero, aes(x = antimicrobial_compound, 
                                      y = percent_resistant, 
                                      fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(subtitle = "Enterococcus",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type") + 
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
entero_plot

##marging plots using patchwork

pathogen_plot <- (e.coli_plot | campy_plot) /
  (salmonella_plot | entero_plot) /
  (staphyl_plot | s.aureus_plot) /
  (a.pyogn_plot | c.bovis_plot | e.str_plot) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a", tag_prefix = "(",
                  tag_suffix = ")") &
  theme(plot.tag = element_text(face = "bold"))

pathogen_plot


##Campylobacter strain plots


##Campylobacter strains plot

c_strain <- bar_data %>%
  filter(strain == "C.coli" |  strain== "C.jejuni") %>%
  select("country",
         "farm_type",
         "antimicrobial_compound",
         "pathogen",
         "strain",
         "percent_resistant"
         
  )

##Camylobacter strain bar plot

strain_plot <- ggplot(c_strain, aes(x = antimicrobial_compound, 
                                  y = percent_resistant, 
                                  fill = farm_type
)
) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(subtitle = "C.coli and C.jejuni resistance",
       x = "Antimicrobial", y = "Percentage resistance", 
       fill = "farm type") + 
  facet_wrap(~strain)+
  theme_bw() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
strain_plot


             