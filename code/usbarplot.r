library(tidyverse)
library(dplyr)
library(ggpubr)
library(ggside)


#Load data
main_data <- read_csv("conv.csv")

#changing percentage resistance into numeric class and rounding off  
main_data$percent_resistant <- as.numeric(as.character
                                          (main_data$percent_resistant))

main_data$percent_resistant <- round(main_data$percent_resistant, digits = 0)

usa_data <- main_data %>%
  filter(country == "United States of America") %>%
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
            who_classification+percent_resistant, usa_data, mean) 
      
        
       
         
usa_data2<- usa_data1 %>%
    relocate(antimicrobial, .after = "host")%>%
  relocate(percent_resistant, .after = "who_classification")%>%
  arrange(host)




us_plot <- ggbarplot(usa_data2, "antimicrobial", "percent_resistant", 
                     fill = "farm_type",
          subtitle = "AMR of different antibiotics in the USA farms",
          xlab = "Antimicrobial", ylab = "Percentage resistance",
          legend.title = "Farm type", font.x = "bold", font.y = "bold",
          font.legend = "bold", font.subtitle = "bold")+
  rotate_x_text(90)

facet(us_plot, facet.by = "host")

us_plot+facet_wrap(host~who_classification, ncol =3)+ 
  guides(guide_axis(check.overlap = T))

##grouped bar


us_plot <- ggbarplot(usa_data2, "antimicrobial", "percent_resistant", 
                     fill = "farm_type", position = position_dodge(0.7),
                     
                     subtitle = "AMR of different antibiotics in the USA farms",
                     xlab = "Antimicrobial", ylab = "Percentage resistance",
                     legend.title = "Farm type", font.x = "bold", font.y = "bold",
                     font.legend = "bold", font.subtitle = "bold"
                    )+
  rotate_x_text(90)

facet(us_plot, facet.by = "host")

us_plot+facet_wrap(host~who_classification, ncol =3)+ 
  guides(guide_axis(check.overlap = T))




 
##Plot by Kenji et al.

kenji<- usa_data %>%
  filter(author == "Kenji et al.")

#stacked barplot

kenji_stacked <- ggbarplot(kenji, "antimicrobial", "percent_resistant", fill = "farm_type",
         subtitle = "AMR of E.coli in fecal samples of cows and calves in Wisconsin, n=1121",
          xlab = "Antimicrobial", ylab = "Percentage resistance",
          legend.title = "Farm type", font.x = "bold", font.y = "bold",
          font.legend = "bold", font.subtitle = "bold")+
 rotate_x_text(60)

kenji_stacked <- facet(kenji_stacked, facet.by = "who_classification")
##grouped bar


kenji_grouped <- ggbarplot(kenji, "antimicrobial", "percent_resistant", fill = "farm_type",
          subtitle = "AMR of E.coli in fecal samples of cows and calves in Wisconsin, n=1121",
          xlab = "Antimicrobial", ylab = "Percentage resistance",
          legend.title = "Farm type", font.x = "bold", font.y = "bold",
          font.legend = "bold", font.subtitle = "bold",
          position = position_dodge(0.7))+
  rotate_x_text(60)
kenji_grouped
##Study  by Rollo et al.


Rollo<- usa_data %>%
  filter(author == "Rollo et al.")

#stacked barplot

Rollo_stacked <- ggbarplot(Rollo, "antimicrobial", "percent_resistant", fill = "farm_type",
          subtitle = "AMR of Campylobacter in fecal samples of pigs in Mid-west USA, n=464",
          xlab = "Antimicrobial", ylab = "Percentage resistance",
          legend.title = "Farm type", font.x = "bold", font.y = "bold",
          font.legend = "bold", font.subtitle = "bold")+
  rotate_x_text(60)

Rollo_stacked
##grouped bar


Rollo_grouped <- ggbarplot(Rollo, "antimicrobial", "percent_resistant", fill = "farm_type",
          subtitle = "AMR of Campylobacter in fecal samples of pigs in Mid-west USA, n=464",
          xlab = "Antimicrobial", ylab = "Percentage resistance",
          legend.title = "Farm type", font.x = "bold", font.y = "bold",
          font.legend = "bold", font.subtitle = "bold",
          position = position_dodge(0.7))+
  rotate_x_text(60)
Rollo_grouped

##Tifosky et al.


Tikofsky <- usa_data %>%
  filter(author == "Tikofsky et al.")

#stacked barplot

Tikofsky_stacked <- ggbarplot(Tikofsky, "antimicrobial", "percent_resistant", fill = "farm_type",
          subtitle = "AMR of S.aureus in milk samples of cows in USA, n=261",
          xlab = "Antimicrobial", ylab = "Percentage resistance",
          legend.title = "Farm type", font.x = "bold", font.y = "bold",
          font.legend = "bold", font.subtitle = "bold")+
  rotate_x_text(60)

Tikofsky_stacked
##grouped bar


Tikofsky_grouped <- ggbarplot(Tikofsky, "antimicrobial", "percent_resistant", fill = "farm_type",
          subtitle = "AMR of S.aureus in milk samples of cows in USA, n=261",
          xlab = "Antimicrobial", ylab = "Percentage resistance",
          legend.title = "Farm type", font.x = "bold", font.y = "bold",
          font.legend = "bold", font.subtitle = "bold",
          position = position_dodge(0.7))+
  rotate_x_text(60)

Tikofsky_grouped

## Study by Luangtongkum et al. 

Luangtongkum <- usa_data %>%
  filter(author == "Luangtongkum et al.")



#stacked barplot

Luangto_stacked <- ggbarplot(Luangtongkum, "antimicrobial", "percent_resistant", fill = "farm_type",
          subtitle = "AMR of campylobacter in meat samples of chickens in USA, n=694",
          xlab = "Antimicrobial", ylab = "Percentage resistance",
          legend.title = "Farm type", font.x = "bold", font.y = "bold",
          font.legend = "bold", font.subtitle = "bold")+
  rotate_x_text(60)

Luangto_stacked
##grouped bar


Luangto_grouped <- ggbarplot(Luangtongkum, "antimicrobial", "percent_resistant", fill = "farm_type",
          subtitle = "AMR of campylobacter in meat samples of chickens in USA, n=694",
          xlab = "Antimicrobial", ylab = "Percentage resistance",
          legend.title = "Farm type", font.x = "bold", font.y = "bold",
          font.legend = "bold", font.subtitle = "bold",
          position = position_dodge(0.7))+
  rotate_x_text(60)
Luangto_grouped

##Tadesse et aal.


Tadesse <- usa_data %>%
  filter(author == "Tadesse et al")

#stacked barplot

Tadesse_stacked <- ggbarplot(Tadesse, "antimicrobial", "percent_resistant", fill = "farm_type",
                              subtitle = "AMR of Campylobacter in pig samples in mid-west USA, n=380",
                              xlab = "Antimicrobial", ylab = "Percentage resistance",
                              legend.title = "Farm type", font.x = "bold", font.y = "bold",
                              font.legend = "bold", font.subtitle = "bold")+
  rotate_x_text(60)

Tadesse_stacked
##grouped bar


Tadesse_grouped <- ggbarplot(Tadesse, "antimicrobial", "percent_resistant", fill = "farm_type",
                              subtitle = "AMR of Campylobacter in pig samples in mid-west USA, n=380",
                              xlab = "Antimicrobial", ylab = "Percentage resistance",
                              legend.title = "Farm type", font.x = "bold", font.y = "bold",
                              font.legend = "bold", font.subtitle = "bold",
                              position = position_dodge(0.7))+
  rotate_x_text(60)

Tadesse_grouped

##Sanchez et al.


Sanchez <- usa_data %>%
  filter(author == "Sanchez et al")

#stacked barplot

Sanchez_stacked <- ggbarplot(Sanchez, "antimicrobial", "percent_resistant", fill = "farm_type",
                             subtitle = "AMR of E.coli in Chicken samples in USA, n=174",
                             xlab = "Antimicrobial", ylab = "Percentage resistance",
                             legend.title = "Farm type", font.x = "bold", font.y = "bold",
                             font.legend = "bold", font.subtitle = "bold")+
  rotate_x_text(60)

Sanchez_stacked
##grouped bar


Sanchez_grouped <- ggbarplot(Sanchez, "antimicrobial", "percent_resistant", fill = "farm_type",
                             subtitle = "AMR of E.coli in Chicken samples in USA, n=174",
                             xlab = "Antimicrobial", ylab = "Percentage resistance",
                             legend.title = "Farm type", font.x = "bold", font.y = "bold",
                             font.legend = "bold", font.subtitle = "bold",
                             position = position_dodge(0.7))+
  rotate_x_text(60)

Sanchez_grouped



##Kim et al.


Kim <- usa_data %>%
  filter(author == "Kim et al.")

#stacked barplot

Kim_stacked <- ggbarplot(Kim, "antimicrobial", "percent_resistant", fill = "farm_type",
                             subtitle = "AMR of E.coli in Chicken  caecal samples in USA, n=120",
                             xlab = "Antimicrobial", ylab = "Percentage resistance",
                             legend.title = "Farm type", font.x = "bold", font.y = "bold",
                             font.legend = "bold", font.subtitle = "bold")+
  rotate_x_text(60)

Kim_stacked

##grouped bar
Kim_grouped <- ggbarplot(Kim, "antimicrobial", "percent_resistant", fill = "farm_type",
                             subtitle = "AMR of E.coli in Chicken  caecal samples in USA, n=120",
                             xlab = "Antimicrobial", ylab = "Percentage resistance",
                             legend.title = "Farm type", font.x = "bold", font.y = "bold",
                             font.legend = "bold", font.subtitle = "bold",
                             position = position_dodge(0.7))+
  rotate_x_text(60)

Kim_grouped


##Kilonzo-Nthenge et al

Kilonzo <- usa_data %>%
  filter(author == "Kilonzo-Nthenge et al")

#stacked barplot

Kilonzo_stacked <- ggbarplot(Kilonzo, "antimicrobial", "percent_resistant", fill = "farm_type",
                         subtitle = "AMR of Enterococcus in Chicken meat samples in Tennessee USA, n=265",
                         xlab = "Antimicrobial", ylab = "Percentage resistance",
                         legend.title = "Farm type", font.x = "bold", font.y = "bold",
                         font.legend = "bold", font.subtitle = "bold")+
  rotate_x_text(60)

Kilonzo_stacked

##grouped bar
Kilonzo_grouped <- ggbarplot(Kilonzo, "antimicrobial", "percent_resistant", fill = "farm_type",
                         subtitle = "AMR of Enterococcus in Chicken meat samples in Tennessee USA, n=265",
                         xlab = "Antimicrobial", ylab = "Percentage resistance",
                         legend.title = "Farm type", font.x = "bold", font.y = "bold",
                         font.legend = "bold", font.subtitle = "bold",
                         position = position_dodge(0.7))+
  rotate_x_text(60)

Kilonzo_grouped

##Bailley et al


Bailley <- usa_data %>%
  filter(author == "Bailley et al")

Bailley_stacked <- ggbarplot(Bailley, "antimicrobial", "percent_resistant", fill = "farm_type",
                         subtitle = "AMR of Salmonella in Chicken  fecal samples in USA, n=40",
                         xlab = "Antimicrobial", ylab = "Percentage resistance",
                         legend.title = "Farm type", font.x = "bold", font.y = "bold",
                         font.legend = "bold", font.subtitle = "bold")+
  rotate_x_text(60)

Bailley_stacked

##grouped bar
Bailley_grouped <- ggbarplot(Bailley, "antimicrobial", "percent_resistant", fill = "farm_type",
                         subtitle = "AMR of Salmonella in Chicken  fecal samples in USA, n=40",
                         xlab = "Antimicrobial", ylab = "Percentage resistance",
                         legend.title = "Farm type", font.x = "bold", font.y = "bold",
                         font.legend = "bold", font.subtitle = "bold",
                         position = position_dodge(0.7))+
  rotate_x_text(60)

Bailley_grouped


