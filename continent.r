library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(patchwork)

#Load data
main_data <- read_csv("conv.csv")


data1 %>%
  count(continent) %>%
  View()


data1 <- data1%>%
  mutate(
    continent =
      ifelse(continent == "Oceania", 
             "New Zealand", continent
      )
  )





data1 <- main_data %>%
  group_by(country) %>%
  select("continent",
         "farm_type",
         "host",
         "antimicrobial_compound",
         "antimicrobial_class",
         "pathogen",
         "percent_resistant",
         "no_isolates" )

#changing percentage resistance into numeric class and rounding off  
data1$percent_resistant <- as.numeric(as.character
                                          (data1$percent_resistant))

data1$percent_resistant <- round(data1$percent_resistant, digits = 2)

data2 <- data1 %>%
  select("continent",
         "farm_type",
         "host",
         "antimicrobial_compound",
         "antimicrobial_class",
         "pathogen",
         "percent_resistant",
         "no_isolates" )

##isolate calculation
isolate <- data2 %>%select("continent",
                           "farm_type",
                           
                           "antimicrobial_compound",
                           
                           "pathogen",
                           "percent_resistant",
                           "no_isolates" )%>%
  filter(continent == "Asia", pathogen == "Staphylococcus")
  
data3 <- aggregate(percent_resistant~continent+farm_type+
                     antimicrobial_compound+
                         pathogen+percent_resistant, data2, mean) %>%
  arrange(continent)

data3 <- data3 %>%select("continent",
                         "farm_type",
                         "antimicrobial_compound",
                        
                         "pathogen",
                         "percent_resistant")%>%
filter(pathogen != "Arcanobacterium pyogenes", pathogen != "Corynebacterium bovis",
           pathogen != "Environmental streptococci")
 

#changing percentage resistance into numeric class and rounding off  
data3$percent_resistant <- as.numeric(as.character
                                      (data3$percent_resistant))

data3$percent_resistant <- round(data3$percent_resistant, digits = 0)


##Asia e.coli bar plot

asia.ecoli <- data3 %>%
  filter(continent == "Asia", pathogen == "E.coli")



asia.ecoli.plot <- ggbarplot(asia.ecoli, "antimicrobial_compound", "percent_resistant", 
          fill = "farm_type", position = position_dodge(0.7),
          subtitle = "Asia, E.coli = 215",
          xlab = "Antimicrobial", ylab = "Percentage resistance",
          legend.title = "Farm type", font.x = "bold", font.y = "bold",
          font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)

asia.ecoli.plot


##Asia enterococcus bar plot

asia.entero <- data3 %>%
  filter(continent == "Asia", pathogen == "Enterococcus")



asia.entero.plot <- ggbarplot(asia.entero, "antimicrobial_compound", "percent_resistant", 
                             fill = "farm_type", position = position_dodge(0.7),
                             subtitle = "Asia, Enterococcus = 104",
                             xlab = "Antimicrobial", ylab = "Percentage resistance",
                             legend.title = "Farm type", font.x = "bold", font.y = "bold",
                             font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)

asia.entero.plot


##Asia listeria bar plot

asia.list <- data3 %>%
  filter(continent == "Asia", pathogen == "Listeria")



asia.listeria.plot <- ggbarplot(asia.list, "antimicrobial_compound", "percent_resistant", 
                              fill = "farm_type", position = position_dodge(0.7),
                              subtitle = "Asia, Listeria = 96",
                              xlab = "Antimicrobial", ylab = "Percentage resistance",
                              legend.title = "Farm type", font.x = "bold", font.y = "bold",
                              font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)

asia.listeria.plot


##Asia Salmonella bar plot

asia.sal <- data3 %>%
  filter(continent == "Asia", pathogen == "Salmonella")



asia.salmonella.plot <- ggbarplot(asia.sal, "antimicrobial_compound", "percent_resistant", 
                                fill = "farm_type", position = position_dodge(0.7),
                                subtitle = "Asia, Salmonella = 556",
                                xlab = "Antimicrobial", ylab = "Percentage resistance",
                                legend.title = "Farm type", font.x = "bold", font.y = "bold",
                                font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)

asia.salmonella.plot


##Asia Staphylococcus bar plot

asia.staph <- data3 %>%
  filter(continent == "Asia", pathogen == "Staphylococcus")



asia.Staphylococcus.plot <- ggbarplot(asia.staph, "antimicrobial_compound", "percent_resistant", 
                                  fill = "farm_type", position = position_dodge(0.7),
                                  subtitle = "Asia, Staphylococcus = 162",
                                  xlab = "Antimicrobial", ylab = "Percentage resistance",
                                  legend.title = "Farm type", font.x = "bold", font.y = "bold",
                                  font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)

asia.Staphylococcus.plot


##Europe Campylobacter bar plot

europe.campy <- data3 %>%
  filter(continent == "Europe", pathogen == "Campylobacter")



europe.Campylobacter.plot <- ggbarplot(europe.campy, "antimicrobial_compound", "percent_resistant", 
                                      fill = "farm_type", position = position_dodge(0.7),
                                      subtitle = "Europe, Campylobacter = 712",
                                      xlab = "Antimicrobial", ylab = "Percentage resistance",
                                      legend.title = "Farm type", font.x = "bold", font.y = "bold",
                                      font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)

europe.Campylobacter.plot


##Europe E.coli bar plot

europe.ecoli <- data3 %>%
  filter(continent == "Europe", pathogen == "E.coli")



europe.ecoli.plot <- ggbarplot(europe.ecoli, "antimicrobial_compound", "percent_resistant", 
                                     fill = "farm_type", position = position_dodge(0.7),
                                     subtitle = "Europe, E.coli = 5,313",
                                     xlab = "Antimicrobial", ylab = "Percentage resistance",
                                     legend.title = "Farm type", font.x = "bold", font.y = "bold",
                                     font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)

europe.ecoli.plot


##Europe Enterococcus bar plot

europe.entero <- data3 %>%
  filter(continent == "Europe", pathogen == "Enterococcus")



europe.enterococcus.plot <- ggbarplot(europe.entero, "antimicrobial_compound", "percent_resistant", 
                               fill = "farm_type", position = position_dodge(0.7),
                               subtitle = "Europe, Enterococcus = 184",
                               xlab = "Antimicrobial", ylab = "Percentage resistance",
                               legend.title = "Farm type", font.x = "bold", font.y = "bold",
                               font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)

europe.enterococcus.plot

##Europe Listeria bar plot

europe.list <- data3 %>%
  filter(continent == "Europe", pathogen == "Listeria")



europe.listeria.plot <- ggbarplot(europe.list, "antimicrobial_compound", "percent_resistant", 
                                      fill = "farm_type", position = position_dodge(0.7),
                                      subtitle = "Europe, Listeria = 71",
                                      xlab = "Antimicrobial", ylab = "Percentage resistance",
                                      legend.title = "Farm type", font.x = "bold", font.y = "bold",
                                      font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)

europe.listeria.plot


##Europe MRSA bar plot

europe.MRSA <- data3 %>%
  filter(continent == "Europe", pathogen == "MRSA")



europe.MRSA.plot <- ggbarplot(europe.MRSA, "antimicrobial_compound", "percent_resistant", 
                                  fill = "farm_type", position = position_dodge(0.7),
                                  subtitle = "Europe, MRSA = 273",
                                  xlab = "Antimicrobial", ylab = "Percentage resistance",
                                  legend.title = "Farm type", font.x = "bold", font.y = "bold",
                                  font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)

europe.MRSA.plot


##Europe S.aureus bar plot

europe.saureus <- data3 %>%
  filter(continent == "Europe", pathogen == "S.aureus")



europe.s.aureus.plot <- ggbarplot(europe.saureus, "antimicrobial_compound", "percent_resistant", 
                              fill = "farm_type", position = position_dodge(0.7),
                              subtitle = "Europe, S.aureus = 688",
                              xlab = "Antimicrobial", ylab = "Percentage resistance",
                              legend.title = "Farm type", font.x = "bold", font.y = "bold",
                              font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)

europe.s.aureus.plot


##North America, campylobacter bar plot

N.A.campylobacter <- data3 %>%
  filter(continent == "North America", pathogen == "Campylobacter")



N.A.campylobacter.plot <- ggbarplot(N.A.campylobacter, "antimicrobial_compound", "percent_resistant", 
                                  fill = "farm_type", position = position_dodge(0.7),
                                  subtitle = "North America, Campylobacter = 9,219",
                                  xlab = "Antimicrobial", ylab = "Percentage resistance",
                                  legend.title = "Farm type", font.x = "bold", font.y = "bold",
                                  font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)

N.A.campylobacter.plot

##North America, E.coli bar plot

N.A.ecoli <- data3 %>%
  filter(continent == "North America", pathogen == "E.coli")



N.A.ecoli.plot <- ggbarplot(N.A.ecoli, "antimicrobial_compound", "percent_resistant", 
                                    fill = "farm_type", position = position_dodge(0.7),
                                    subtitle = "North America, E.coli = 12,509",
                                    xlab = "Antimicrobial", ylab = "Percentage resistance",
                                    legend.title = "Farm type", font.x = "bold", font.y = "bold",
                                    font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)

N.A.ecoli.plot


##North America, Enterococcus bar plot

N.A.entero <- data3 %>%
  filter(continent == "North America", pathogen == "Enterococcus")



N.A.entero.plot <- ggbarplot(N.A.entero, "antimicrobial_compound", "percent_resistant", 
                            fill = "farm_type", position = position_dodge(0.7),
                            subtitle = "North America, Enterococcus = 546",
                            xlab = "Antimicrobial", ylab = "Percentage resistance",
                            legend.title = "Farm type", font.x = "bold", font.y = "bold",
                            font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)

N.A.entero.plot



##North America, S.aureus bar plot

N.A.saureus <- data3 %>%
  filter(continent == "North America", pathogen == "S.aureus")



N.A.saureus.plot <- ggbarplot(N.A.saureus, "antimicrobial_compound", "percent_resistant", 
                             fill = "farm_type", position = position_dodge(0.7),
                             subtitle = "North America, S.aureus = 486",
                             xlab = "Antimicrobial", ylab = "Percentage resistance",
                             legend.title = "Farm type", font.x = "bold", font.y = "bold",
                             font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)

N.A.saureus.plot



##North America, Salmonella bar plot

N.A.sal <- data3 %>%
  filter(continent == "North America", pathogen == "Salmonella")



N.A.salmonella.plot <- ggbarplot(N.A.sal, "antimicrobial_compound", "percent_resistant", 
                              fill = "farm_type", position = position_dodge(0.7),
                              subtitle = "North America, Salmonella = 3,111",
                              xlab = "Antimicrobial", ylab = "Percentage resistance",
                              legend.title = "Farm type", font.x = "bold", font.y = "bold",
                              font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)

N.A.salmonella.plot



##North America, Staphylococcus bar plot

N.A.staphy <- data3 %>%
  filter(continent == "North America", pathogen == "Staphylococcus")



N.A.Staphylococcus.plot <- ggbarplot(N.A.staphy, "antimicrobial_compound", "percent_resistant", 
                                 fill = "farm_type", position = position_dodge(0.7),
                                 subtitle = "North America, Staphylococcus = 405",
                                 xlab = "Antimicrobial", ylab = "Percentage resistance",
                                 legend.title = "Farm type", font.x = "bold", font.y = "bold",
                                 font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)

N.A.Staphylococcus.plot



##Oceania, E.coli bar plot

oceania.ecoli <- data3 %>%
  filter(continent == "Oceania", pathogen == "E.coli")



oceania.ecoli.plot <- ggbarplot(oceania.ecoli, "antimicrobial_compound", "percent_resistant", 
                                     fill = "farm_type", position = position_dodge(0.7),
                                     subtitle = "Oceania, E.coli = 1,189",
                                     xlab = "Antimicrobial", ylab = "Percentage resistance",
                                     legend.title = "Farm type", font.x = "bold", font.y = "bold",
                                     font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)

oceania.ecoli.plot




##Oceania, Enterococcus bar plot

oceania.entero <- data3 %>%
  filter(continent == "Oceania", pathogen == "Enterococcus")



oceania.enterococcus.plot <- ggbarplot(oceania.entero, "antimicrobial_compound", "percent_resistant", 
                                fill = "farm_type", position = position_dodge(0.7),
                                subtitle = "Oceania, Enterococcus = 353",
                                xlab = "Antimicrobial", ylab = "Percentage resistance",
                                legend.title = "Farm type", font.x = "bold", font.y = "bold",
                                font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)

oceania.enterococcus.plot


##South America, E.coli bar plot

S.A.ecoli <- data3 %>%
  filter(continent == "South America", pathogen == "E.coli")



S.A.ecoli.plot <- ggbarplot(S.A.ecoli, "antimicrobial_compound", "percent_resistant", 
                                       fill = "farm_type", position = position_dodge(0.7),
                                       subtitle = "South America, E.coli = 156",
                                       xlab = "Antimicrobial", ylab = "Percentage resistance",
                                       legend.title = "Farm type", font.x = "bold", font.y = "bold",
                                       font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)

S.A.ecoli.plot










