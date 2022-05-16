library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(patchwork)

#Load data
main_data <- read_csv("conv.csv")




data1 <- main_data%>%
  mutate(
    continent =
      ifelse(continent == "Oceania", 
             "New Zealand", continent
      )
  )





data1 <- main_data %>%
  group_by(country) %>%
  select("continent",
         "doi",
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

data1$percent_resistant <- round(data1$percent_resistant, digits = 0)

data2 <- data1 %>%
  select("doi",
    "continent",
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
                           "doi",
                           "antimicrobial_compound",
                           
                           "pathogen",
                           "percent_resistant",
                           "no_isolates" )%>%
  filter(continent == "Asia", pathogen == "Staphylococcus")
  
data3 <- aggregate(percent_resistant~antimicrobial_compound+pathogen+continent+
                     farm_type+no_isolates,
                     data2, mean)
                         


data3 <- data3 %>%select("continent",
                         "farm_type",
                         "antimicrobial_compound",
                        "pathogen",
                         "percent_resistant", "no_isolates")%>%
filter(pathogen != "Arcanobacterium pyogenes", pathogen != "Corynebacterium bovis",
           pathogen != "Environmental streptococci")
 

#changing percentage resistance into numeric class and rounding off  
data3$percent_resistant <- as.numeric(as.character
                                      (data3$percent_resistant))

data3$percent_resistant <- round(data3$percent_resistant, digits = 0)

# Compute the 95% CI of proportion where x = p_hat and y = n two tailed z = 1.96
CI.function <- function(x,y) {
  x + c(-1.96,1.96)*sqrt(x*(1-x)/y)}

#95% CI
CI_data <- as.data.frame(t(mapply(CI.function, data3$percent_resistant/100,
                                  data3$no_isolates)))

data3.1<- cbind(data3, round(CI_data*100, digits = 0))
data3 <- data3.1
##New col names
colnames(data3) = c("continent", "farm_type",
                      "antimicrobial_compound",
                      "pathogen", "percent_resistant",
                      "no_isolates","CILow","CIHigh")

##Reducing -1 range to 1 and 100+ to 100.
data3$CILow[data3$CILow < 0] = 0
data3$CIHigh[data3$CIHigh >100] = 100

#remove drugs where resistance = 0
data3 <- data3%>%
  filter(percent_resistant>1)

##Asia e.coli bar plot

asia.ecoli <- data3 %>%
  filter(continent == "Asia", pathogen == "E.coli")

##Ordering bar graphs in descending order
asia.ecoli<- asia.ecoli[order(-asia.ecoli$percent_resistant), ]

asia.ecoli.plot <- ggbarplot(asia.ecoli, "antimicrobial_compound", "percent_resistant", 
          fill = "farm_type", position = position_dodge(0.7),
          subtitle = "Asia, E.coli = 215",
          xlab = FALSE, ylab = FALSE,
          legend.title = "Farm type",
          font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.5))

asia.ecoli.plot


##Asia enterococcus bar plot

asia.entero <- data3 %>%
  filter(continent == "Asia", pathogen == "Enterococcus")

asia.entero<- asia.entero[order(-asia.entero$percent_resistant), ]

asia.entero.plot <- ggbarplot(asia.entero, "antimicrobial_compound", "percent_resistant", 
                             fill = "farm_type", 
                             position = position_dodge(0.7),
                             subtitle = "Asia, Enterococcus = 104",
                             xlab = FALSE, ylab = FALSE,
                             legend.title = "Farm type",
                             font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.5))

asia.entero.plot


##Asia listeria bar plot

asia.list <- data3 %>%
  filter(continent == "Asia", pathogen == "Listeria")

asia.list<- asia.list[order(-asia.list$percent_resistant), ]

asia.listeria.plot <- ggbarplot(asia.list, "antimicrobial_compound", "percent_resistant", 
                              fill = "farm_type", position = position_dodge(0.7),
                              subtitle = "Asia, Listeria = 96",
                              xlab = FALSE, ylab = FALSE,
                              legend.title = "Farm type",
                              font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.5))


asia.listeria.plot



##Asia Salmonella bar plot

asia.sal <- data3 %>%
  filter(continent == "Asia", pathogen == "Salmonella")

asia.sal<- asia.sal[order(-asia.sal$percent_resistant), ]

asia.salmonella.plot <- ggbarplot(asia.sal, "antimicrobial_compound", "percent_resistant", 
                                fill = "farm_type", position = position_dodge(0.7),
                                subtitle = "Asia, Salmonella = 556",
                                xlab = FALSE, ylab = FALSE,
                                legend.title = "Farm type",
                                font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.5))


asia.salmonella.plot


##Asia Staphylococcus bar plot

asia.staph <- data3 %>%
  filter(continent == "Asia", pathogen == "Staphylococcus")

asia.staph<- asia.staph[order(-asia.staph$percent_resistant), ]


asia.Staphylococcus.plot <- ggbarplot(asia.staph, "antimicrobial_compound", "percent_resistant", 
                                  fill = "farm_type", position = position_dodge(0.7),
                                  subtitle = "Asia, Staphylococcus = 162",
                                  xlab = FALSE, ylab = FALSE,
                                  legend.title = "Farm type",
                                  font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.5))

  

asia.Staphylococcus.plot


##Europe Campylobacter bar plot

europe.campy <- data3 %>%
  filter(continent == "Europe", pathogen == "Campylobacter")

europe.campy<- asia.staph[order(-asia.staph$percent_resistant), ]

europe.Campylobacter.plot <- ggbarplot(europe.campy, "antimicrobial_compound", "percent_resistant", 
                                      fill = "farm_type", position = position_dodge(0.7),
                                      subtitle = "Europe, Campylobacter = 712",
                                      xlab = FALSE, ylab = FALSE,
                                      legend.title = "Farm type",
                                      font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.5))


europe.Campylobacter.plot


##Europe E.coli bar plot

europe.ecoli <- data3 %>%
  filter(continent == "Europe", pathogen == "E.coli")

europe.ecoli<- europe.ecoli[order(-europe.ecoli$percent_resistant), ]

europe.ecoli.plot <- ggbarplot(europe.ecoli, "antimicrobial_compound", "percent_resistant", 
                                     fill = "farm_type", position = position_dodge(0.7),
                                     subtitle = "Europe, E.coli = 5,313",
                                     xlab = FALSE, ylab = FALSE,
                                     legend.title = "Farm type",
                                     font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.5))


europe.ecoli.plot


##Europe Enterococcus bar plot

europe.entero <- data3 %>%
  filter(continent == "Europe", pathogen == "Enterococcus")

europe.entero<- europe.entero[order(-europe.entero$percent_resistant), ]

europe.enterococcus.plot <- ggbarplot(europe.entero, "antimicrobial_compound", "percent_resistant", 
                               fill = "farm_type", position = position_dodge(0.7),
                               subtitle = "Europe, Enterococcus = 184",
                               xlab = FALSE, ylab = FALSE,
                               legend.title = "Farm type",
                               font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.5))


europe.enterococcus.plot

##Europe Listeria bar plot

europe.list <- data3 %>%
  filter(continent == "Europe", pathogen == "Listeria")

europe.list<- europe.list[order(-europe.list$percent_resistant), ]

europe.listeria.plot <- ggbarplot(europe.list, "antimicrobial_compound", "percent_resistant", 
                                      fill = "farm_type", position = position_dodge(0.7),
                                      subtitle = "Europe, Listeria = 71",
                                      xlab = FALSE, ylab = FALSE,
                                      legend.title = "Farm type", 
                                      font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.5))

europe.listeria.plot


##Europe MRSA bar plot

europe.MRSA <- data3 %>%
  filter(continent == "Europe", pathogen == "MRSA")

europe.MRSA<- europe.MRSA[order(-europe.MRSA$percent_resistant), ]

europe.MRSA.plot <- ggbarplot(europe.MRSA, "antimicrobial_compound", "percent_resistant", 
                                  fill = "farm_type", position = position_dodge(0.7),
                                  subtitle = "Europe, MRSA = 273",
                                  xlab = FALSE, ylab = FALSE,
                                  legend.title = "Farm type", 
                                  font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.5))

  

europe.MRSA.plot


##Europe S.aureus bar plot

europe.saureus <- data3 %>%
  filter(continent == "Europe", pathogen == "S.aureus")

europe.saureus<- europe.saureus[order(-europe.saureus$percent_resistant), ]

europe.s.aureus.plot <- ggbarplot(europe.saureus, "antimicrobial_compound", "percent_resistant", 
                              fill = "farm_type", position = position_dodge(0.7),
                              subtitle = "Europe, S.aureus = 688",
                              xlab = FALSE, ylab = FALSE,
                              legend.title = "Farm type",
                              font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.5))


europe.s.aureus.plot


##North America, campylobacter bar plot

N.A.campylobacter <- data3 %>%
  filter(continent == "North America", pathogen == "Campylobacter")

N.A.campylobacter<- N.A.campylobacter[order(-N.A.campylobacter$percent_resistant), ]

N.A.campylobacter.plot <- ggbarplot(N.A.campylobacter, "antimicrobial_compound", "percent_resistant", 
                                  fill = "farm_type", position = position_dodge(0.7),
                                  subtitle = "North America, Campylobacter = 9,219",
                                  xlab = FALSE, ylab = FALSE,
                                  legend.title = "Farm type", 
                                  font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.5))


N.A.campylobacter.plot

##North America, E.coli bar plot

N.A.ecoli <- data3 %>%
  filter(continent == "North America", pathogen == "E.coli")

N.A.ecoli<- N.A.ecoli[order(-N.A.ecoli$percent_resistant), ]

N.A.ecoli.plot <- ggbarplot(N.A.ecoli, "antimicrobial_compound", "percent_resistant", 
                                    fill = "farm_type", position = position_dodge(0.7),
                                    subtitle = "North America, E.coli = 12,509",
                                    xlab = FALSE, ylab = FALSE,
                                    legend.title = "Farm type",
                                    font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.5))


N.A.ecoli.plot


##North America, Enterococcus bar plot

N.A.entero <- data3 %>%
  filter(continent == "North America", pathogen == "Enterococcus")

N.A.entero<- N.A.entero[order(-N.A.entero$percent_resistant), ]


N.A.entero.plot <- ggbarplot(N.A.entero, "antimicrobial_compound", "percent_resistant", 
                            fill = "farm_type", position = position_dodge(0.7),
                            subtitle = "North America, Enterococcus = 546",
                            xlab = FALSE, ylab = FALSE,
                            legend.title = "Farm type", 
                            font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.5))

N.A.entero.plot



##North America, S.aureus bar plot

N.A.saureus <- data3 %>%
  filter(continent == "North America", pathogen == "S.aureus")

N.A.saureus<- N.A.saureus[order(-N.A.saureus$percent_resistant), ]

N.A.saureus.plot <- ggbarplot(N.A.saureus, "antimicrobial_compound", "percent_resistant", 
                             fill = "farm_type", position = position_dodge(0.7),
                             subtitle = "North America, S.aureus = 486",
                             xlab = FALSE, ylab = FALSE,
                             legend.title = "Farm type", 
                             font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.5))

N.A.saureus.plot



##North America, Salmonella bar plot

N.A.sal <- data3 %>%
  filter(continent == "North America", pathogen == "Salmonella")

N.A.sal <- N.A.sal [order(-N.A.sal$percent_resistant), ]

N.A.salmonella.plot <- ggbarplot(N.A.sal, "antimicrobial_compound", "percent_resistant", 
                              fill = "farm_type", position = position_dodge(0.7),
                              subtitle = "North America, Salmonella = 3,111",
                              xlab = FALSE, ylab = FALSE,
                              legend.title = "Farm type", 
                              font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.5))


N.A.salmonella.plot



##North America, Staphylococcus bar plot

N.A.staphy <- data3 %>%
  filter(continent == "North America", pathogen == "Staphylococcus")

N.A.staphy<- N.A.staphy[order(-N.A.staphy$percent_resistant), ]

N.A.Staphylococcus.plot <- ggbarplot(N.A.staphy, "antimicrobial_compound", "percent_resistant", 
                                 fill = "farm_type", position = position_dodge(0.7),
                                 subtitle = "North America, Staphylococcus = 405",
                                 xlab = FALSE, ylab = FALSE,
                                 legend.title = "Farm type", 
                                 font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.5))

N.A.Staphylococcus.plot



##Oceania, E.coli bar plot

oceania.ecoli <- data3 %>%
  filter(continent == "Oceania", pathogen == "E.coli")

oceania.ecoli<- oceania.ecoli[order(-oceania.ecoli$percent_resistant), ]

oceania.ecoli.plot <- ggbarplot(oceania.ecoli, "antimicrobial_compound", "percent_resistant", 
                                     fill = "farm_type", position = position_dodge(0.7),
                                     subtitle = "Oceania, E.coli = 1,189",
                                     xlab = FALSE, ylab = FALSE,
                                     legend.title = "Farm type", 
                                     font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.5))

oceania.ecoli.plot




##Oceania, Enterococcus bar plot

oceania.entero <- data3 %>%
  filter(continent == "Oceania", pathogen == "Enterococcus")

oceania.entero<- oceania.entero[order(-oceania.entero$percent_resistant), ]

oceania.enterococcus.plot <- ggbarplot(oceania.entero, "antimicrobial_compound", "percent_resistant", 
                                fill = "farm_type", position = position_dodge(0.7),
                                subtitle = "Oceania, Enterococcus = 353",
                                xlab = FALSE, ylab = FALSE,
                                legend.title = "Farm type",
                                font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.5))


oceania.enterococcus.plot


##South America, E.coli bar plot

S.A.ecoli <- data3 %>%
  filter(continent == "South America", pathogen == "E.coli")

S.A.ecoli<- S.A.ecoli[order(-S.A.ecoli$percent_resistant), ]

S.A.ecoli.plot <- ggbarplot(S.A.ecoli, "antimicrobial_compound", "percent_resistant", 
                                       fill = "farm_type", position = position_dodge(0.7),
                                       subtitle = "South America, E.coli = 156",
                                       xlab = FALSE, ylab = FALSE,
                                       legend.title = "Farm type", 
                                       font.legend = "bold", font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.5))

  

S.A.ecoli.plot



##Putting all plots together using patchwork

continent_plot1 <- (asia.ecoli.plot | europe.ecoli.plot) /  (N.A.ecoli.plot |
                     oceania.ecoli.plot) / (S.A.ecoli.plot | asia.salmonella.plot) /
  (N.A.salmonella.plot | europe.Campylobacter.plot) / (N.A.campylobacter.plot | 
     asia.entero.plot) +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")")+
  plot_layout(guides = "collect", widths = c(2, 2))&
  theme(plot.tag = element_text(face = "bold"),
        legend.position = "top")




continent_plot2 <- (europe.enterococcus.plot | N.A.entero.plot) /
  (oceania.enterococcus.plot | asia.Staphylococcus.plot) / 
  (N.A.Staphylococcus.plot | asia.listeria.plot) / (europe.listeria.plot | europe.MRSA.plot) /
  (europe.s.aureus.plot | N.A.saureus.plot)+ 
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")")+
  plot_layout(guides = "collect")&
  theme(plot.tag = element_text(face = "bold"),
        legend.position = "top")



