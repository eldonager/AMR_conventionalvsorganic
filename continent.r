library(tidyverse)
library(ggplot2)
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
         "no_isolates_resistant",
         "no_isolates_susceptible",
         "no_isolates_intermidiate",
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
    "no_isolates_resistant",
    "no_isolates_susceptible",
    "no_isolates_intermidiate",
         "no_isolates" )



data3 <- data2 %>%select("continent",
                        "doi",
                         "farm_type",
                        "host",
                         "antimicrobial_compound",
                        "pathogen",
                         "percent_resistant", "no_isolates")%>%
filter(pathogen != "Arcanobacterium pyogenes", pathogen != "Corynebacterium bovis",
           pathogen != "Environmental streptococci")
 

#changing percentage resistance into numeric class and rounding off  
data3$percent_resistant <- as.numeric(as.character
                                      (data3$percent_resistant))

data3$percent_resistant <- round(data3$percent_resistant, digits = 0)
#####################################################################################

ecoli<- subset(data3, pathogen == "E.coli")
#create a function to return the number of resistant isolates
new.function1 <- function(x,y) {
  (x*y)/100}

ResIsoEcoli <- mapply(new.function1, ecoli$percent_resistant, ecoli$no_isolates)
ResIsoEcoli <- round(ResIsoEcoli, digits = 0)
AMR.tmp.ecoli <- cbind(ecoli, ResIsoEcoli)

#determine total NIsolates
EcoliNIsolates <- aggregate(no_isolates ~ doi + continent + farm_type + 
                             antimicrobial_compound + pathogen + percent_resistant+
                             ResIsoEcoli, data = AMR.tmp.ecoli, FUN = sum)


EcoliNIsolates$no_isolates <- sapply(EcoliNIsolates$no_isolates, function(x) sum(unlist(x)))

#aggregate all resistant isolates and all NIsolates by speciesag, compound and region
ecolires <- aggregate(ResIsoEcoli ~ antimicrobial_compound  + farm_type+
                       continent, 
                     data = AMR.tmp.ecoli, FUN = sum)
ecoliAll <- aggregate(no_isolates ~ antimicrobial_compound  + farm_type+
                       continent, 
                     data = AMR.tmp.ecoli, FUN = sum)
#and divide ResIso/NIsolates to return true mean
EcoliMean <- round((ecolires$ResIsoEcoli /ecoliAll$no_isolates)*100, digits = 0)
EcoliMeanDF <- as.data.frame(cbind(ecolires$antimicrobial_compound,
                                  ecolires$farm_type, ecolires$continent,
                                  EcoliMean, ecoliAll$no_isolates))
colnames(EcoliMeanDF) <- c("antimicrobial_compound","farm_type","continent",
                           "Mean","no_isolates")
EcoliMeanDF$Mean <- as.numeric(as.character(EcoliMeanDF$Mean))
EcoliMeanDF$no_isolates <- as.numeric(as.character(EcoliMeanDF$no_isolates))
#restrict display drug pairings where NIsolates > 10
EcoliMeanDF = EcoliMeanDF[which(EcoliMeanDF$no_isolates >= 10),]

# Compute the 95% CI of proportion where x = p_hat and y = n two tailed z = 1.96
CI.function <- function(x,y) {
  x + c(-1.96,1.96)*sqrt(x*(1-x)/y)}

#95% CI
CIecoli<- as.data.frame(t(mapply(CI.function, EcoliMeanDF$Mean/100, EcoliMeanDF$no_isolates)))
EcoliMeanDF <-cbind(EcoliMeanDF, round(CIecoli*100, digits = 0))
colnames(EcoliMeanDF) <- c("antimicrobial_compound", "farm_type","continent",
                         "Mean","no_isolates","CILow","CIHigh")
EcoliMeanDF$CILow[EcoliMeanDF$CILow < 0] = 0
EcoliMeanDF$CIHigh[EcoliMeanDF$CIHigh >100] = 100

##remove data with less than 1% resistance
EcoliMeanDF <- EcoliMeanDF%>%
  filter(Mean>1)




  
##Ecoli bars  

##Europe
europe.ecoli <- EcoliMeanDF %>%
  filter(continent == "Europe")

europe.ecoli<- europe.ecoli[order(-europe.ecoli$Mean), ]

europe.ecoli.plot <- ggbarplot(europe.ecoli, "antimicrobial_compound", "Mean", 
                               fill = "farm_type", position = position_dodge(0.7),
                               subtitle = "Europe, E.coli n= 18,910",
                               xlab = FALSE, ylab = FALSE,
                               legend.title = "Farm type",
                                font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.7))





#Asia 
asia.ecoli <- EcoliMeanDF %>%
  filter(continent == "Asia")

##Ordering bar graphs in descending order
asia.ecoli<- asia.ecoli[order(-asia.ecoli$Mean), ]

asia.ecoli.plot <- ggbarplot(asia.ecoli, "antimicrobial_compound", "Mean", 
                             fill = "farm_type", position = position_dodge(0.7),
                             subtitle = "Asia, E.coli n= 970",
                             xlab = FALSE, ylab = FALSE,
                             legend.title = "Farm type",
                             font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.7))




##North America

N.A.ecoli <- EcoliMeanDF %>%
  filter(continent == "North America")

N.A.ecoli<- N.A.ecoli[order(-N.A.ecoli$Mean), ]

N.A.ecoli.plot <- ggbarplot(N.A.ecoli, "antimicrobial_compound", "Mean", 
                            fill = "farm_type", position = position_dodge(0.7),
                            subtitle = "North America, E.coli n= 27,381",
                            xlab = FALSE, ylab = FALSE,
                            legend.title = "Farm type",
                             font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.7))





#Oceania

oceania.ecoli <- EcoliMeanDF %>%
  filter(continent == "Oceania")

oceania.ecoli<- oceania.ecoli[order(-oceania.ecoli$Mean), ]

oceania.ecoli.plot <- ggbarplot(oceania.ecoli, "antimicrobial_compound", "Mean", 
                                fill = "farm_type", position = position_dodge(0.7),
                                subtitle = "Oceania, E.coli n= 4,756",
                                xlab = FALSE, ylab = FALSE,
                                legend.title = "Farm type", 
                               font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.7))



#South america

S.A.ecoli <- EcoliMeanDF %>%
  filter(continent == "South America")

S.A.ecoli<- S.A.ecoli[order(-S.A.ecoli$Mean), ]

S.A.ecoli.plot <- ggbarplot(S.A.ecoli, "antimicrobial_compound", "Mean", 
                            fill = "farm_type", position = position_dodge(0.7),
                            subtitle = "South America, E.coli n= 2340",
                            xlab = FALSE, ylab = FALSE,
                            legend.title = "Farm type", 
                             font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.7))







##Enterococcus data

entero<- subset(data3, pathogen == "Enterococcus")
#create a function to return the number of resistant isolates
new.function1 <- function(x,y) {
  (x*y)/100}

ResIsoEntero <- mapply(new.function1, entero$percent_resistant, entero$no_isolates)
ResIsoEntero <- round(ResIsoEntero, digits = 0)
AMR.tmp.entero <- cbind(entero, ResIsoEntero)

#determine total NIsolates
EnteroNIsolates <- aggregate(no_isolates ~ doi + continent + farm_type + 
                              antimicrobial_compound + pathogen + percent_resistant+
                              ResIsoEntero, data = AMR.tmp.entero, FUN = sum)


EnteroNIsolates$no_isolates <- sapply(EnteroNIsolates$no_isolates, function(x) sum(unlist(x)))

#aggregate all resistant isolates and all NIsolates by speciesag, compound and region
enterores <- aggregate(ResIsoEntero ~ antimicrobial_compound  + farm_type+
                        continent, 
                      data = AMR.tmp.entero, FUN = sum)
enteroAll <- aggregate(no_isolates ~ antimicrobial_compound  + farm_type+
                        continent, 
                      data = AMR.tmp.entero, FUN = sum)
#and divide ResIso/NIsolates to return true mean
EnteroMean <- round((enterores$ResIsoEntero /enteroAll$no_isolates)*100, digits = 0)
EnteroMeanDF <- as.data.frame(cbind(enterores$antimicrobial_compound,
                                   enterores$farm_type, enterores$continent,
                                   EnteroMean, enteroAll$no_isolates))
colnames(EnteroMeanDF) <- c("antimicrobial_compound","farm_type","continent",
                           "Mean","no_isolates")
EnteroMeanDF$Mean <- as.numeric(as.character(EnteroMeanDF$Mean))
EnteroMeanDF$no_isolates <- as.numeric(as.character(EnteroMeanDF$no_isolates))

#restrict display drug pairings where NIsolates > 10
EnteroMeanDF = EnteroMeanDF[which(EnteroMeanDF$no_isolates >= 10),]

# Compute the 95% CI of proportion where x = p_hat and y = n two tailed z = 1.96
CI.function <- function(x,y) {
  x + c(-1.96,1.96)*sqrt(x*(1-x)/y)}

#95% CI
CIentero<- as.data.frame(t(mapply(CI.function, EnteroMeanDF$Mean/100,
                                  EnteroMeanDF$no_isolates)))
EnteroMeanDF <-cbind(EnteroMeanDF, round(CIentero*100, digits = 0))

colnames(EnteroMeanDF) <- c("antimicrobial_compound", "farm_type","continent",
                           "Mean","no_isolates","CILow","CIHigh")

EnteroMeanDF$CILow[EnteroMeanDF$CILow < 0] = 0
EnteroMeanDF$CIHigh[EnteroMeanDF$CIHigh >100] = 100

##remove data with less than 1% resistance
EnteroMeanDF <- EnteroMeanDF%>%
  filter(Mean>1)

###Bar plots

##Asia enterococcus bar plot

asia.entero <- EnteroMeanDF %>%
  filter(continent == "Asia")

asia.entero<- asia.entero[order(-asia.entero$Mean), ]

asia.entero.plot <- ggbarplot(asia.entero, "antimicrobial_compound", "Mean", 
                              fill = "farm_type", 
                              position = position_dodge(0.7),
                              subtitle = "Asia, Enterococcus n= 468",
                              xlab = FALSE, ylab = FALSE,
                              legend.title = "Farm type",
                             font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.7))





##Europe Enterococcus bar plot

europe.entero <- EnteroMeanDF %>%
  filter(continent == "Europe")

europe.entero<- europe.entero[order(-europe.entero$Mean), ]

europe.enterococcus.plot <- ggbarplot(europe.entero, "antimicrobial_compound",
                                      "Mean", 
                                      fill = "farm_type", position = position_dodge(0.7),
                                      subtitle = "Europe, Enterococcus n= 4,196",
                                      xlab = FALSE, ylab = FALSE,
                                      legend.title = "Farm type",
                                       font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.7))






##North America, Enterococcus bar plot

N.A.entero <- EnteroMeanDF %>%
  filter(continent == "North America")

N.A.entero<- N.A.entero[order(-N.A.entero$Mean), ]


N.A.entero.plot <- ggbarplot(N.A.entero, "antimicrobial_compound", "Mean", 
                             fill = "farm_type", position = position_dodge(0.7),
                             subtitle = "North America, Enterococcus n= 4,746",
                             xlab = FALSE, ylab = FALSE,
                             legend.title = "Farm type", 
                            font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.7))






##Oceania, Enterococcus bar plot

oceania.entero <- EnteroMeanDF %>%
  filter(continent == "Oceania",)

oceania.entero<- oceania.entero[order(-oceania.entero$Mean), ]

oceania.enterococcus.plot <- ggbarplot(oceania.entero, "antimicrobial_compound",
                                       "Mean", 
                                       fill = "farm_type", position = position_dodge(0.7),
                                       subtitle = "Oceania, Enterococcus n= 1765",
                                       xlab = FALSE, ylab = FALSE,
                                       legend.title = "Farm type",
                                       font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.7))







##Listeria data

listeria<- subset(data3, pathogen == "Listeria")
#create a function to return the number of resistant isolates
new.function1 <- function(x,y) {
  (x*y)/100}

ResIsoListeria <- mapply(new.function1,listeria$percent_resistant,
                         listeria$no_isolates)

ResIsoListeria <- round(ResIsoListeria, digits = 0)
AMR.tmp.listeria <- cbind(listeria, ResIsoListeria)

#determine total NIsolates
ListeriaNIsolates <- aggregate(no_isolates ~ doi + continent + farm_type + 
                              antimicrobial_compound + pathogen + percent_resistant+
                              ResIsoListeria, data = AMR.tmp.listeria, FUN = sum)


ListeriaNIsolates$no_isolates <- sapply(ListeriaNIsolates$no_isolates, function(x) sum(unlist(x)))

#aggregate all resistant isolates and all NIsolates by  compound and region
listeriares <- aggregate(ResIsoListeria ~ antimicrobial_compound  + farm_type+
                        continent, 
                      data = AMR.tmp.listeria, FUN = sum)

listeriaAll <- aggregate(no_isolates ~ antimicrobial_compound  + farm_type+
                        continent, 
                      data = AMR.tmp.listeria, FUN = sum)
#and divide ResIso/NIsolates to return true mean
ListeriaMean <- round((listeriares$ResIsoListeria /listeriaAll$no_isolates)*100, digits = 0)
ListeriaMeanDF <- as.data.frame(cbind(listeriares$antimicrobial_compound,
                                   listeriares$farm_type, listeriares$continent,
                                   ListeriaMean, listeriaAll$no_isolates))

colnames(ListeriaMeanDF) <- c("antimicrobial_compound","farm_type","continent",
                           "Mean","no_isolates")

ListeriaMeanDF$Mean <- as.numeric(as.character(ListeriaMeanDF$Mean))
ListeriaMeanDF$no_isolates <- as.numeric(as.character(ListeriaMeanDF$no_isolates))

#restrict display drug pairings where NIsolates > 10
ListeriaMeanDF = ListeriaMeanDF[which(ListeriaMeanDF$no_isolates >= 10),]

# Compute the 95% CI of proportion where x = p_hat and y = n two tailed z = 1.96
CI.function <- function(x,y) {
  x + c(-1.96,1.96)*sqrt(x*(1-x)/y)}

#95% CI
CIlisteria<- as.data.frame(t(mapply(CI.function, ListeriaMeanDF$Mean/100,
                                    ListeriaMeanDF$no_isolates)))
ListeriaMeanDF <-cbind(ListeriaMeanDF, round(CIlisteria*100, digits = 0))
colnames(ListeriaMeanDF) <- c("antimicrobial_compound", "farm_type","continent",
                           "Mean","no_isolates","CILow","CIHigh")
ListeriaMeanDF$CILow[ListeriaMeanDF$CILow < 0] = 0
ListeriaMeanDF$CIHigh[ListeriaMeanDF$CIHigh >100] = 100

##remove data with less than 1% resistance
ListeriaMeanDF <- ListeriaMeanDF%>%
  filter(Mean>1)


#Listeria plots


##Asia listeria bar plot

asia.list <- ListeriaMeanDF %>%
  filter(continent == "Asia")

asia.list<- asia.list[order(-asia.list$Mean), ]

asia.listeria.plot <- ggbarplot(asia.list, "antimicrobial_compound",
                                "Mean", 
                                fill = "farm_type", position = position_dodge(0.7),
                                subtitle = "Asia, Listeria n= 480",
                                xlab = FALSE, ylab = FALSE,
                                legend.title = "Farm type",
                                font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.7))






##Europe Listeria bar plot

europe.list <- ListeriaMeanDF %>%
  filter(continent == "Europe")

europe.list<- europe.list[order(-europe.list$Mean), ]

europe.listeria.plot <- ggbarplot(europe.list, "antimicrobial_compound", 
                                  "Mean", 
                                  fill = "farm_type", position = position_dodge(0.7),
                                  subtitle = "Europe, Listeria n= 32",
                                  xlab = FALSE, ylab = FALSE,
                                  legend.title = "Farm type", 
                                   font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.7))




##Salmonella


sal<- subset(data3, pathogen == "Salmonella")
#create a function to return the number of resistant isolates
new.function1 <- function(x,y) {
  (x*y)/100}

ResIsoSal <- mapply(new.function1, sal$percent_resistant, sal$no_isolates)
ResIsoSal  <- round(ResIsoSal , digits = 0)
AMR.tmp.sal <- cbind(sal, ResIsoSal)

#determine total NIsolates
SalNIsolates <- aggregate(no_isolates ~ doi + continent + farm_type + 
                              antimicrobial_compound + pathogen + percent_resistant+
                            ResIsoSal, data = AMR.tmp.sal, FUN = sum)


SalNIsolates$no_isolates <- sapply(SalNIsolates$no_isolates, function(x) sum(unlist(x)))

#aggregate all resistant isolates and all NIsolates by speciesag, compound and region
salres <- aggregate(ResIsoSal ~ antimicrobial_compound  + farm_type+
                        continent, 
                      data = AMR.tmp.sal, FUN = sum)
salAll <- aggregate(no_isolates ~ antimicrobial_compound  + farm_type+
                        continent, 
                      data = AMR.tmp.sal, FUN = sum)
#and divide ResIso/NIsolates to return true mean
SalMean <- round((salres$ResIsoSal /salAll$no_isolates)*100, digits = 0)

SalMeanDF <- as.data.frame(cbind(salres$antimicrobial_compound,
                                   salres$farm_type, salres$continent,
                                   SalMean, salAll$no_isolates))
colnames(SalMeanDF) <- c("antimicrobial_compound","farm_type","continent",
                           "Mean","no_isolates")


SalMeanDF$Mean <- as.numeric(as.character(SalMeanDF$Mean))
SalMeanDF$no_isolates <- as.numeric(as.character(SalMeanDF$no_isolates))
#restrict display drug pairings where NIsolates > 10
SalMeanDF = SalMeanDF[which(SalMeanDF$no_isolates >= 10),]

# Compute the 95% CI of proportion where x = p_hat and y = n two tailed z = 1.96
CI.function <- function(x,y) {
  x + c(-1.96,1.96)*sqrt(x*(1-x)/y)}

#95% CI
CIsal<- as.data.frame(t(mapply(CI.function, SalMeanDF$Mean/100, 
                               SalMeanDF$no_isolates)))
SalMeanDF <-cbind(SalMeanDF, round(CIsal*100, digits = 0))

colnames(SalMeanDF) <- c("antimicrobial_compound", "farm_type","continent",
                           "Mean","no_isolates","CILow","CIHigh")

SalMeanDF$CILow[SalMeanDF$CILow < 0] = 0
SalMeanDF$CIHigh[SalMeanDF$CIHigh >100] = 100

##remove data with less than 1% resistance
SalMeanDF <- SalMeanDF%>%
  filter(Mean>1)



##Asia Salmonella bar plot

asia.sal <- SalMeanDF %>%
  filter(continent == "Asia")

asia.sal<- asia.sal[order(-asia.sal$Mean), ]

asia.salmonella.plot <- ggbarplot(asia.sal, "antimicrobial_compound",
                                  "Mean", 
                                  fill = "farm_type", position = position_dodge(0.7),
                                  subtitle = "Asia, Salmonella n= 5,924",
                                  xlab = FALSE, ylab = FALSE,
                                  legend.title = "Farm type",
                                 font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.7))






##North America, Salmonella bar plot

N.A.sal <- SalMeanDF %>%
  filter(continent == "North America")

N.A.sal <- N.A.sal [order(-N.A.sal$Mean), ]

N.A.salmonella.plot <- ggbarplot(N.A.sal, "antimicrobial_compound", 
                                 "Mean", 
                                 fill = "farm_type", position = position_dodge(0.7),
                                 subtitle = "North America, Salmonella n= 22,696",
                                 xlab = FALSE, ylab = FALSE,
                                 legend.title = "Farm type", 
                                 font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.7))






##Staphylocccus data


staph<- subset(data3, pathogen == "Staphylococcus")
#create a function to return the number of resistant isolates
new.function1 <- function(x,y) {
  (x*y)/100}

ResIsoStaphi <- mapply(new.function1, staph$percent_resistant, staph$no_isolates)
ResIsoStaphi <- round(ResIsoStaphi, digits = 0)
AMR.tmp.staph <- cbind(staph, ResIsoStaphi)

#determine total NIsolates
StaphNIsolates <- aggregate(no_isolates ~ doi + continent + farm_type + 
                              antimicrobial_compound + pathogen + percent_resistant+
                              ResIsoStaphi, data = AMR.tmp.staph, FUN = unique)


StaphNIsolates$no_isolates <- sapply(StaphNIsolates$no_isolates, function(x) sum(unlist(x)))

#aggregate all resistant isolates and all NIsolates by host compound and region
staphres <- aggregate(ResIsoStaphi ~ antimicrobial_compound  + farm_type+
                        continent, 
                      data = AMR.tmp.staph, FUN = sum)
staphAll <- aggregate(no_isolates ~ antimicrobial_compound  + farm_type+
                        continent, 
                      data = AMR.tmp.staph, FUN = sum)
#and divide ResIso/NIsolates to return true mean
StaphMean <- round((staphres$ResIsoStaphi /staphAll$no_isolates)*100, digits = 0)

StaphMeanDF <- as.data.frame(cbind(staphres$antimicrobial_compound,
                                  staphres$farm_type, staphres$continent,
                                   StaphMean, staphAll$no_isolates))

colnames(StaphMeanDF) <- c("antimicrobial_compound","farm_type","continent",
                           "Mean","no_isolates")
StaphMeanDF$Mean <- as.numeric(as.character(StaphMeanDF$Mean))
StaphMeanDF$no_isolates <- as.numeric(as.character(StaphMeanDF$no_isolates))


#restrict display drug pairings where NIsolates > 10
StaphMeanDF <- StaphMeanDF[which(StaphMeanDF$no_isolates >= 10),]

# Compute the 95% CI of proportion where x = p_hat and y = n two tailed z = 1.96
CI.function <- function(x,y) {
  x + c(-1.96,1.96)*sqrt(x*(1-x)/y)}

#95% CI
CIstaph<- as.data.frame(t(mapply(CI.function, StaphMeanDF$Mean/100, StaphMeanDF$no_isolates)))
StaphMeanDF <-cbind(StaphMeanDF, round(CIstaph*100, digits = 0))
colnames(StaphMeanDF) <- c("antimicrobial_compound", "farm_type","continent",
                           "Mean","no_isolates","CILow","CIHigh")
StaphMeanDF$CILow[StaphMeanDF$CILow < 0] = 0
StaphMeanDF$CIHigh[StaphMeanDF$CIHigh >100] = 100

##remove data with less than 1% resistance
StaphMeanDF <- StaphMeanDF%>%
  filter(Mean>1)


##Bar plots

##Asia Staphylococcus bar plot

asia.staph <- StaphMeanDF %>%
  filter(continent == "Asia")

asia.staph<- asia.staph[order(-asia.staph$Mean), ]


asia.Staphylococcus.plot <- ggbarplot(asia.staph, "antimicrobial_compound", "Mean", 
                                      fill = "farm_type", position = position_dodge(0.7),
                                      subtitle = "Asia, Staphylococcus n= 972",
                                      xlab = FALSE, ylab = FALSE,
                                      legend.title = "Farm type",
                                     font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.7))








##North America, Staphylococcus bar plot

N.A.staphy <- StaphMeanDF %>%
  filter(continent == "North America")

N.A.staphy<- N.A.staphy[order(-N.A.staphy$Mean), ]

N.A.Staphylococcus.plot <- ggbarplot(N.A.staphy, "antimicrobial_compound", "Mean", 
                                     fill = "farm_type", position = position_dodge(0.7),
                                     subtitle = "North America, Staphylococcus n= 405",
                                     xlab = FALSE, ylab = FALSE,
                                     legend.title = "Farm type", 
                                    font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.7))





##Campylobacter


campy<- subset(data3, pathogen == "Campylobacter")
#create a function to return the number of resistant isolates
new.function1 <- function(x,y) {
  (x*y)/100}

ResIsoCampy <- mapply(new.function1, campy$percent_resistant, campy$no_isolates)
ResIsoCampy  <- round(ResIsoCampy , digits = 0)
AMR.tmp.campy <- cbind(campy, ResIsoCampy)

#determine total NIsolates
CampyNIsolates <- aggregate(no_isolates ~ doi + continent + farm_type + 
                            antimicrobial_compound + pathogen + percent_resistant+
                            ResIsoCampy, data = AMR.tmp.campy, FUN = sum)


CampyNIsolates$no_isolates <- sapply(CampyNIsolates$no_isolates, function(x) sum(unlist(x)))

#aggregate all resistant isolates and all NIsolates by speciesag, compound and region
campyres <- aggregate(ResIsoCampy ~ antimicrobial_compound  + farm_type+
                      continent, 
                    data = AMR.tmp.campy, FUN = sum)
campyAll <- aggregate(no_isolates ~ antimicrobial_compound  + farm_type+
                      continent, 
                    data = AMR.tmp.campy, FUN = sum)
#and divide ResIso/NIsolates to return true mean
CampyMean <- round((campyres$ResIsoCampy /campyAll$no_isolates)*100, digits = 0)

CampyMeanDF <- as.data.frame(cbind(campyres$antimicrobial_compound,
                                campyres$farm_type, campyres$continent,
                                 CampyMean, campyAll$no_isolates))
colnames(CampyMeanDF) <- c("antimicrobial_compound","farm_type","continent",
                         "Mean","no_isolates")


CampyMeanDF$Mean <- as.numeric(as.character(CampyMeanDF$Mean))
CampyMeanDF$no_isolates <- as.numeric(as.character(CampyMeanDF$no_isolates))
#restrict display drug pairings where NIsolates > 10
CampyMeanDF = CampyMeanDF[which(CampyMeanDF$no_isolates >= 10),]

# Compute the 95% CI of proportion where x = p_hat and y = n two tailed z = 1.96
CI.function <- function(x,y) {
  x + c(-1.96,1.96)*sqrt(x*(1-x)/y)}

#95% CI
CIcampy<- as.data.frame(t(mapply(CI.function, CampyMeanDF$Mean/100, 
                               CampyMeanDF$no_isolates)))
CampyMeanDF <-cbind(CampyMeanDF, round(CIcampy*100, digits = 0))

colnames(CampyMeanDF) <- c("antimicrobial_compound", "farm_type","continent",
                         "Mean","no_isolates","CILow","CIHigh")

CampyMeanDF$CILow[CampyMeanDF$CILow < 0] = 0
CampyMeanDF$CIHigh[CampyMeanDF$CIHigh >100] = 100

##remove data with less than 1% resistance
CampyMeanDF <- CampyMeanDF%>%
  filter(Mean>1)



##Europe Campylobacter bar plot

europe.campy <- CampyMeanDF %>%
  filter(continent == "Europe")

europe.campy<- europe.campy[order(-europe.campy$Mean), ]

europe.Campylobacter.plot <- ggbarplot(europe.campy, "antimicrobial_compound", "Mean", 
                                       fill = "farm_type", position = position_dodge(0.7),
                                       subtitle = "Europe, Campylobacter n= 4966",
                                       xlab = FALSE, ylab = FALSE,
                                       legend.title = "Farm type",
                                      font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.7))






##North America, campylobacter bar plot

N.A.campylobacter <- CampyMeanDF %>%
  filter(continent == "North America")

N.A.campylobacter<- N.A.campylobacter[order(-N.A.campylobacter$Mean), ]

N.A.campylobacter.plot <- ggbarplot(N.A.campylobacter, "antimicrobial_compound", "Mean", 
                                    fill = "farm_type", position = position_dodge(0.7),
                                    subtitle = "North America, Campylobacter n= 22,586",
                                    xlab = FALSE, ylab = FALSE,
                                    legend.title = "Farm type", 
                                    font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.7))





##Methicillin-resistant staphylococcus aureus - MRSA

mrsa<- subset(data3, pathogen == "MRSA")
#create a function to return the number of resistant isolates
new.function1 <- function(x,y) {
  (x*y)/100}

ResIsoMrsa <- mapply(new.function1, mrsa$percent_resistant, mrsa$no_isolates)
ResIsoMrsa <- round(ResIsoMrsa, digits = 0)
AMR.tmp.mrsa <- cbind(mrsa, ResIsoMrsa)

#determine total NIsolates
MrsaNIsolates <- aggregate(no_isolates ~ doi + continent + farm_type + 
                              antimicrobial_compound + pathogen + percent_resistant+
                              ResIsoMrsa, data = AMR.tmp.mrsa, FUN = sum)


MrsaNIsolates$no_isolates <- sapply(MrsaNIsolates$no_isolates, function(x) sum(unlist(x)))

#aggregate all resistant isolates and all NIsolates by speciesag, compound and region
mrsares <- aggregate(ResIsoMrsa ~ antimicrobial_compound  + farm_type+
                        continent, 
                      data = AMR.tmp.mrsa, FUN = sum)
mrsaAll <- aggregate(no_isolates ~ antimicrobial_compound  + farm_type+
                        continent, 
                      data = AMR.tmp.mrsa, FUN = sum)
#and divide ResIso/NIsolates to return true mean
MrsaMean <- round((mrsares$ResIsoMrsa /mrsaAll$no_isolates)*100, digits = 0)

MrsaMeanDF <- as.data.frame(cbind(mrsares$antimicrobial_compound,
                                   mrsares$farm_type,mrsares$continent,
                                   MrsaMean, mrsaAll$no_isolates))

colnames(MrsaMeanDF) <- c("antimicrobial_compound","farm_type","continent",
                           "Mean","no_isolates")


MrsaMeanDF$Mean <- as.numeric(as.character(MrsaMeanDF$Mean))

MrsaMeanDF$no_isolates <- as.numeric(as.character(MrsaMeanDF$no_isolates))

#restrict display drug pairings where NIsolates > 10
MrsaMeanDF = MrsaMeanDF[which(MrsaMeanDF$no_isolates >= 10),]

# Compute the 95% CI of proportion where x = p_hat and y = n two tailed z = 1.96
CI.function <- function(x,y) {
  x + c(-1.96,1.96)*sqrt(x*(1-x)/y)}

#95% CI

CImrsa<- as.data.frame(t(mapply(CI.function, MrsaMeanDF$Mean/100,
                                    MrsaMeanDF$no_isolates)))

MrsaMeanDF <-cbind(MrsaMeanDF, round(CImrsa*100, digits = 0))
colnames(MrsaMeanDF) <- c("antimicrobial_compound", "farm_type","continent",
                           "Mean","no_isolates","CILow","CIHigh")

MrsaMeanDF$CILow[MrsaMeanDF$CILow < 0] = 0
MrsaMeanDF$CIHigh[MrsaMeanDF$CIHigh >100] = 100

##remove data with less than 1% resistance
MrsaMeanDF <- MrsaMeanDF%>%
  filter(Mean>1)



##Europe MRSA bar plot

europe.MRSA <- MrsaMeanDF %>%
  filter(continent == "Europe")

europe.MRSA<- europe.MRSA[order(-europe.MRSA$Mean), ]

europe.MRSA.plot <- ggbarplot(europe.MRSA, "antimicrobial_compound", 
                              "Mean", 
                              fill = "farm_type", position = position_dodge(0.7),
                              subtitle = "Europe, MRSA n= 2,933",
                              xlab = FALSE, ylab = FALSE,
                              legend.title = "Farm type", 
                              font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.7))






##S.aureus
saureus<- subset(data3, pathogen == "S.aureus")
#create a function to return the number of resistant isolates
new.function1 <- function(x,y) {
  (x*y)/100}

ResIsoSaureus <- mapply(new.function1, saureus$percent_resistant, saureus$no_isolates)
ResIsoSaureus  <- round(ResIsoSaureus , digits = 0)
AMR.tmp.saureus <- cbind(saureus, ResIsoSaureus)

#determine total NIsolates
SaureusNIsolates <- aggregate(no_isolates ~ doi + continent + farm_type + 
                            antimicrobial_compound + pathogen + percent_resistant+
                            ResIsoSaureus, data = AMR.tmp.saureus, FUN = sum)


SaureusNIsolates$no_isolates <- sapply(SaureusNIsolates$no_isolates, function(x) sum(unlist(x)))

#aggregate all resistant isolates and all NIsolates by speciesag, compound and region
saureusres <- aggregate(ResIsoSaureus ~ antimicrobial_compound  + farm_type+
                      continent, 
                    data = AMR.tmp.saureus, FUN = sum)
saureusAll <- aggregate(no_isolates ~ antimicrobial_compound  + farm_type+
                      continent, 
                    data = AMR.tmp.saureus, FUN = sum)

#and divide ResIso/NIsolates to return true mean
SaureusMean <- round((saureusres$ResIsoSaureus /saureusAll$no_isolates)*100, 
                     digits = 0)

SaureusMeanDF <- as.data.frame(cbind(saureusres$antimicrobial_compound,
                                 saureusres$farm_type, saureusres$continent,
                                 SaureusMean, saureusAll$no_isolates))

colnames(SaureusMeanDF) <- c("antimicrobial_compound","farm_type","continent",
                         "Mean","no_isolates")


SaureusMeanDF$Mean <- as.numeric(as.character(SaureusMeanDF$Mean))
SaureusMeanDF$no_isolates <- as.numeric(as.character(SaureusMeanDF$no_isolates))
#restrict display drug pairings where NIsolates > 10
SaureusMeanDF <- SaureusMeanDF[which(SaureusMeanDF$no_isolates >= 10),]

# Compute the 95% CI of proportion where x = p_hat and y = n two tailed z = 1.96
CI.function <- function(x,y) {
  x + c(-1.96,1.96)*sqrt(x*(1-x)/y)}

#95% CI
CIsaureus<- as.data.frame(t(mapply(CI.function, SaureusMeanDF$Mean/100, 
                               SaureusMeanDF$no_isolates)))
SaureusMeanDF <-cbind(SaureusMeanDF, round(CIsaureus*100, digits = 0))

colnames(SaureusMeanDF) <- c("antimicrobial_compound", "farm_type","continent",
                         "Mean","no_isolates","CILow","CIHigh")

SaureusMeanDF$CILow[SaureusMeanDF$CILow < 0] = 0
SaureusMeanDF$CIHigh[SaureusMeanDF$CIHigh >100] = 100

##remove data with less than 1% resistance
SaureusMeanDF <- SaureusMeanDF%>%
  filter(Mean>1)



##Europe S.aureus bar plot

europe.saureus <- SaureusMeanDF %>%
  filter(continent == "Europe")

europe.saureus<- europe.saureus[order(-europe.saureus$Mean), ]

europe.s.aureus.plot <- ggbarplot(europe.saureus, "antimicrobial_compound", 
                                  "Mean", 
                                  fill = "farm_type", position = position_dodge(0.7),
                                  subtitle = "Europe, S.aureus n= 3,950",
                                  xlab = FALSE, ylab = FALSE,
                                  legend.title = "Farm type",
                                 font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.7))





##North America, S.aureus bar plot

N.A.saureus <- SaureusMeanDF %>%
  filter(continent == "North America")

N.A.saureus<- N.A.saureus[order(-N.A.saureus$Mean), ]

N.A.saureus.plot <- ggbarplot(N.A.saureus, "antimicrobial_compound",
                              "Mean", 
                             fill = "farm_type", position = position_dodge(0.7),
                             subtitle = "North America, S.aureus n= 1,521",
                             xlab = FALSE, ylab = FALSE,
                             legend.title = "Farm type", 
                             font.subtitle = "italic")+
  rotate_x_text(90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.7))





##Putting all plots together using patchwork

continent_plot1 <- (asia.ecoli.plot | europe.ecoli.plot | N.A.ecoli.plot | S.A.ecoli.plot) / 
                    (oceania.ecoli.plot | asia.salmonella.plot | N.A.salmonella.plot |europe.Campylobacter.plot)/
                    (N.A.campylobacter.plot | asia.entero.plot | europe.enterococcus.plot | N.A.entero.plot )/
                    (oceania.enterococcus.plot | europe.s.aureus.plot |N.A.saureus.plot)+
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")")+
  plot_layout(guides = "collect")&
  theme(plot.tag = element_text(face = "italic"),
        legend.position = "top")


continent_plot1

ggsave(filename = "continents.png",continent_plot1)


