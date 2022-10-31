
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



data3 <- aggregate(percent_resistant~antimicrobial_compound+doi+pathogen+host+continent+
                     farm_type+no_isolates,
                   data2, mean)



data3 <- data3 %>%select("continent",
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
EcoliNIsolates <- aggregate(no_isolates ~ doi + continent + farm_type + host+
                              antimicrobial_compound + pathogen + percent_resistant+
                              ResIsoEcoli, data = AMR.tmp.ecoli, FUN = unique)


EcoliNIsolates$no_isolates <- sapply(EcoliNIsolates$no_isolates, function(x) sum(unlist(x)))

#aggregate all resistant isolates and all NIsolates by speciesag, compound and region
ecolires <- aggregate(ResIsoEcoli ~ antimicrobial_compound  + farm_type+doi+host+
                        continent, 
                      data = AMR.tmp.ecoli, FUN = sum)
ecoliAll <- aggregate(no_isolates ~ antimicrobial_compound  + farm_type+host+farm_type+
                        continent, 
                      data = AMR.tmp.ecoli, FUN = sum)
#and divide ResIso/NIsolates to return true mean
EcoliMean <- round((ecolires$ResIsoEcoli /ecoliAll$no_isolates)*100, digits = 0)
EcoliMeanDF <- as.data.frame(cbind(ecolires$farm_type,
                                   ecoliAll$host,
                                   
                                   EcoliMean, ecoliAll$no_isolates))
colnames(EcoliMeanDF) <- c("antimicrobial_compound","farm_type","continent",
                           "Mean","no_isolates")
EcoliMeanDF$Mean <- as.numeric(as.character(EcoliMeanDF$Mean))
EcoliMeanDF$no_isolates <- as.numeric(as.character(EcoliMeanDF$no_isolates))
#restrict display drug pairings where NIsolates > 10
EcoliMeanDF = EcoliMeanDF[which(EcoliMeanDF$no_isolates >= 10),]



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
                               ResIsoEntero, data = AMR.tmp.entero, FUN = unique)


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
                                 ResIsoListeria, data = AMR.tmp.listeria, FUN = unique)


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
                            ResIsoSal, data = AMR.tmp.sal, FUN = unique)


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
                              ResIsoCampy, data = AMR.tmp.campy, FUN = unique)


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
                             ResIsoMrsa, data = AMR.tmp.mrsa, FUN = unique)


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
                                ResIsoSaureus, data = AMR.tmp.saureus, FUN = unique)


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
                             ResIsoMrsa, data = AMR.tmp.mrsa, FUN = unique)


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