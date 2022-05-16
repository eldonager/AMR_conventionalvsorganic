####################
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(patchwork)
library(scales)

#Load data
main_data <- read_csv("conv.csv")


data1 <- data1%>%
  mutate(
    continent =
      ifelse(continent == "Oceania", 
             "New Zealand", continent
      )
  )





data1 <- main_data %>%
  group_by(country) %>%
  select("doi",
  "continent",
      
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
  select("doi",
          "continent",
         "farm_type",
         "host",
         "antimicrobial_compound",
         "antimicrobial_class",
         "pathogen",
         "percent_resistant",
         "no_isolates" )

###Ecoli

E.coli <- subset(data2, pathogen == "E.coli")





#determine total no_isolates isolates per study
Ecoli_isolates <- aggregate(no_isolates ~ doi + continent + farm_type +
                              + antimicrobial_compound + pathogen+
                                 no_isolates,
                                  data = E.coli, FUN = unique)


#saggregate all resistant isolates and all no_isolates by host, compound and continent
EcoliRes <- aggregate(ResIsoEcoli ~ antimicrobial_compound  + 
                        pathogen+ farm_type+
                       continent, data = AMR.tmp.Ecoli, FUN = sum)
EcoliAll <- aggregate(no_isolates ~ antimicrobial_compound  + 
                        farm_type+ pathogen + 
                       continent, data = AMR.tmp.Ecoli, FUN = sum)

#and divide ResIso/no_isolates to return true mean
EcoliMean <- round((EcoliRes$ResIsoEcoli /EcoliAll$no_isolates)*100, digits = 0)

EcoliMeanDF <- as.data.frame(cbind(EcoliRes$antimicrobial_compound,
                              EcoliRes$pathogen,
                                  EcoliRes$continent,
                                  EcoliRes$farm_type,
                                  EcoliMean, EcoliAll$no_isolates))

colnames(EcoliMeanDF) <- c("antimicrobial_compound", "pathogen",
                           "continent", 
                           "farm_type",
                           "Mean",
                          "no_isolates")


#Only displaying data with no_isolates > 10

EcoliMeanDF <- EcoliMeanDF %>%
  filter(no_isolates >= 10)

##Changing mean and no_isolates from character to numeric class
EcoliMeanDF$Mean <- as.numeric(as.character
                                      (EcoliMeanDF$Mean))
EcoliMeanDF$no_isolates <- as.numeric(as.character
                               (EcoliMeanDF$no_isolates))


# Compute the 95% CI of proportion where x = p_hat and y = n two tailed z = 1.96
CI.function <- function(x,y) {
  x + c(-1.96,1.96)*sqrt(x*(1-x)/y)}

#95% CI
CIEcoli <- as.data.frame(t(mapply(CI.function, EcoliMeanDF$Mean/100,
                                  EcoliMeanDF$no_isolates)))

EcoliMeanDF <- cbind(EcoliMeanDF, round(CIEcoli*100, digits = 0))

##New col names
colnames(EcoliMeanDF) = c("antimicrobial_compound","pathogen",
                          "continent","farm_type",
                          "Mean","no_isolates","CILow","CIHigh")

##Reducing -1 range to 1 and 100+ to 100.
EcoliMeanDF$CILow[EcoliMeanDF$CILow < 0] = 0
EcoliMeanDF$CIHigh[EcoliMeanDF$CIHigh >100] = 100

#remove drugs where resistance = 0
EcoliMeanDF <- EcoliMeanDF %>%
  filter(Mean>1)


###Ecoli plots per continent

##Asia
Ecoli.asia <- EcoliMeanDF %>%
  filter(continent == "Asia") 



ecoliplots<- ggbarplot(EcoliMeanDF, "antimicrobial_compound", "Mean", 
          fill = "farm_type", 
          position = position_dodge(0.7),
          subtitle = "E.coli",
          legend.title = "Farm type", font.x = "bold", font.y = "bold",
          font.legend = "bold", font.subtitle = "italic")+ rotate_x_text(angle = 90)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                    position = position_dodge(width = 0.5))


p1 <- facet(ecoliplots, facet.by = "continent", nrow = 5)




###########################################################
x =  barplot(EcMatrix[,rev(order(colMeans(EcMatrix[,,1]))),1], beside = T, plot = FALSE)
segments(x,EcMatrixCIHigh[,rev(order(colMeans(EcMatrix[,,1]))),1],x,EcMatrixCILow[,rev(order(colMeans(EcMatrix[,,1]))),1], col = "grey", lwd = .5)
mtext(bquote(bold("Eastern Asia -") ~ italic("Ecoli ") ~ italic("n = ") ~ italic(.(sum(EcNIsolates$NIsolates[which(EcNIsolates$CombRegion == "Eastern Asia")])))), side = 3, cex = .8, line = 0.5)

barplot(EcMatrix.sea[,rev(order(colMeans(EcMatrix.sea)))], beside = T, col = Cols, border = F, ylim = c(0,100),
        cex.names = 1.2, cex.axis = 1.2, las = 2)
x =  barplot(EcMatrix[,rev(order(colMeans(EcMatrix[,,2]))),2], beside = T, plot = FALSE)
segments(x,EcMatrixCIHigh[,rev(order(colMeans(EcMatrix[,,2]))),2],x,EcMatrixCILow[,rev(order(colMeans(EcMatrix[,,2]))),2], col = "grey", lwd = .5)
mtext(bquote(bold("South-eastern Asia -") ~ italic("Ecoli ") ~ italic("n = ") ~ italic(.(sum(EcNIsolates$NIsolates[which(EcNIsolates$CombRegion == "South-eastern Asia")])))), side = 3, cex = .8, line = 0.5)

barplot(EcMatrix.wsa[,rev(order(colMeans(EcMatrix.wsa)))], beside = T, col = Cols, border = F, ylim = c(0,100),
        cex.names = 1.2, cex.axis = 1.2, las = 2)
x =  barplot(EcMatrix[,rev(order(colMeans(EcMatrix[,,3]))),3], beside = T, plot = FALSE)
segments(x,EcMatrixCIHigh[,rev(order(colMeans(EcMatrix[,,3]))),3],x,EcMatrixCILow[,rev(order(colMeans(EcMatrix[,,3]))),3], col = "grey", lwd = .5)
mtext(bquote(bold("Western and Southern Asia -") ~ italic("Ecoli ") ~ italic("n = ") ~ italic(.(sum(EcNIsolates$NIsolates[which(EcNIsolates$CombRegion == "Western and Southern Asia")])))), side = 3, cex = .8, line = 0.5)




##Plotting Asia bar plot


#convert df into xtab/array
EcoliMatrix <- xtabs(Mean ~  host +antimicrobial_compound + continent, 
                     EcoliMeanDF)
EcoliMatrixCILow = xtabs(CILow ~ host +antimicrobial_compound + continent, 
                       EcoliMeanDF)
EcoliMatrixCIHigh = xtabs(CIHigh ~ host +antimicrobial_compound + continent, 
                          EcoliMeanDF)

EcoliAvarage <- EcoliMeanDF %>%
  group_by(continent)%>%
  select("pathogen", "antimicrobial_compound", "farm_type", "Mean")

EcoliCILow <- EcoliMeanDF %>%
  group_by(continent)%>%
  select("pathogen", "antimicrobial_compound", "farm_type","CILow")

EcoliCIHigh <- EcoliMeanDF %>%
  group_by(continent)%>%
  select("pathogen", "antimicrobial_compound","farm_type", "CIHigh")


EcoliMeanDF %>%
  count(host)%>%
  View()

Ecoli.asia = EcoliAvarage %>%
  filter(continent == "Asia")

Ecoli.europe = EcoliAvarage %>%
  filter(continent == "Europe")

Ecoli.northamerica = EcoliAvarage %>%
  filter(continent == "North America")

Ecoli.oceania = EcoliAvarage %>%
  filter(continent == "Oceania")


Ecoli.southamerica = EcoliAvarage %>%
  filter(continent == "South America")

#visualization bar plot
barplot(Ecoli.asia[,rev(order(colMeans(Ecoli.asia)))],
        beside = T, border = F, ylim = c(0,100),
        cex.names = 1, cex.axis = 1.2, las = 2)

x <-  barplot(EcoliMatrix[,rev(order(colMeans(EcoliMatrix[,,1]))),1], 
             beside = T, plot = FALSE)
x
segments(x,EcoliMatrixCIHigh[,rev(order(colMeans(EcoliMatrix[,,1]))),1],
         x,EcoliMatrixCILow[,rev(order(colMeans(EcoliMatrix[,,1]))),1], 
         col = "grey", lwd = .5)
mtext(bquote(bold("Eastern Asia -") ~ italic("Vibrio ") ~ italic("n = ") ~ italic(.(sum(VibNIsolates$NIsolates[which(VibNIsolates$CombRegion == "Eastern Asia")])))), side = 3, cex = .8, line = 0.5)

barplot(VibMatrix.sea[,rev(order(colMeans(VibMatrix.sea)))], beside = T, col = Cols, border = F, ylim = c(0,100),
        cex.names = 1, cex.axis = 1.2, las = 2)
x =  barplot(VibMatrix[,rev(order(colMeans(VibMatrix[,,2]))),2], beside = T, plot = FALSE)
segments(x,VibMatrixCIHigh[,rev(order(colMeans(VibMatrix[,,2]))),2],x,VibMatrixCILow[,rev(order(colMeans(VibMatrix[,,2]))),2], col = "grey", lwd = .5)
mtext(bquote(bold("South-eastern Asia -") ~ italic("Vibrio ") ~ italic("n = ") ~ italic(.(sum(VibNIsolates$NIsolates[which(VibNIsolates$CombRegion == "South-eastern Asia")])))), side = 3, cex = .8, line = 0.5)

barplot(VibMatrix.wsa[,rev(order(colMeans(VibMatrix.wsa)))], beside = T, col = Cols, border = F, ylim = c(0,100),
        cex.names = 1, cex.axis = 1.2, las = 2)
x =  barplot(VibMatrix[,rev(order(colMeans(VibMatrix[,,3]))),3], beside = T, 
             plot = FALSE)
segments(x,VibMatrixCIHigh[,rev(order(colMeans(VibMatrix[,,3]))),3],
         x,VibMatrixCILow[,rev(order(colMeans(VibMatrix[,,3]))),3], 
         col = "grey", lwd = .5)
mtext(bquote(bold("Western and Southern Asia -") ~ italic("Vibrio ") ~ italic("n = ") ~ italic(.(sum(VibNIsolates$NIsolates[which(VibNIsolates$CombRegion == "Western and Southern Asia")])))), side = 3, cex = .8, line = 0.5)

##data3 <- aggregate(percent_resistant~antimicrobial_compound+
                     continent,data2, mean) %>%
  arrange(continent)
###Ecoli
Ecoli = subset(AqAMR, Pathogen == "Ecoli")
# all AGISAR pairings
Ecoli = Ecoli[Ecoli$Compound %in% c("GEN","CHL","IPM","MEM","FOX","CTX","CRO","CAZ",
                                    "FEP","TIG","AZM","NIT","AMP","AMX","TEM","CST",
                                    "CIP","NAL","PEF","SOX","SXT","SSS","SMN","SMZ",
                                    "TET","TMP"),]
ResIsoEc = mapply(new.function1, Ecoli$Rescom, Ecoli$NIsolates)
ResIsoEc = round(ResIsoEc, digits = 0)
AqAMR.tmp.ec = cbind(Ecoli, ResIsoEc)

#determine total NIsolates
EcNIsolates = aggregate(NIsolates ~ DOI + XCoord + MidDate + SpeciesAg + CombRegion, data = AqAMR.tmp.ec, FUN = unique)
EcNIsolates = EcNIsolates[EcNIsolates$SpeciesAg %in% c("Freshwater Fish", "Marine Fish", "Shrimp"),]
EcNIsolates$NIsolates = sapply(EcNIsolates$NIsolates, function(x) sum(unlist(x)))


#aggregate all resistant isolates and all NIsolates by speciesag, compound and region
EcRes = aggregate(ResIsoEc ~ Compound + SpeciesAg + CombRegion, data = AqAMR.tmp.ec, FUN = sum)
EcAll = aggregate(NIsolates ~ Compound + SpeciesAg + CombRegion, data = AqAMR.tmp.ec, FUN = sum)
#and divide ResIso/NIsolates to return true mean
EcMean = round((EcRes$ResIsoEc /EcAll$NIsolates)*100, digits = 0)
EcMeanDF = as.data.frame(cbind(EcRes$Compound, EcRes$SpeciesAg, EcRes$CombRegion, EcMean, EcAll$NIsolates))
colnames(EcMeanDF) = c("Compound","SpeciesAg","Region", "Mean", "NIsolates")
EcMeanDF$Mean = as.numeric(as.character(EcMeanDF$Mean))
EcMeanDF$NIsolates = as.numeric(as.character(EcMeanDF$NIsolates))
#restrict display fig. 2 to bug-drug pairings where NIsolates > 10
EcMeanDF = EcMeanDF[which(EcMeanDF$NIsolates >= 10),]

#95% CI
CIec = as.data.frame(t(mapply(CI.function, EcMeanDF$Mean/100, EcMeanDF$NIsolates)))
EcMeanDF = cbind(EcMeanDF, round(CIec*100, digits = 0))
colnames(EcMeanDF) = c("Compound","SpeciesAg","Region","Mean", "NIsolates","CILow","CIHigh")
EcMeanDF$CILow[EcMeanDF$CILow < 0] = 0
EcMeanDF$CIHigh[EcMeanDF$CIHigh >100] = 100

#convert df into xtab/array
EcMatrix = xtabs(Mean ~ SpeciesAg + Compound + Region, EcMeanDF)
EcMatrixCILow = xtabs(CILow ~ SpeciesAg + Compound + Region, EcMeanDF)
EcMatrixCIHigh = xtabs(CIHigh ~ SpeciesAg + Compound + Region, EcMeanDF)

#select top 3 SpeciesAg groupings - freshwater fish, marine fish, shrimp
EcMatrix = EcMatrix[c(1:2,5),,]
EcMatrixCILow = EcMatrixCILow[c(1:2,5),,]
EcMatrixCIHigh = EcMatrixCIHigh[c(1:2,5),,]

#remove drugs where resistance = 0
EcMatrix.ea = EcMatrix[,which(colMeans(EcMatrix[,,1])>0),1]
EcMatrix.sea = EcMatrix[,which(colMeans(EcMatrix[,,2])>0),2]
EcMatrix.wsa = EcMatrix[,which(colMeans(EcMatrix[,,3])>0),3]


##Streptococcus
Streptococcus = subset(AqAMR, Pathogen == "Streptococcus")
#Streptococcus (CLSI M100 S28 TABLE 1A,2H-1,2H-2 for all b hemolytic/viridans non S.pneumoniae drug pairings)
Streptococcus = Streptococcus[Streptococcus$Compound %in% c("AZM","CLI","CLR","ERT","MEM","ERY","PEN","AMP","FEP","CTX","CRO",
                                                            "VAN","CPT","CHL","DAP","LVX","LIZ","TET","OFX"),]

ResIsoStrep = mapply(new.function1, Streptococcus$Rescom, Streptococcus$NIsolates)
ResIsoStrep = round(ResIsoStrep, digits = 0)
AqAMR.tmp.strep = cbind(Streptococcus, ResIsoStrep)

#determine total NIsolates
StrepNIsolates = aggregate(NIsolates ~ DOI + XCoord + MidDate + SpeciesAg + CombRegion, data = AqAMR.tmp.strep, FUN = unique)
StrepNIsolates = StrepNIsolates[StrepNIsolates$SpeciesAg %in% c("Freshwater Fish", "Marine Fish", "Shrimp"),]
StrepNIsolates$NIsolates = sapply(StrepNIsolates$NIsolates, function(x) sum(unlist(x)))

#aggregate all resistant isolates and all NIsolates by speciesag, compound and region
StrepAll = aggregate(NIsolates ~ Compound + SpeciesAg + CombRegion, data = AqAMR.tmp.strep, FUN = sum)
StrepRes = aggregate(ResIsoStrep ~ Compound + SpeciesAg + CombRegion, data = AqAMR.tmp.strep, FUN = sum)
#and divide ResIso/NIsolates to return true mean
StrepMean = round((StrepRes$ResIsoStrep/StrepAll$NIsolates)*100, digits = 0)
StrepMeanDF = as.data.frame(cbind(StrepRes$Compound, StrepRes$SpeciesAg, StrepRes$CombRegion, StrepMean, StrepAll$NIsolates))
colnames(StrepMeanDF) = c("Compound","SpeciesAg", "Region", "Mean", "NIsolates")
StrepMeanDF$Mean = as.numeric(as.character(StrepMeanDF$Mean))
StrepMeanDF$NIsolates = as.numeric(as.character(StrepMeanDF$NIsolates))
#restrict display fig. 2 to bug-drug pairings where NIsolates > 10
StrepMeanDF = StrepMeanDF[which(StrepMeanDF$NIsolates >= 10),]

#95% CI
CIstrep = as.data.frame(t(mapply(CI.function, StrepMeanDF$Mean/100, StrepMeanDF$NIsolates)))
StrepMeanDF = cbind(StrepMeanDF, round(CIstrep*100, digits = 0))
colnames(StrepMeanDF) = c("Compound","SpeciesAg","Region","Mean","NIsolates","CILow","CIHigh")
StrepMeanDF$CILow[StrepMeanDF$CILow < 0] = 0
StrepMeanDF$CIHigh[StrepMeanDF$CIHigh >100] = 100

#convert df into xtab/array
StrepMatrix = xtabs(Mean ~ SpeciesAg + Compound + Region, StrepMeanDF)
StrepMatrixCILow = xtabs(CILow ~ SpeciesAg + Compound + Region, StrepMeanDF)
StrepMatrixCIHigh = xtabs(CIHigh ~ SpeciesAg + Compound + Region, StrepMeanDF)

#select top 3 SpeciesAg groupings - freshwater fish, marine fish (NO SHRIMP DATA)
StrepMatrix = StrepMatrix[c(2:3),,]
StrepMatrixCILow = StrepMatrixCILow[c(2:3),,]
StrepMatrixCIHigh = StrepMatrixCIHigh[c(2:3),,]

#remove drugs where resistance = 0
StrepMatrix.ea = StrepMatrix[,which(colMeans(StrepMatrix[,,1])>0),1]
StrepMatrix.sea = StrepMatrix[,which(colMeans(StrepMatrix[,,2])>0),2]
StrepMatrix.wsa = StrepMatrix[,which(colMeans(StrepMatrix[,,3])>0),3]

ColsStrep = c("sky blue","#3182BD")

##Aeromonas
Aeromonas = subset(AqAMR, Pathogen == "Aeromonas")
#Aeromonas (CLSI M45 A3 2015 Table 3 drug pairings)
Aeromonas = Aeromonas[Aeromonas$Compound %in% c("PIT","FEP","CTX","FOX",
                                                "CAZ","CRO","CXM","ERT","IPM","MEM","ATM",
                                                "AMK","GEN","TET","CIP","LVX","SXT","CHL"),]
ResIsoAero = mapply(new.function1, Aeromonas$Rescom, Aeromonas$NIsolates)
ResIsoAero = round(ResIsoAero, digits = 0)
AqAMR.tmp.aero = cbind(Aeromonas, ResIsoAero)

AeroNIsolates = aggregate(NIsolates ~ DOI + XCoord + MidDate + SpeciesAg + CombRegion, data = AqAMR.tmp.aero, FUN = unique)
AeroNIsolates = AeroNIsolates[AeroNIsolates$SpeciesAg %in% c("Freshwater Fish", "Marine Fish", "Shrimp"),]
AeroNIsolates$NIsolates = sapply(AeroNIsolates$NIsolates, function(x) sum(unlist(x)))

#aggregate all resistant isolates and all NIsolates by speciesag, compound and region
AeroRes = aggregate(ResIsoAero ~ Compound + SpeciesAg + CombRegion, data = AqAMR.tmp.aero, FUN = sum)
AeroAll = aggregate(NIsolates ~ Compound + SpeciesAg + CombRegion, data = AqAMR.tmp.aero, FUN = sum)
#and divide ResIso/NIsolates to return true mean
AeroMean = round((AeroRes$ResIsoAero/AeroAll$NIsolates)*100, digits = 0)
AeroMeanDF = as.data.frame(cbind(AeroRes$Compound, AeroRes$SpeciesAg, AeroRes$CombRegion, AeroMean, AeroAll$NIsolates))
colnames(AeroMeanDF) = c("Compound","SpeciesAg", "Region","Mean", "NIsolates")
AeroMeanDF$Mean = as.numeric(as.character(AeroMeanDF$Mean))
AeroMeanDF$NIsolates = as.numeric(as.character(AeroMeanDF$NIsolates))
#restrict display fig. 2 to bug-drug pairings where NIsolates > 10
AeroMeanDF = AeroMeanDF[which(AeroMeanDF$NIsolates >= 10),]

#95% CI
CIaero = as.data.frame(t(mapply(CI.function, AeroMeanDF$Mean/100, AeroMeanDF$NIsolates)))
AeroMeanDF = cbind(AeroMeanDF, round(CIaero*100, digits = 0))
colnames(AeroMeanDF) = c("Compound","SpeciesAg","Region", "Mean","NIsolates","CILow","CIHigh")
AeroMeanDF$CILow[AeroMeanDF$CILow < 0] = 0
AeroMeanDF$CIHigh[AeroMeanDF$CIHigh >100] = 100

#convert df into xtab/array
AeroMatrix = xtabs(Mean ~ SpeciesAg + Compound + Region, AeroMeanDF)
AeroMatrixCILow = xtabs(CILow ~ SpeciesAg + Compound + Region, AeroMeanDF)
AeroMatrixCIHigh = xtabs(CIHigh ~ SpeciesAg + Compound + Region, AeroMeanDF)

#select top 3 SpeciesAg groupings - freshwater fish, marine fish, shrimp
AeroMatrix = AeroMatrix[c(2:3,6),,]
AeroMatrixCILow = AeroMatrixCILow[c(2:3,6),,]
AeroMatrixCIHigh = AeroMatrixCIHigh[c(2:3,6),,]

#remove drugs where resistance = 0
AeroMatrix.ea = AeroMatrix[,which(colMeans(AeroMatrix[,,1])>0),1]
AeroMatrix.sea = AeroMatrix[,which(colMeans(AeroMatrix[,,2])>0),2]
AeroMatrix.wsa = AeroMatrix[,which(colMeans(AeroMatrix[,,3])>0),3]


#visualize fig. 2 - multi-panel 3 regions x 4 pathogens
layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12), nrow = 4, ncol= 3, byrow = T))
par(mar=c(3.6,3.5,2.4,.9), oma=rep(0, 4))
#vibrio
barplot(VibMatrix.ea[,rev(order(colMeans(VibMatrix.ea)))], beside = T, col = Cols, border = F, ylim = c(0,100),
        cex.names = 1, cex.axis = 1.2, las = 2)
x =  barplot(VibMatrix[,rev(order(colMeans(VibMatrix[,,1]))),1], beside = T, plot = FALSE)
segments(x,VibMatrixCIHigh[,rev(order(colMeans(VibMatrix[,,1]))),1],x,VibMatrixCILow[,rev(order(colMeans(VibMatrix[,,1]))),1], col = "grey", lwd = .5)
mtext(bquote(bold("Eastern Asia -") ~ italic("Vibrio ") ~ italic("n = ") ~ italic(.(sum(VibNIsolates$NIsolates[which(VibNIsolates$CombRegion == "Eastern Asia")])))), side = 3, cex = .8, line = 0.5)

barplot(VibMatrix.sea[,rev(order(colMeans(VibMatrix.sea)))], beside = T, col = Cols, border = F, ylim = c(0,100),
        cex.names = 1, cex.axis = 1.2, las = 2)
x =  barplot(VibMatrix[,rev(order(colMeans(VibMatrix[,,2]))),2], beside = T, plot = FALSE)
segments(x,VibMatrixCIHigh[,rev(order(colMeans(VibMatrix[,,2]))),2],x,VibMatrixCILow[,rev(order(colMeans(VibMatrix[,,2]))),2], col = "grey", lwd = .5)
mtext(bquote(bold("South-eastern Asia -") ~ italic("Vibrio ") ~ italic("n = ") ~ italic(.(sum(VibNIsolates$NIsolates[which(VibNIsolates$CombRegion == "South-eastern Asia")])))), side = 3, cex = .8, line = 0.5)

barplot(VibMatrix.wsa[,rev(order(colMeans(VibMatrix.wsa)))], beside = T, col = Cols, border = F, ylim = c(0,100),
        cex.names = 1, cex.axis = 1.2, las = 2)
x =  barplot(VibMatrix[,rev(order(colMeans(VibMatrix[,,3]))),3], beside = T, plot = FALSE)
segments(x,VibMatrixCIHigh[,rev(order(colMeans(VibMatrix[,,3]))),3],x,VibMatrixCILow[,rev(order(colMeans(VibMatrix[,,3]))),3], col = "grey", lwd = .5)
mtext(bquote(bold("Western and Southern Asia -") ~ italic("Vibrio ") ~ italic("n = ") ~ italic(.(sum(VibNIsolates$NIsolates[which(VibNIsolates$CombRegion == "Western and Southern Asia")])))), side = 3, cex = .8, line = 0.5)

#Ecoli
barplot(EcMatrix.ea[,rev(order(colMeans(EcMatrix.ea)))],
        beside = T, col = Cols, border = F, ylim = c(0,100),
        cex.names = 1.2, cex.axis = 1.2, las = 2)
x =  barplot(EcMatrix[,rev(order(colMeans(EcMatrix[,,1]))),1], beside = T, plot = FALSE)
segments(x,EcMatrixCIHigh[,rev(order(colMeans(EcMatrix[,,1]))),1],x,EcMatrixCILow[,rev(order(colMeans(EcMatrix[,,1]))),1], col = "grey", lwd = .5)
mtext(bquote(bold("Eastern Asia -") ~ italic("Ecoli ") ~ italic("n = ") ~ italic(.(sum(EcNIsolates$NIsolates[which(EcNIsolates$CombRegion == "Eastern Asia")])))), side = 3, cex = .8, line = 0.5)

barplot(EcMatrix.sea[,rev(order(colMeans(EcMatrix.sea)))], beside = T, col = Cols, border = F, ylim = c(0,100),
        cex.names = 1.2, cex.axis = 1.2, las = 2)
x =  barplot(EcMatrix[,rev(order(colMeans(EcMatrix[,,2]))),2], beside = T, plot = FALSE)
segments(x,EcMatrixCIHigh[,rev(order(colMeans(EcMatrix[,,2]))),2],x,EcMatrixCILow[,rev(order(colMeans(EcMatrix[,,2]))),2], col = "grey", lwd = .5)
mtext(bquote(bold("South-eastern Asia -") ~ italic("Ecoli ") ~ italic("n = ") ~ italic(.(sum(EcNIsolates$NIsolates[which(EcNIsolates$CombRegion == "South-eastern Asia")])))), side = 3, cex = .8, line = 0.5)

barplot(EcMatrix.wsa[,rev(order(colMeans(EcMatrix.wsa)))], beside = T, col = Cols, border = F, ylim = c(0,100),
        cex.names = 1.2, cex.axis = 1.2, las = 2)
x =  barplot(EcMatrix[,rev(order(colMeans(EcMatrix[,,3]))),3], beside = T, plot = FALSE)
segments(x,EcMatrixCIHigh[,rev(order(colMeans(EcMatrix[,,3]))),3],x,EcMatrixCILow[,rev(order(colMeans(EcMatrix[,,3]))),3], col = "grey", lwd = .5)
mtext(bquote(bold("Western and Southern Asia -") ~ italic("Ecoli ") ~ italic("n = ") ~ italic(.(sum(EcNIsolates$NIsolates[which(EcNIsolates$CombRegion == "Western and Southern Asia")])))), side = 3, cex = .8, line = 0.5)

#Streptococcus
barplot(StrepMatrix.ea[,rev(order(colMeans(StrepMatrix.ea)))], beside = T, col = ColsStrep, border = F, ylim = c(0,100),
        cex.names = 1.2, cex.axis = 1.2, las = 2)
x =  barplot(StrepMatrix[,rev(order(colMeans(StrepMatrix[,,1]))),1], beside = T, plot = FALSE)
segments(x,StrepMatrixCIHigh[,rev(order(colMeans(StrepMatrix[,,1]))),1],x,StrepMatrixCILow[,rev(order(colMeans(StrepMatrix[,,1]))),1], col = "grey", lwd = .5)
mtext(bquote(bold("Eastern Asia -") ~ italic("Streptococcus ") ~ italic("n = ") ~ italic(.(sum(StrepNIsolates$NIsolates[which(StrepNIsolates$CombRegion == "Eastern Asia")])))), side = 3, cex = .8, line = 0.5)

barplot(StrepMatrix.sea[,rev(order(colMeans(StrepMatrix.sea)))], beside = T, col = ColsStrep, border = F, ylim = c(0,100),
        cex.names = 1.2, cex.axis = 1.2, las = 2)
x =  barplot(StrepMatrix[,rev(order(colMeans(StrepMatrix[,,2]))),2], beside = T, plot = FALSE)
segments(x,StrepMatrixCIHigh[,rev(order(colMeans(StrepMatrix[,,2]))),2],x,StrepMatrixCILow[,rev(order(colMeans(StrepMatrix[,,2]))),2], col = "grey", lwd = .5)
mtext(bquote(bold("South-eastern Asia -") ~ italic("Streptococcus ") ~ italic("n = ") ~ italic(.(sum(StrepNIsolates$NIsolates[which(StrepNIsolates$CombRegion == "South-eastern Asia")])))), side = 3, cex = .8, line = 0.5)

barplot(StrepMatrix.wsa[,rev(order(colMeans(StrepMatrix.wsa)))], beside = T, col = ColsStrep, border = F, ylim = c(0,100),
        cex.names = 1.2, cex.axis = 1.2, las = 2)
x =  barplot(StrepMatrix[,rev(order(colMeans(StrepMatrix[,,3]))),3], beside = T, plot = FALSE)
segments(x,StrepMatrixCIHigh[,rev(order(colMeans(StrepMatrix[,,3]))),3],x,StrepMatrixCILow[,rev(order(colMeans(StrepMatrix[,,3]))),3], col = "grey", lwd = .5)
mtext(bquote(bold("Western and Southern Asia -") ~ italic("Streptococcus ") ~ italic("n = ") ~ italic(.(sum(StrepNIsolates$NIsolates[which(StrepNIsolates$CombRegion == "Western and Southern Asia")])))), side = 3, cex = .8, line = 0.5)

#Aeromonas
barplot(AeroMatrix.ea[,rev(order(colMeans(AeroMatrix.ea)))], beside = T, col = Cols, border = F, ylim = c(0,100),
        cex.names = 1.2, cex.axis = 1.2,las = 2)
x =  barplot(AeroMatrix[,rev(order(colMeans(AeroMatrix[,,1]))),1], beside = T, plot = FALSE)
segments(x,AeroMatrixCIHigh[,rev(order(colMeans(AeroMatrix[,,1]))),1],x,AeroMatrixCILow[,rev(order(colMeans(AeroMatrix[,,1]))),1], col = "grey", lwd = .5)
mtext(bquote(bold("Eastern Asia -") ~ italic("Aeromonas ") ~ italic("n = ") ~ italic(.(sum(AeroNIsolates$NIsolates[which(AeroNIsolates$CombRegion == "Eastern Asia")])))), side = 3, cex = .8, line = 0.5)

barplot(AeroMatrix.sea[,rev(order(colMeans(AeroMatrix.sea)))], beside = T, col = Cols, border = F, ylim = c(0,100),
        cex.names = 1.2, cex.axis = 1.2,las = 2)
x =  barplot(AeroMatrix[,rev(order(colMeans(AeroMatrix[,,2]))),2], beside = T, plot = FALSE)
segments(x,AeroMatrixCIHigh[,rev(order(colMeans(AeroMatrix[,,2]))),2],x,AeroMatrixCILow[,rev(order(colMeans(AeroMatrix[,,2]))),2], col = "grey", lwd = .5)
mtext(bquote(bold("South-eastern Asia -") ~ italic("Aeromonas ") ~ italic("n = ") ~ italic(.(sum(AeroNIsolates$NIsolates[which(AeroNIsolates$CombRegion == "South-eastern Asia")])))), side = 3, cex = .8, line = 0.5)

barplot(AeroMatrix.wsa[,rev(order(colMeans(AeroMatrix.wsa)))], beside = T, col = Cols, border = F, ylim = c(0,100),
        cex.names = 1.2, cex.axis = 1.2,las = 2)
x =  barplot(AeroMatrix[,rev(order(colMeans(AeroMatrix[,,3]))),3], beside = T, plot = FALSE)
segments(x,AeroMatrixCIHigh[,rev(order(colMeans(AeroMatrix[,,3]))),3],x,AeroMatrixCILow[,rev(order(colMeans(AeroMatrix[,,3]))),3], col = "grey", lwd = .5)
mtext(bquote(bold("Western and Southern Asia -") ~ italic("Aeromonas ") ~ italic("n = ") ~ italic(.(sum(AeroNIsolates$NIsolates[which(AeroNIsolates$CombRegion == "Western and Southern Asia")])))), side = 3, cex = .8, line = 0.5)

par(mfrow = c(1,1))
mtext("Resistance (%)", side = 2, cex = .93, line = 2.7, las = 0)
legend(5, 113, c("Freshwater Fish", "Marine Fish", "Shrimp"), fill = Cols, cex = .65, bty = "n")


##################################
#Foodborne pathogens resistance
#Resistance prevalence by pathogen and drug
#PEN, SUL, TET, QUI, AMI
#3rdG CEP,CAR and CST and HPCIA
#################################################
#classes recommended for AST in CLSI/AGISAR
FB = subset(AqAMR, Pathogen %in% c("Aeromonas","Vibrio","Ecoli","Streptococcus"))

#PEN
FBpen = subset(FB, Compound %in% c("PIT","AMP","AMX","AMC","PEN","SAM","PIP"))
ResIsoFBpen = mapply(new.function1, FBpen$Rescom, FBpen$NIsolates)
ResIsoFBpen = round(ResIsoFBpen, digits = 0)
AqAMR.tmp.FBpen = cbind(FBpen, ResIsoFBpen)
#overall rate of PEN from foodborne pathogens
FBpenmean = sum(AqAMR.tmp.FBpen$ResIsoFBpen, na.rm = T)/sum(AqAMR.tmp.FBpen$NIsolates, na.rm = T)
#95CI
CIFBpen = as.data.frame(t(mapply(CI.function, FBpenmean, sum(AqAMR.tmp.FBpen$NIsolates, na.rm = T))))
CIFBpen = round(CIFBpen*100, digits = 0)
colnames(CIFBpen) = c("CILow","CIHigh")

#SUL
FBsul = subset(FB, Compound %in% c("SMN","SMZ","SOX","SSS","SUD","SXT","TMP"))
ResIsoFBsul = mapply(new.function1, FBsul$Rescom, FBsul$NIsolates)
ResIsoFBsul = round(ResIsoFBsul, digits = 0)
AqAMR.tmp.FBsul = cbind(FBsul, ResIsoFBsul)
#overall rate of SUL from foodborne pathogens
FBsulmean = sum(AqAMR.tmp.FBsul$ResIsoFBsul, na.rm = T)/sum(AqAMR.tmp.FBsul$NIsolates, na.rm = T)
#95CI
CIFBsul = as.data.frame(t(mapply(CI.function, FBsulmean, sum(AqAMR.tmp.FBsul$NIsolates, na.rm = T))))
CIFBsul = round(CIFBsul*100, digits = 0)
colnames(CIFBsul) = c("CILow","CIHigh")

#AMI
FBami = subset(FB, Compound %in% c("AMK","GEN"))
ResIsoFBami = mapply(new.function1, FBami$Rescom, FBami$NIsolates)
ResIsoFBami = round(ResIsoFBami, digits = 0)
AqAMR.tmp.FBami = cbind(FBami, ResIsoFBami)
#overall rate of AMI from foodborne pathogens
FBamimean = sum(AqAMR.tmp.FBami$ResIsoFBami, na.rm = T)/sum(AqAMR.tmp.FBami$NIsolates, na.rm = T)
#95CI
CIFBami = as.data.frame(t(mapply(CI.function, FBamimean, sum(AqAMR.tmp.FBami$NIsolates, na.rm = T))))
CIFBami = round(CIFBami*100, digits = 0)
colnames(CIFBami) = c("CILow","CIHigh")

#TET
FBtet = subset(FB, Compound %in% c("TET","DOX"))
ResIsoFBtet = mapply(new.function1, FBtet$Rescom, FBtet$NIsolates)
ResIsoFBtet = round(ResIsoFBtet, digits = 0)
AqAMR.tmp.FBtet = cbind(FBtet, ResIsoFBtet)
#overall rate of TET from foodborne pathogens
FBtetmean = sum(AqAMR.tmp.FBtet$ResIsoFBtet, na.rm = T)/sum(AqAMR.tmp.FBtet$NIsolates, na.rm = T)
#95CI
CIFBtet = as.data.frame(t(mapply(CI.function, FBtetmean, sum(AqAMR.tmp.FBtet$NIsolates, na.rm = T))))
CIFBtet = round(CIFBtet*100, digits = 0)
colnames(CIFBtet) = c("CILow","CIHigh")

#3rd/4th G CEP
FBcep = subset(FB, Compound %in% c("CTX","CRO","CAZ","FEP"))
ResIsoFBcep = mapply(new.function1, FBcep$Rescom, FBcep$NIsolates)
ResIsoFBcep = round(ResIsoFBcep, digits = 0)
AqAMR.tmp.FBcep = cbind(FBcep, ResIsoFBcep)
#overall rate of 3rd/4th G CEP from foodborne pathogens
FBcepmean = sum(AqAMR.tmp.FBcep$ResIsoFBcep, na.rm = T)/sum(AqAMR.tmp.FBcep$NIsolates, na.rm = T)
#95CI
CIFBcep = as.data.frame(t(mapply(CI.function, FBcepmean, sum(AqAMR.tmp.FBcep$NIsolates, na.rm = T))))
CIFBcep = round(CIFBcep*100, digits = 0)
colnames(CIFBcep) = c("CILow","CIHigh")

#MAC
FBmac = subset(FB, Compound %in% c("AZM","CLR","ERY"))
ResIsoFBmac = mapply(new.function1, FBmac$Rescom, FBmac$NIsolates)
ResIsoFBmac = round(ResIsoFBmac, digits = 0)
AqAMR.tmp.FBmac = cbind(FBmac, ResIsoFBmac)
#overall rate of MAC from foodborne pathogens
FBmacmean = sum(AqAMR.tmp.FBmac$ResIsoFBmac, na.rm = T)/sum(AqAMR.tmp.FBmac$NIsolates, na.rm = T)
#95CI
CIFBmac = as.data.frame(t(mapply(CI.function, FBmacmean, sum(AqAMR.tmp.FBmac$NIsolates, na.rm = T))))
CIFBmac = round(CIFBmac*100, digits = 0)
colnames(CIFBmac) = c("CILow","CIHigh")

#QUI
FBqui = subset(FB, Compound %in% c("CIP","LVX","PEF","NAL","OFX"))
ResIsoFBqui = mapply(new.function1, FBqui$Rescom, FBqui$NIsolates)
ResIsoFBqui = round(ResIsoFBqui, digits = 0)
AqAMR.tmp.FBqui = cbind(FBqui, ResIsoFBqui)
#overall rate of QUI from foodborne pathogens
FBquimean = sum(AqAMR.tmp.FBqui$ResIsoFBqui, na.rm = T)/sum(AqAMR.tmp.FBqui$NIsolates, na.rm = T)
#95CI
CIFBqui = as.data.frame(t(mapply(CI.function, FBquimean, sum(AqAMR.tmp.FBqui$NIsolates, na.rm = T))))
CIFBqui = round(CIFBqui*100, digits = 0)
colnames(CIFBqui) = c("CILow","CIHigh")

#GLY
FBgly = subset(FB, Compound %in% c("VAN") & Pathogen == "Streptococcus")
ResIsoFBgly = mapply(new.function1, FBgly$Rescom, FBgly$NIsolates)
ResIsoFBgly = round(ResIsoFBgly, digits = 0)
AqAMR.tmp.FBgly = cbind(FBgly, ResIsoFBgly)
#overall rate VAN from Strep
FBglymean = sum(AqAMR.tmp.FBgly$ResIsoFBgly, na.rm = T)/sum(AqAMR.tmp.FBgly$NIsolates, na.rm = T)
#95CI
CIFBgly = as.data.frame(t(mapply(CI.function, FBglymean, sum(AqAMR.tmp.FBgly$NIsolates, na.rm = T))))
CIFBgly = round(CIFBgly*100, digits = 0)
colnames(CIFBgly) = c("CILow","CIHigh")

#### AWaRe RESERVE Group
#fosfomycin in ecoli
FOFec = subset(AqAMR, Compound %in% c("FOF") & Pathogen == "Ecoli")

ResIsoFOFec = mapply(new.function1, FOFec$Rescom, FOFec$NIsolates)
ResIsoFOFec = round(ResIsoFOFec, digits = 0)
AqAMR.tmp.FOFec = cbind(FOFec, ResIsoFOFec)

#overall rate of FOF resistance 
FOFecmean = sum(AqAMR.tmp.FOFec$ResIsoFOFec, na.rm = T)/sum(AqAMR.tmp.FOFec$NIsolates, na.rm = T)
#95CI
CIFOFec = as.data.frame(t(mapply(CI.function, FOFecmean, sum(AqAMR.tmp.FOFec$NIsolates, na.rm = T))))
CIFOFec = round(CIFOFec*100, digits = 0)
colnames(CIFOFec) = c("CILow","CIHigh")

#PMB in Ecoli
PMBec = subset(AqAMR, Compound %in% c("PMB") & Pathogen == "Ecoli")

ResIsoPMBec = mapply(new.function1, PMBec$Rescom, PMBec$NIsolates)
ResIsoPMBec = round(ResIsoPMBec, digits = 0)
AqAMR.tmp.PMBec = cbind(PMBec, ResIsoPMBec)

#overall rate
PMBecmean = sum(AqAMR.tmp.PMBec$ResIsoPMBec, na.rm = T)/sum(AqAMR.tmp.PMBec$NIsolates, na.rm = T)
#95CI
CIPMBec = as.data.frame(t(mapply(CI.function, PMBecmean, sum(AqAMR.tmp.PMBec$NIsolates, na.rm = T))))
CIPMBec = round(CIPMBec*100, digits = 0)
colnames(CIPMBec) = c("CILow","CIHigh")

#PMB in Vibrio
PMBvib = subset(AqAMR, Compound %in% c("PMB") & Pathogen == "Vibrio")

ResIsoPMBvib = mapply(new.function1, PMBvib$Rescom, PMBvib$NIsolates)
ResIsoPMBvib = round(ResIsoPMBvib, digits = 0)
AqAMR.tmp.PMBvib = cbind(PMBvib, ResIsoPMBvib)

#overall rate 
PMBvibmean = sum(AqAMR.tmp.PMBvib$ResIsoPMBvib, na.rm = T)/sum(AqAMR.tmp.PMBvib$NIsolates, na.rm = T)
#95CI
CIPMBvib = as.data.frame(t(mapply(CI.function, PMBvibmean, sum(AqAMR.tmp.PMBvib$NIsolates, na.rm = T))))
CIPMBvib = round(CIPMBvib*100, digits = 0)
colnames(CIPMBvib) = c("CILow","CIHigh")

#PMB in Aeromonas
PMBaer = subset(AqAMR, Compound %in% c("PMB") & Pathogen == "Aeromonas")

ResIsoPMBaer = mapply(new.function1, PMBaer$Rescom, PMBaer$NIsolates)
ResIsoPMBaer = round(ResIsoPMBaer, digits = 0)
AqAMR.tmp.PMBaer = cbind(PMBaer, ResIsoPMBaer)

#overall rate 
PMBaermean = sum(AqAMR.tmp.PMBaer$ResIsoPMBaer, na.rm = T)/sum(AqAMR.tmp.PMBaer$NIsolates, na.rm = T)
#95CI
CIPMBaer = as.data.frame(t(mapply(CI.function, PMBaermean, sum(AqAMR.tmp.PMBaer$NIsolates, na.rm = T))))
CIPMBaer = round(CIPMBaer*100, digits = 0)
colnames(CIPMBaer) = c("CILow","CIHigh")


##3rd Gen CEP ecoli
CEP3G = subset(AqAMR, Compound %in% c("CAZ","CRO","CTX"))
CEP3G_ecoli = subset(CEP3G, Pathogen == "Ecoli")

ResIsoCEP3G_ecoli = mapply(new.function1, CEP3G_ecoli$Rescom, CEP3G_ecoli$NIsolates)
ResIsoCEP3G_ecoli = round(ResIsoCEP3G_ecoli, digits = 0)
AqAMR.tmp.CEP3G_ecoli = cbind(CEP3G_ecoli, ResIsoCEP3G_ecoli)

#overall rate 
CEP3G_ecoli_mean = sum(AqAMR.tmp.CEP3G_ecoli$ResIsoCEP3G_ecoli, na.rm = T)/sum(AqAMR.tmp.CEP3G_ecoli$NIsolates, na.rm = T)
#95CI
CICEP3G_ecoli = as.data.frame(t(mapply(CI.function, CEP3G_ecoli_mean, sum(AqAMR.tmp.CEP3G_ecoli$NIsolates, na.rm = T))))
CICEP3G_ecoli = round(CICEP3G_ecoli*100, digits = 0)
colnames(CICEP3G_ecoli) = c("CILow","CIHigh")

###CAR
CAR = subset(AqAMR, Drug == "CAR")

ResIsoCAR = mapply(new.function1, CAR$Rescom, CAR$NIsolates)
ResIsoCAR = round(ResIsoCAR, digits = 0)
AqAMR.tmp.CAR = cbind(CAR, ResIsoCAR)

#overall rate of CAR resistance from all surveys in the study
CARmean = sum(AqAMR.tmp.CAR$ResIsoCAR, na.rm = T)/sum(AqAMR.tmp.CAR$NIsolates, na.rm = T)
#95CI
CICAR = as.data.frame(t(mapply(CI.function, CARmean, sum(AqAMR.tmp.CAR$NIsolates, na.rm = T))))
CICAR = round(CICAR*100, digits = 0)
colnames(CICAR) = c("CILow","CIHigh")

#CAR Aeromonas in WSA
CARaer = subset(CAR, Pathogen %in% c("Aeromonas") & CombRegion %in% "Western and Southern Asia")

ResIsoCARaer = mapply(new.function1, CARaer$Rescom, CARaer$NIsolates)
ResIsoCARaer = round(ResIsoCARaer, digits = 0)
AqAMR.tmp.CARaer = cbind(CARaer, ResIsoCARaer)

#overall rate of CAR resistance from aeromonas in WSA
CARaermean = sum(AqAMR.tmp.CARaer$ResIsoCARaer, na.rm = T)/sum(AqAMR.tmp.CARaer$NIsolates, na.rm = T)
#95CI
CICARaer = as.data.frame(t(mapply(CI.function, CARaermean, sum(AqAMR.tmp.CARaer$NIsolates, na.rm = T))))
CICARaer = round(CICARaer*100, digits = 0)
colnames(CICARaer) = c("CILow","CIHigh")

#CAR Aeromonas by sampling date
#before 2010
CARaer_bf2010 = subset(CARaer, MidDate > "2000-01-01" & MidDate < "2010-01-01")
ResIsoCARaer_bf2010 = mapply(new.function1, CARaer_bf2010$Rescom, CARaer_bf2010$NIsolates)
ResIsoCARaer_bf2010 = round(ResIsoCARaer_bf2010, digits = 0)
AqAMR.tmp.CARaer_bf2010 = cbind(CARaer_bf2010, ResIsoCARaer_bf2010)

#overall rate of CAR resistance from aeromonas
CARaer_bf2010mean = sum(AqAMR.tmp.CARaer_bf2010$ResIsoCARaer_bf2010, na.rm = T)/sum(AqAMR.tmp.CARaer_bf2010$NIsolates, na.rm = T)
#95CI
CICARaer_bf2010 = as.data.frame(t(mapply(CI.function, CARaer_bf2010mean, sum(AqAMR.tmp.CARaer_bf2010$NIsolates, na.rm = T))))
CICARaer_bf2010 = round(CICARaer_bf2010*100, digits = 0)
colnames(CICARaer_bf2010) = c("CILow","CIHigh")

#after 2010
CARaer_af2010 = subset(CARaer, MidDate > "2010-01-01" & MidDate < "2019-01-01")
ResIsoCARaer_af2010 = mapply(new.function1, CARaer_af2010$Rescom, CARaer_af2010$NIsolates)
ResIsoCARaer_af2010 = round(ResIsoCARaer_af2010, digits = 0)
AqAMR.tmp.CARaer_af2010 = cbind(CARaer_af2010, ResIsoCARaer_af2010)

#overall rate of CAR resistance from aeromonas
CARaer_af2010mean = sum(AqAMR.tmp.CARaer_af2010$ResIsoCARaer_af2010, na.rm = T)/sum(AqAMR.tmp.CARaer_af2010$NIsolates, na.rm = T)
#95CI
CICARaer_af2010 = as.data.frame(t(mapply(CI.function, CARaer_af2010mean, sum(AqAMR.tmp.CARaer_af2010$NIsolates, na.rm = T))))
CICARaer_af2010 = round(CICARaer_af2010*100, digits = 0)
colnames(CICARaer_af2010) = c("CILow","CIHigh")

#Fisher's exact test
CARtable = matrix(c(sum(AqAMR.tmp.CARaer_bf2010$ResIsoCARaer_bf2010, na.rm = T),sum(AqAMR.tmp.CARaer_af2010$ResIsoCARaer_af2010, na.rm = T),
                    sum(AqAMR.tmp.CARaer_bf2010$NIsolates, na.rm = T)-sum(AqAMR.tmp.CARaer_bf2010$ResIsoCARaer_bf2010, na.rm = T),sum(AqAMR.tmp.CARaer_af2010$NIsolates, na.rm = T)-sum(AqAMR.tmp.CARaer_af2010$ResIsoCARaer_af2010, na.rm = T)),
                  nrow = 2, ncol = 2, byrow = T)
CARaer_fisher = fisher.test(CARtable)


#CAR foodborne (no Aeromonas)
CARfb = subset(CAR, Pathogen %in% c("Vibrio","Ecoli","Streptococcus"))

ResIsoCARfb = mapply(new.function1, CARfb$Rescom, CARfb$NIsolates)
ResIsoCARfb = round(ResIsoCARfb, digits = 0)
AqAMR.tmp.CARfb = cbind(CARfb, ResIsoCARfb)

#overall rate of CAR foodborne resistance (no aeromonas)
CARaerfbmean = sum(AqAMR.tmp.CARfb$ResIsoCARfb, na.rm = T)/sum(AqAMR.tmp.CARfb$NIsolates, na.rm = T)
#95CI
CICARaerfb = as.data.frame(t(mapply(CI.function, CARaerfbmean, sum(AqAMR.tmp.CARfb$NIsolates, na.rm = T))))
CICARaerfb = round(CICARaerfb*100, digits = 0)
colnames(CICARaerfb) = c("CILow","CIHigh")

#determine total NIsolates
CARNIsolates = aggregate(NIsolates ~ DOI + XCoord + MidDate + Pathogen + SpeciesAg + CombRegion, data = AqAMR.tmp.CAR, FUN = unique)
CARNIsolates$NIsolates = sapply(CARNIsolates$NIsolates, function(x) sum(unlist(x)))

#aggregate all resistant isolates and all NIsolates by speciesag, compound and region
CARRes = aggregate(ResIsoCAR ~ Compound + Pathogen + SpeciesAg + CombRegion, data = AqAMR.tmp.CAR, FUN = sum)
CARAll = aggregate(NIsolates ~ Compound + Pathogen + SpeciesAg + CombRegion, data = AqAMR.tmp.CAR, FUN = sum)
#and divide ResIso/NIsolates to return true mean
CARMean = round((CARRes$ResIsoCAR /CARAll$NIsolates)*100, digits = 0)
CARMeanDF = as.data.frame(cbind(CARRes$Compound, CARRes$Pathogen,CARRes$SpeciesAg, CARRes$CombRegion, CARMean, CARAll$NIsolates))
colnames(CARMeanDF) = c("Compound","Pathogen","SpeciesAg","Region","Mean","NIsolates")
CARMeanDF$Mean = as.numeric(as.character(CARMeanDF$Mean))
CARMeanDF$NIsolates = as.numeric(as.character(CARMeanDF$NIsolates))

####CST
#CST All Pathogens
CST = subset(AqAMR, Compound == "CST")

ResIsoCST = mapply(new.function1, CST$Rescom, CST$NIsolates)
ResIsoCST = round(ResIsoCST, digits = 0)
AqAMR.tmp.CST = cbind(CST, ResIsoCST)

#overall rate of CST resistance from all surveys in the study
CSTmean = sum(AqAMR.tmp.CST$ResIsoCST, na.rm = T)/sum(AqAMR.tmp.CST$NIsolates, na.rm = T)
#95CI
CICST = as.data.frame(t(mapply(CI.function, CSTmean, sum(AqAMR.tmp.CST$NIsolates, na.rm = T))))
CICST = round(CICST*100, digits = 0)
colnames(CICST) = c("CILow","CIHigh")

#CST Gram negative
CSTgn = subset(CST, Pathogen %in% c("Ecoli","Edwardsiella","Klebsiella","Acinetobacter","Aeromonas",
                                    "Chryseobacterium","Citrobacter","Elizabethkingia","Flavobacterium",
                                    "Morganella","Pantoea","Pseudomonas","Serratia","Shewanella","Stenotrophomonas",
                                    "Vibrio"))
ResIsoCSTgn = mapply(new.function1, CSTgn$Rescom, CSTgn$NIsolates)
ResIsoCSTgn = round(ResIsoCSTgn, digits = 0)
AqAMR.tmp.CSTgn = cbind(CSTgn, ResIsoCSTgn)

#overall rate of CST resistance from gram neg surveys in the study
CSTgnmean = sum(AqAMR.tmp.CSTgn$ResIsoCSTgn, na.rm = T)/sum(AqAMR.tmp.CSTgn$NIsolates, na.rm = T)
#95CI
CICSTgn = as.data.frame(t(mapply(CI.function, CSTgnmean, sum(AqAMR.tmp.CSTgn$NIsolates, na.rm = T))))
CICSTgn = round(CICSTgn*100, digits = 0)
colnames(CICSTgn) = c("CILow","CIHigh")

#CST Vibrio
CSTvib = subset(CST, Pathogen %in% c("Vibrio"))

ResIsoCSTvib = mapply(new.function1, CSTvib$Rescom, CSTvib$NIsolates)
ResIsoCSTvib = round(ResIsoCSTvib, digits = 0)
AqAMR.tmp.CSTvib = cbind(CSTvib, ResIsoCSTvib)

#overall rate of CST resistance from vibrio surveys in the study
CSTvibmean = sum(AqAMR.tmp.CSTvib$ResIsoCSTvib, na.rm = T)/sum(AqAMR.tmp.CSTvib$NIsolates, na.rm = T)
#95CI
CICSTvib = as.data.frame(t(mapply(CI.function, CSTvibmean, sum(AqAMR.tmp.CSTvib$NIsolates, na.rm = T))))
CICSTvib = round(CICSTvib*100, digits = 0)
colnames(CICSTvib) = c("CILow","CIHigh")

#CST Aeromonas
CSTaer = subset(CST, Pathogen %in% c("Aeromonas"))

ResIsoCSTaer = mapply(new.function1, CSTaer$Rescom, CSTaer$NIsolates)
ResIsoCSTaer = round(ResIsoCSTaer, digits = 0)
AqAMR.tmp.CSTaer = cbind(CSTaer, ResIsoCSTaer)

#overall rate of CST resistance from aeromonas surveys in the study
CSTaermean = sum(AqAMR.tmp.CSTaer$ResIsoCSTaer, na.rm = T)/sum(AqAMR.tmp.CSTaer$NIsolates, na.rm = T)
#95CI
CICSTaer = as.data.frame(t(mapply(CI.function, CSTaermean, sum(AqAMR.tmp.CSTaer$NIsolates, na.rm = T))))
CICSTaer = round(CICSTaer*100, digits = 0)
colnames(CICSTaer) = c("CILow","CIHigh")

#CST in ecoli
CSTec = subset(CST, Pathogen %in% c("Ecoli"))

ResIsoCSTec = mapply(new.function1, CSTec$Rescom, CSTec$NIsolates)
ResIsoCSTec = round(ResIsoCSTec, digits = 0)
AqAMR.tmp.CSTec = cbind(CSTec, ResIsoCSTec)

#overall rate of CST resistance from ecoli surveys in the study
CSTecmean = sum(AqAMR.tmp.CSTec$ResIsoCSTec, na.rm = T)/sum(AqAMR.tmp.CSTec$NIsolates, na.rm = T)
#95CI
CICSTec = as.data.frame(t(mapply(CI.function, CSTecmean, sum(AqAMR.tmp.CSTec$NIsolates, na.rm = T))))
CICSTec = round(CICSTec*100, digits = 0)
colnames(CICSTec) = c("CILow","CIHigh")

#determine total NIsolates
CSTNIsolates = aggregate(NIsolates ~ DOI + XCoord + MidDate + Pathogen + SpeciesAg + CombRegion, data = AqAMR.tmp.CST, FUN = unique)
CSTNIsolates$NIsolates = sapply(CSTNIsolates$NIsolates, function(x) sum(unlist(x)))

#aggregate all resistant isolates and all NIsolates by speciesag, compound and region
CSTRes = aggregate(ResIsoCST ~ Compound + Pathogen + SpeciesAg + CombRegion, data = AqAMR.tmp.CST, FUN = sum)
CSTAll = aggregate(NIsolates ~ Compound + Pathogen + SpeciesAg + CombRegion, data = AqAMR.tmp.CST, FUN = sum)
#and divide ResIso/NIsolates to return true mean
CSTMean = round((CSTRes$ResIsoCST /CSTAll$NIsolates)*100, digits = 0)
CSTMeanDF = as.data.frame(cbind(CSTRes$Compound, CSTRes$Pathogen,CSTRes$SpeciesAg, CSTRes$CombRegion, CSTMean, CSTAll$NIsolates))
colnames(CSTMeanDF) = c("Compound","Pathogen","SpeciesAg","Region","Mean","NIsolates")
CSTMeanDF$Mean = as.numeric(as.character(CSTMeanDF$Mean))
CSTMeanDF$NIsolates = as.numeric(as.character(CSTMeanDF$NIsolates))

#Aeromonas in WSA from FW/Marine Fish
AER = subset(AqAMR, Pathogen %in% "Aeromonas" & CombRegion %in% "Western and Southern Asia"
             & SpeciesAg %in% c("Freshwater Fish", "Marine Fish"))

#ATM
AERatm = subset(AER, Compound == "ATM")
ResIsoAERatm = mapply(new.function1, AERatm$Rescom, AERatm$NIsolates)
ResIsoAERatm = round(ResIsoAERatm, digits = 0)
AqAMR.tmp.AERatm = cbind(AERatm, ResIsoAERatm)

#overall rate 
AERatmmean = sum(AqAMR.tmp.AERatm$ResIsoAERatm, na.rm = T)/sum(AqAMR.tmp.AERatm$NIsolates, na.rm = T)
#95CI
CIAERatm = as.data.frame(t(mapply(CI.function, AERatmmean, sum(AqAMR.tmp.AERatm$NIsolates, na.rm = T))))
CIAERatm = round(CIAERatm*100, digits = 0)
colnames(CIAERatm) = c("CILow","CIHigh")

#3rd/4th Gen Cep
AERcep = subset(AER, Compound %in% c("CRO","CAZ","CTX","FEP"))
ResIsoAERcep = mapply(new.function1, AERcep$Rescom, AERcep$NIsolates)
ResIsoAERcep = round(ResIsoAERcep, digits = 0)
AqAMR.tmp.AERcep = cbind(AERcep, ResIsoAERcep)

#overall rate 
AERcepmean = sum(AqAMR.tmp.AERcep$ResIsoAERcep, na.rm = T)/sum(AqAMR.tmp.AERcep$NIsolates, na.rm = T)
#95CI
CIAERcep = as.data.frame(t(mapply(CI.function, AERcepmean, sum(AqAMR.tmp.AERcep$NIsolates, na.rm = T))))
CIAERcep = round(CIAERcep*100, digits = 0)
colnames(CIAERcep) = c("CILow","CIHigh")
