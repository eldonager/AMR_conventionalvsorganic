
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(patchwork)
library(scales)
library(wesanderson)
library(ggpubr)
library(gridExtra)


#Load data
main_data <- read_csv("conv.csv")

data1 <- main_data %>%
  group_by(country) %>%
  select("country",
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
         "country",
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






data3 <- data2 %>%select("country",
                         "doi",
                         "farm_type",
                         "host",
                         "antimicrobial_class",
                         "pathogen",
                         "percent_resistant", "no_isolates")%>%
  filter(pathogen != "Arcanobacterium pyogenes", pathogen != "Corynebacterium bovis",
         pathogen != "Environmental streptococci")


#changing percentage resistance into numeric class and rounding off  
data3$percent_resistant <- as.numeric(as.character
                                      (data3$percent_resistant))

data3$percent_resistant <- round(data3$percent_resistant, digits = 0)




usa<- data3
#create a function to return the number of resistant isolates
new.function1 <- function(x,y) {
  (x*y)/100}

ResIsousa <- mapply(new.function1, usa$percent_resistant, usa$no_isolates)
ResIsousa <- round(ResIsousa, digits = 0)
AMR.tmp.usa <- cbind(usa, ResIsousa)

#determine total NIsolates
usaNIsolates <- aggregate(no_isolates ~ doi + country + farm_type + 
                            antimicrobial_class + pathogen+host + percent_resistant+
                            ResIsousa, data = AMR.tmp.usa, FUN = sum)


usaNIsolates$no_isolates <- sapply(usaNIsolates$no_isolates, function(x) sum(unlist(x)))

#aggregate all resistant isolates and all NIsolates by speciesag, compound and region
usares <- aggregate(ResIsousa ~ antimicrobial_class  + farm_type+
                      host+country, 
                    data = AMR.tmp.usa, FUN = sum)
usaAll <- aggregate(no_isolates ~ antimicrobial_class  + farm_type+
                      host +country, 
                    data = AMR.tmp.usa, FUN = sum)
#and divide ResIso/NIsolates to return true mean
usaMean <- round((usares$ResIsousa /usaAll$no_isolates)*100, digits = 0)
usaMeanDF <- as.data.frame(cbind(usares$antimicrobial_class,
                                 usares$farm_type, usares$country,usares$host,
                                 usaMean, usaAll$no_isolates))
colnames(usaMeanDF) <- c("antimicrobial_class","farm_type","country","host",
                         "Mean","no_isolates")
usaMeanDF$Mean <- as.numeric(as.character(usaMeanDF$Mean))
usaMeanDF$no_isolates <- as.numeric(as.character(usaMeanDF$no_isolates))
#restrict display drug pairings where NIsolates > 10
usaMeanDF = usaMeanDF[which(usaMeanDF$no_isolates >= 10),]

# Compute the 95% CI of proportion where x = p_hat and y = n two tailed z = 1.96
CI.function <- function(x,y) {
  x + c(-1.96,1.96)*sqrt(x*(1-x)/y)}

#95% CI
CIusa<- as.data.frame(t(mapply(CI.function, usaMeanDF$Mean/100, usaMeanDF$no_isolates)))
usaMeanDF <-cbind(usaMeanDF, round(CIusa*100, digits = 0))
colnames(usaMeanDF) <- c("antimicrobial_class", "farm_type","country", "host",
                         "Mean","no_isolates","CILow","CIHigh")
usaMeanDF$CILow[usaMeanDF$CILow < 0] = 0
usaMeanDF$CIHigh[usaMeanDF$CIHigh >100] = 100

##remove data with less than 1% resistance
usaMeanDF <- usaMeanDF%>%
  filter(Mean>1)

country.data <- usaMeanDF

##Filter data from chicken species

chicken.data <- country.data %>%
  filter(host == "Chicken")




##usa bar plot
usa.1 <- chicken.data %>%
  filter(country == "United States of America")

usa.1<- usa.1[order(-usa.1$Mean), ]

usa.bar <- ggbarplot(usa.1, "antimicrobial_class", "Mean", 
                     fill = "farm_type", position = position_dodge(0.7),
                     subtitle = "USA, n= 2,883",
                     xlab = FALSE, ylab = FALSE,
                     legend.title = "Farm type",
                     font.subtitle = c(19))+
  rotate_x_text(45)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.7))+
  font("xy.text", size = 19)+
  font("legend.title",size = 19)+
  font("legend.text", size = 19)


usa.bar <- ggpar(usa.bar, ylim = c(0, 100))


##China bar plot

china.1 <- chicken.data %>%
  filter(country == "China")

china.1<- china.1[order(-china.1$Mean), ]

china.bar <- ggbarplot(china.1, "antimicrobial_class", "Mean", 
                       fill = "farm_type", position = position_dodge(0.7),
                       subtitle = "China, n= 185",
                       xlab = FALSE, ylab = FALSE,
                       legend.title = "Farm type",
                       font.subtitle = c(19))+
  rotate_x_text(60)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.7))+
  font("xy.text", size = 19)+
  font("legend.title",size = 19)+
  font("legend.text", size = 19)

china.bar <- ggpar(china.bar, ylim = c(0, 100))


uk.1 <- chicken.data %>%
  filter(country == "United Kingdom")

uk.1<- uk.1[order(-uk.1$Mean), ]

uk.bar <- ggbarplot(uk.1, "antimicrobial_class", "Mean", 
                    fill = "farm_type", position = position_dodge(0.7),
                    subtitle = "United Kingdom, n= 30",
                    xlab = FALSE, ylab = FALSE,
                    legend.title = "Farm type",
                    font.subtitle = c(19))+
  rotate_x_text(45)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.7))+
  font("xy.text", size = 19)+
  font("legend.title",size = 19)+
  font("legend.text", size = 19)

uk.bar <- ggpar(uk.bar, ylim = c(0, 100))

port.1 <- chicken.data %>%
  filter(country == "Portugal")

port.1<- port.1[order(-port.1$Mean), ]

port.bar <- ggbarplot(port.1, "antimicrobial_class", "Mean", 
                      fill = "farm_type", position = position_dodge(0.7),
                      subtitle = "Portugal, n= 43",
                      xlab = FALSE, ylab = FALSE,
                      legend.title = "Farm type",
                      font.subtitle = c(19))+
  rotate_x_text(45)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.7))+
  font("xy.text", size = 19)+
  font("legend.title",size = 19)+
  font("legend.text", size = 19)

port.bar <- ggpar(port.bar, ylim = c(0, 100))




##Putting all plots together using patchwork

country.plot <- (usa.bar | uk.bar)/
  (port.bar | china.bar)+
  plot_annotation(tag_levels = "A",
                  tag_prefix = "(",
                  tag_suffix = ")")+
  plot_layout(guides = "auto")&
  theme(plot.tag = element_text(face = NULL),
        legend.position = "top")


country.plot <- patchwork::patchworkGrob(country.plot)

yleft1 <- textGrob("Percentage resistance", rot = 90, gp = gpar(fontsize = 20))
bottom1 <- textGrob("Antimicrobial class", gp = gpar(fontsize = 20))
country.plot1 <- grid.arrange(country.plot, ncol = 1, nrow = 1,left = yleft1, bottom = bottom1)
country.plot1
