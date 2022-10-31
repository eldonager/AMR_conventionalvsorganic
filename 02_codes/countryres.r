library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(patchwork)

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
                              antimicrobial_class + pathogen + percent_resistant+
                              ResIsousa, data = AMR.tmp.usa, FUN = sum)


usaNIsolates$no_isolates <- sapply(usaNIsolates$no_isolates, function(x) sum(unlist(x)))

#aggregate all resistant isolates and all NIsolates by speciesag, compound and region
usares <- aggregate(ResIsousa ~ antimicrobial_class  + farm_type+
                        country, 
                      data = AMR.tmp.usa, FUN = sum)
usaAll <- aggregate(no_isolates ~ antimicrobial_class  + farm_type+
                        country, 
                      data = AMR.tmp.usa, FUN = sum)
#and divide ResIso/NIsolates to return true mean
usaMean <- round((usares$ResIsousa /usaAll$no_isolates)*100, digits = 0)
usaMeanDF <- as.data.frame(cbind(usares$antimicrobial_class,
                                   usares$farm_type, usares$country,
                                  usaMean, usaAll$no_isolates))
colnames(usaMeanDF) <- c("antimicrobial_class","farm_type","country",
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
colnames(usaMeanDF) <- c("antimicrobial_class", "farm_type","country",
                           "Mean","no_isolates","CILow","CIHigh")
usaMeanDF$CILow[usaMeanDF$CILow < 0] = 0
usaMeanDF$CIHigh[usaMeanDF$CIHigh >100] = 100

##remove data with less than 1% resistance
usaMeanDF <- usaMeanDF%>%
  filter(Mean>1)

country.data <- usaMeanDF

##Mean percentage resistance in each farm type
df1 <- aggregate(Mean~farm_type, data = country.data, FUN = mean)





##usa bar plot
usa.1 <- usaMeanDF %>%
  filter(country == "United States of America")

usa.1<- usa.1[order(-usa.1$Mean), ]

usa.bar <- ggbarplot(usa.1, "antimicrobial_class", "Mean", 
                               fill = "farm_type", position = position_dodge(0.7),
                               subtitle = "USA, n= 22,122",
                               xlab = FALSE, ylab = FALSE,
                               legend.title = "Farm type",
                               font.subtitle = "italic")+
  rotate_x_text(45)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.7))




##Denmark plot

den.1 <- usaMeanDF %>%
  filter(country == "Denmark")

den.1<- den.1[order(-den.1$Mean), ]

den.bar <- ggbarplot(den.1, "antimicrobial_class", "Mean", 
                     fill = "farm_type", position = position_dodge(0.7),
                     subtitle = "Denmark, n= 468 ",
                     xlab = FALSE, ylab = FALSE,
                     legend.title = "Farm type",
                     font.subtitle = "italic")+
  rotate_x_text(45)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.7))





##Sweden bar plot

swe.1 <- usaMeanDF %>%
  filter(country == "Sweden")

swe.1<- swe.1[order(-swe.1$Mean), ]

sweden.bar <- ggbarplot(swe.1, "antimicrobial_class", "Mean", 
                     fill = "farm_type", position = position_dodge(0.7),
                     subtitle = "Sweden, n= 28,259",
                     xlab = FALSE, ylab = FALSE,
                     legend.title = "Farm type",
                     font.subtitle = "italic")+
  rotate_x_text(45)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.7))





##South kores bar plot

s.k1 <- usaMeanDF %>%
  filter(country == "South Korea")

s.k1<- s.k1[order(-s.k1$Mean), ]

Southkorea.bar <- ggbarplot(s.k1, "antimicrobial_class", "Mean", 
                        fill = "farm_type", position = position_dodge(0.7),
                        subtitle = "South Korea, n= 2,382",
                        xlab = FALSE, ylab = FALSE,
                        legend.title = "Farm type",
                        font.subtitle = "italic")+
  rotate_x_text(60)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.7))




##China bar plot

china.1 <- usaMeanDF %>%
  filter(country == "China")

china.1<- china.1[order(-china.1$Mean), ]

china.bar <- ggbarplot(china.1, "antimicrobial_class", "Mean", 
                            fill = "farm_type", position = position_dodge(0.7),
                            subtitle = "China, n= 3,544",
                            xlab = FALSE, ylab = FALSE,
                            legend.title = "Farm type",
                            font.subtitle = "italic")+
  rotate_x_text(60)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.7))






##Germany bar plot

ger.1 <- usaMeanDF %>%
  filter(country == "Germany")

ger.1<- ger.1[order(-ger.1$Mean), ]

germany.bar <- ggbarplot(ger.1, "antimicrobial_class", "Mean", 
                       fill = "farm_type", position = position_dodge(0.7),
                       subtitle = "Germany, n= 17,808",
                       xlab = FALSE, ylab = FALSE,
                       legend.title = "Farm type",
                       font.subtitle = "italic")+
  rotate_x_text(45)+
  geom_linerange(aes(group = farm_type, ymax = CIHigh, ymin = CILow),
                 position = position_dodge(width = 0.7))








##Putting all plots together using patchwork

country.plot <- (usa.bar | germany.bar | Southkorea.bar) / 
  (sweden.bar | den.bar | china.bar)+
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")")+
  plot_layout(guides = "collect")&
  theme(plot.tag = element_text(face = "italic"),
        legend.position = "top")


country.plot

#ggsave(filename = "country.png",country.plot)
