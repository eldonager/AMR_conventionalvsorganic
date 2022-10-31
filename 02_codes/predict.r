library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(patchwork)
library(nlme)
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

model1<- country.data %>%
  select(country, farm_type, antimicrobial_class, Mean)


df1 <-lme(Mean ~ farm_type,random= ~
      1|country/antimicrobial_class, data = model1)

summary(df1)

model.predict <- model1 %>%
  dplyr::mutate(predicted_mean = predict(df1, newdata = model1))


model.predict1 <- model.predict %>%
  select(farm_type, predicted_mean)


model.predict2 <- aggregate(predicted_mean~farm_type, data = model.predict1, 
                            FUN = "mean" )




p1 <-ggdotplot(model.predict2, "farm_type", "predicted_mean",
          color = "farm_type", fill = "farm_type",
          palette = c("#FC4E07", "#00AFBB"), xlab = "Farm type", ylab = "Mean resistance",
          size = 1, title = "prediction" )

p1


#ggsave(filename = "pred.pdf",p1)




