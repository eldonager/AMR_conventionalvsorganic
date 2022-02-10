
library(tidyverse)
library(janitor)
library(dplyr)

#Load data
raw_data <- read_csv("conv.csv", na = "empty")

colnames(raw_data)

##Cleaning variable names using the clean names function

raw_data <- clean_names(raw_data)

View(raw_data)

##removing empty columns and rows
raw_data <- raw_data %>% 
  remove_empty(
    c("rows", "cols"
      )
    )


View(raw_data)

###cleaning the country column

raw_data %>% tabyl(country)


raw_data %>%
  count(country)%>%
  View()


raw_data %>%
  count(iso_3)%>%
  View()

raw_data %>%
  count(locality)%>%
  view()

raw_data %>%
  count(farm_type)%>%
  view()

raw_data %>%
  count(Sample_type)%>%
  view()

raw_data %>%
  count(Host)%>%
  view()

raw_data %>%
  count(Antimicrobial)%>%
  view()

#Cleaning antimicrobials colunm

raw_data<- raw_data%>%
  mutate(
    Antimicrobial = 
           ifelse(Antimicrobial == "Amoxicillin–clavulanic acid", 
                  "Amoxicillin-clavulanic acid", Antimicrobial
                  )
    )


raw_data<- raw_data%>%
  mutate(
    Antimicrobial = 
      ifelse(Antimicrobial == "Cefeclor", 
             "Cefaclor", Antimicrobial
      )
  )



raw_data<- raw_data%>%
  mutate(
    Antimicrobial = 
      ifelse(Antimicrobial == "Cefalothin", 
             "Cephalothin", Antimicrobial
      )
  )


raw_data<- raw_data%>%
  mutate(
    Antimicrobial = 
      ifelse(Antimicrobial == "Cephalotin", 
             "Cephalothin", Antimicrobial
      )
  )



raw_data<- raw_data%>%
  mutate(
    Antimicrobial = 
      ifelse(Antimicrobial == "Sulfisoxazole", 
             "Sulfamethaxazole", Antimicrobial
      )
  )


raw_data %>%
  count(Antimicrobial)%>%
  view()

##cleaning antimicrobial compound
raw_data %>%
  count(`Antimicrobial compound`)%>%
  view()


raw_data %>%
  count(percent_resistant)%>%
  view()

raw_data %>%
  count(percent_intermediate)%>%
  view()

raw_data %>%
  count(percent_susceptible)%>%
  view()

raw_data %>%
  count(pathogen)%>%
  view()

raw_data %>%
  count(strain)%>%
  view()

###Exporting data to csv file in the conv folder
write.table(raw_data, file = "conv.csv", row.names = FALSE,
            sep = ",")
