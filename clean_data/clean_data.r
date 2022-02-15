
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
  count(antimicrobial)%>%
  view()

#Cleaning antimicrobials colunm

raw_data<- raw_data%>%
  mutate(
    antimicrobial = 
           ifelse(antimicrobial == "Amoxicillin–clavulanic acid", 
                  "Amoxicillin-clavulanic acid", antimicrobial
                  )
    )


raw_data<- raw_data%>%
  mutate(
    antimicrobial = 
      ifelse(antimicrobial == "Amoxicillinclavulanic acid", 
             "Amoxicillin-clavulanic acid", antimicrobial
      )
  )


raw_data<- raw_data%>%
  mutate(
    antimicrobial = 
      ifelse(antimicrobial == "Cefeclor", 
             "Cefaclor", antimicrobial
      )
  )



raw_data<- raw_data%>%
  mutate(
    antimicrobial = 
      ifelse(antimicrobial == "Cefalothin", 
             "Cephalothin", antimicrobial
      )
  )


raw_data<- raw_data%>%
  mutate(
    antimicrobial = 
      ifelse(antimicrobial == "Cephalotin", 
             "Cephalothin", antimicrobial
      )
  )



raw_data<- raw_data%>%
  mutate(
    antimicrobial = 
      ifelse(antimicrobial == "Sulfisoxazole", 
             "Sulfamethaxazole", antimicrobial
      )
  )


raw_data %>%
  count(antimicrobial)%>%
  view()

##cleaning antimicrobial compound
raw_data %>%
  count(antimicrobial)%>%
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
