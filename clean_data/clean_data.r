
library(tidyverse)
library(janitor)
library(dplyr)

getwd()
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
  count(doi) %>%
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
  count(sample_type)%>%
  view()

raw_data %>%
  count(host)%>%
  view()

raw_data %>%
  count(antimicrobial)%>%
  view()

##cleaning host colunm

raw_data<- raw_data%>%
  mutate(
    host = 
      ifelse(host == "Cows and Calves", 
             "Cattle", host
      )
  )


raw_data<- raw_data%>%
  mutate(
    host = 
      ifelse(host == "Cows and Calves", 
             "Cattle", host
      )
  )




##Cleaning farm type colunm
raw_data<- raw_data%>%
  mutate(
    farm_type = 
      ifelse(farm_type == "Convetional", 
             "Conventional", farm_type
      )
  )


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
      ifelse(antimicrobial == "Chloramphenicol", 
             "Chloromphenicol", antimicrobial
      )
  )

raw_data<- raw_data%>%
  mutate(
    antimicrobial = 
      ifelse(antimicrobial == "Gentamicin", 
             "Gentamycin", antimicrobial
      )
  )

raw_data<- raw_data%>%
  mutate(
    antimicrobial = 
      ifelse(antimicrobial == "Quinupristin-dalfopristin", 
             "Quinopristin-dalfopristin", antimicrobial
      )
  )


##cleaning antimicrobial class colunm


raw_data<- raw_data%>%
  mutate(
    antimicrobial_class = 
      ifelse(antimicrobial_class == "1st_gen cephalosporn", 
             "1st_gen cephalosporin", antimicrobial_class
      )
  )


raw_data<- raw_data%>%
  mutate(
    antimicrobial_class = 
      ifelse(antimicrobial_class == "Glycopeptide", 
             "Glycopeptides", antimicrobial_class
      )
  )

raw_data<- raw_data%>%
  mutate(
    antimicrobial_class = 
      ifelse(antimicrobial_class == "Lincosamide", 
             "Lincosamides", antimicrobial_class
      )
  )

raw_data<- raw_data%>%
  mutate(
    antimicrobial_class = 
      ifelse(antimicrobial_class == "Polymyxin", 
             "Polymyxins", antimicrobial_class
      )
  )


raw_data<- raw_data%>%
  mutate(
    antimicrobial_class = 
      ifelse(antimicrobial_class == "Sulfisoxazole", 
             "Sulfamethaxazole", antimicrobial_class ))


raw_data<- raw_data%>%
  mutate(
    antimicrobial_compound = 
      ifelse(antimicrobial_compound == "PIT", 
             "PIP", antimicrobial_compound ))




raw_data %>%
  count(antimicrobial_class)%>%
  view()

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
