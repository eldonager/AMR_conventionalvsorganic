

library(tidyverse)
library(gt)

#Load data

library(kableExtra)
library(tinytex)
library(AER)
library(wesanderson)
library(modelsummary)
library(flextable)

#Load data

main_data <- read_csv("conv.csv")

data1 <- main_data%>%
  mutate(
    continent =
      ifelse(continent == "Oceania", 
             "New Zealand", continent
      )
  )





data2 <- main_data %>%
  filter(pathogen == "Campylobacter" | pathogen == "E.coli" | pathogen == "Salmonella" |
           pathogen == "S.aureus" | pathogen == "Enterococcus") %>%
  select("antimicrobial_compound",
         "antimicrobial_class",
         "who_classification",
         "antimicrobial")


data3 <- data2 %>%
  select(antimicrobial,antimicrobial_compound, antimicrobial_class,
         who_classification) %>%
  filter(antimicrobial_compound != "AZT", antimicrobial_compound != "CBP",
         antimicrobial_compound != "CEX", antimicrobial_compound != "CFM",
         antimicrobial_compound != "CEQ", antimicrobial_compound != "DAP",
         antimicrobial_compound != "MOX", antimicrobial_compound != "MUP",
         antimicrobial_compound != "NET", antimicrobial_compound != "PIR",
         antimicrobial_compound != "TEC", antimicrobial_compound != "TEL",
         antimicrobial_compound != "TOB") %>%
  unique() %>%
  arrange(antimicrobial)

data4 <- with(data3,  data3[order(who_classification) , ])


my_table <- flextable(data4) %>% 
  autofit()

#aligning all colunms to center except antimicrobial
my_table <- my_table %>% 
  flextable::align(align = "center", j = c(2:4), part = "all") 

my_table <-  my_table %>%  
  fontsize(i = 1, size = 12, part = "header") %>%   # adjust font size of header
  bold(i = 1, bold = TRUE, part = "header") %>%
  set_header_labels(         # Rename the columns in original header row
    antimicrobial = "Antimicrobial", 
    antimicrobial_compound = "Antimicrobial compound",                  
    antimicrobial_class = "Antimicrobial class",
    who_classification = "WHO classification")


##colouring table     
my_table <- my_table %>% 
  bg(., i= ~ who_classification == "Critically important", part = "body", bg = "#CCCCFF") %>%
  bg(., i= ~ who_classification == "Highly important", part = "body", bg = "#00A08A")%>%
  bg(., i= ~ who_classification == "Important", part = "body", bg = "#F2AD00")%>%
  bg(., i= ~ who_classification == "Not used in humans", part = "body", bg = "#5BBCD6")%>%
  bg(., i= ~ antimicrobial == "Novobiocin", part = "body", bg = "#F98400")%>%
  bg(., i= ~ antimicrobial == "Penicillin-novobiocin", part = "body", bg = "#F98400")%>%
  autofit() %>% 
  fit_to_width(7.5)
  
  




save_as_image(my_table, path = "drugs.pdf")

