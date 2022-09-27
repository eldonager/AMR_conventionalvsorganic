

library(tidyverse)


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
  filter(pathogen == "Campylobacter" | pathogen == "E.coli" | pathogen == "Salmonella" |
           pathogen == "S.aureus" | pathogen == "Enterococcus") %>%
  select("antimicrobial_compound",
         "antimicrobial_class",
         "who_classification",
         "antimicrobial")


data2 <- data1 %>%
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




###Exporting data to csv file in the conv folder
write.table(data1, file = "antimicrobials_table.csv", row.names = FALSE,
            sep = ",")
