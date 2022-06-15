

library(tidyverse)

main_data <- read_csv("conv.csv")

data1 <- main_data %>%
  select(antimicrobial,antimicrobial_compound, antimicrobial_class,
         who_classification) %>%
  unique() %>%
  arrange(antimicrobial)
  

###Exporting data to csv file in the conv folder
write.table(data1, file = "antimicrobials_table.csv", row.names = FALSE,
            sep = ",")
