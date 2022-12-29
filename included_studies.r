
library(tidyverse)

conv <- read_csv("conv.csv")

conv <- conv%>%
  select(doi, author, year, journal)%>%
  unique()

###Exporting data to csv file 
write.table(conv, file = "included_studies.csv", row.names = FALSE,
            sep = ",")


"https://www.r-project.org/logo/Rlogo.png",
"https://www.r-project.org/logo/Rlogo.png",
"https://www.r-project.org/logo/Rlogo.png",
"https://www.r-project.org/logo/Rlogo.png",
"https://www.r-project.org/logo/Rlogo.png"