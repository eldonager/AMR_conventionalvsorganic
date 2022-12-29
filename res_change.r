library(tidyverse)
library(dplyr)


#Load data
main_data <- read_csv("mean.data.csv")

data1 <- main_data %>%
  select("host",
         "farm_type",
         
         "sampling_end_date", "mean")






data3 <-aggregate(mean~host+farm_type, data1, mean)%>%
  arrange(host,farm_type)




#changing percentage resistance into numeric class and rounding off  
data3$mean <- as.numeric(as.character(data3$mean))

data3$mean <- round(data3$mean, digits = 0)

data3<- data3[which(data3$mean>= 1),]



data4 <- data3 %>%
  pivot_wider(names_from = farm_type, values_from = mean)


#Adding a column with percentage change values
data5<- data4%>%
  dplyr::mutate(parcent_resistance_change = (Organic - Conventional)/Conventional*100)





