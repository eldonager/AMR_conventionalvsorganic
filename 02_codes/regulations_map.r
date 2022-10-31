###loading packages
library(tidyverse)
library(dplyr)
library(tmap)
library(leaflet) 
library(sf) 
library(RColorBrewer)
library(leafsync) 
library(viridis)
library(mapview)
library(rnaturalearth)
library(patchwork)
library(ggforce)


##spherical geometry turned off
sf_use_s2(FALSE)
##loading data
main_data <- read_csv("regulations.csv")


main_data<- main_data%>%
  mutate(
    Country = 
      ifelse(Country == "Slovak Republic", 
             "Slovakia", Country))

main_data<- main_data%>%
  mutate(
    Status = 
      ifelse(Status == "Fully Implemented", 
             "Fully implemented", Status))

main_data<- main_data%>%
  mutate(
    Status = 
      ifelse(Status == "National regulations not fully implemented", 
             "Not fully implemented", Status))



##sf_package
world <- ne_countries(type = 'countries', 
                      scale = 'small', 
                      returnclass = "sf") 



### data wrangling
reg_data <- main_data%>%
  group_by(Country) %>%
  select(Continent,Country,Status) 



##joining the data set to the world data
reg_data1 <- world %>%
  filter(continent != "Antarctica", continent !="Greenland")%>%
  select(Country = geounit, geometry, iso_a3) %>%
  left_join(reg_data, by = "Country")


plot(reg_data1[, c("Status")])


##dropping geometry before ploting
#reg_data1=st_drop_geometry(reg_data1)



plot1 <- tm_shape(reg_data1) + 
  tm_polygons(col = "Status",  
              projection = 3857, midpoint = 0)+
  tm_text("iso_a3", size = "AREA")+
tm_style("white") + 
  tmap_options(check.and.fix = TRUE)




