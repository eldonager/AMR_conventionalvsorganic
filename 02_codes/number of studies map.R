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


main_data <- read_csv("conv.csv")

##spherical geometry turned off
sf_use_s2(FALSE)
##sf_package
world <- ne_countries(type = 'countries', 
                      scale = 'small', 
                      returnclass = "sf") 

p1 <- main_data%>% 
  group_by(country, doi) %>% 
  dplyr:: select(country, author) %>%
  distinct() %>% as.data.frame() # drop duplicates.
p1


p2 <- p1 %>% 
  group_by(country) %>% 
  mutate(no_studies = n()) %>%
  dplyr:: select(country, no_studies) %>%
  distinct() %>% as.data.frame() # drop duplicates.
p2

p3 <- world %>%
  filter(continent != "Antarctica", continent !="Greenland",
         continent !="Africa") %>%
  select(country = sovereignt, geometry, iso_a3)



p4 <- p3 %>%
  left_join(p2, by = "country")


p5 <- tm_shape(p4) + 
  tm_polygons(col = "no_studies",  
              n = 5,
              projection = 3857,
              title = "number of studies",
              pallete = "Blues") + tm_style("gray")+
  tm_text("iso_a3", size = 0.45)
  tmap_options(check.and.fix = TRUE)
p5