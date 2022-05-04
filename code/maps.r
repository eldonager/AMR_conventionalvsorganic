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
main_data <- read_csv("conv.csv")


#changing percentage resistance into numeric class and rounding off  
main_data$percent_resistant <- as.numeric(as.character
                                          (main_data$percent_resistant))

main_data$percent_resistant <- round(main_data$percent_resistant, digits = 2)

##sf_package
world <- ne_countries(type = 'countries', 
                      scale = 'small', 
                      returnclass = "sf") 

##Counting countries in the data set
main_data %>%
  count(country)%>%
  View()



### plotting AMR in conventional farms
  
conv_data <- main_data%>%
  group_by(country) %>%
  filter(farm_type == "Conventional") %>%
  select(country, iso_3, locality, 
         longitude, latitude,
         farm_type, antimicrobial,
         percent_resistant, pathogen) %>%
  na.omit()

##Summarizing the data
conv1 <- conv_data %>%
  group_by(country) %>%
  summarise(percent_resistant = mean(percent_resistant, na.rm = TRUE))


##joining the data set to the world data
conv2 <- world %>%
  filter(continent != "Antarctica", continent !="Greenland",
         continent !="Africa") %>%
  select(country = sovereignt, geometry, iso_a3) %>%
  left_join(conv1, by = "country")

##Adding number of studies to the dataset
conv3 <- main_data%>% 
  group_by(country, author) %>% 
  filter(farm_type == "Conventional") %>% 
  dplyr:: select(country, author) %>%
  distinct() %>% as.data.frame() # drop duplicates.
conv3

conv4 <- conv3 %>% 
  group_by(country) %>% 
  mutate(no_studies = n()) %>%
  dplyr:: select(country, no_studies) %>%
  distinct() %>% as.data.frame() # drop duplicates.
conv4

#Joining the data to add number of studies
conv5 <- conv2 %>%
  left_join(conv4, by = "country")
##plotting the map
#conv5 %>%
 # mapview(zcol = "percent_resistant",
          #col.regions = viridisLite::plasma)



##ploting
#conv6 <- world %>% 
  #filter(continent != "Antarctica", continent !="Greenland",
         #continent !="Africa", continent !="South America",
         #continent !="Australia") %>%
  #select(country = sovereignt, geometry) %>%
  #left_join(conv5, by = "country")

#plot(conv6[, c("percent_resistant")])


##dropping geometry before ploting
#conv5=st_drop_geometry(conv5)



conv_plot <- tm_shape(conv5) + 
  tm_polygons(col = "percent_resistant",  
              n = 4,
              projection = 3857,
              title = "% resistance (conventional)",
              palette = "plasma") + tm_style("white") + 
  tm_text("iso_a3", size = 0.45) 
 conv_plot 

##Plotting AMR in conventional farms
org_data <- main_data%>%
  group_by(country) %>%
  filter(farm_type == "Organic") %>%
  select(country, iso_3, locality, 
         longitude, latitude,
         farm_type, antimicrobial,
         percent_resistant, pathogen) %>%
  na.omit()

##Summarizing the data
org1 <- org_data %>%
  group_by(country) %>%
  summarise(percent_resistant = mean(percent_resistant, na.rm = TRUE))


##joining the data set to the world data
org2 <- world %>%
  filter(continent != "Antarctica", continent !="Greenland",
         continent !="Africa") %>%
  select(country = sovereignt, geometry, iso_a3) %>%
  left_join(org1, by = "country")

##Adding number of studies to the dataset
org3 <- main_data%>% 
  group_by(country, author) %>% 
  filter(farm_type == "Organic") %>% 
  dplyr:: select(country, author) %>%
  distinct() %>% as.data.frame() # drop duplicates.
org3

org4 <- org3 %>% 
  group_by(country) %>% 
  mutate(no_studies = n()) %>%
  dplyr:: select(country, no_studies) %>%
  distinct() %>% as.data.frame() # drop duplicates.
org4

#Joining the data to add number of studies
org5 <- org2 %>%
  left_join(org4, by = "country")


##plotting the map
#org5 %>%
#mapview(zcol = "percent_resistant",
#col.regions = viridisLite::plasma)

#dropping geometry before ploting
#org5=st_drop_geometry(org5)

##ploting
#org6 <- world %>% 
#filter(continent != "Antarctica", continent !="Greenland",
#continent !="Africa", continent !="South America",
#continent !="Australia") %>%
#select(country = sovereignt, geometry) %>%
#left_join(org5, by = "country")

#plot(org6[, c("percent_resistant")])





org_plot <- tm_shape(org5) + 
  tm_polygons(col = "percent_resistant",  
              n = 4,
              projection = 3857,
              title = "% resistance (Organic)",
              palette = "plasma") + tm_style("white") + 
  tm_text("iso_a3", size = 0.45)
org_plot


##Marging plots using patchwork

conv_org_plot <- tmap_mode("plot")
tmap_arrange(conv_plot, org_plot, widths = c(.25, .75))
tmap_mode(conv_org_plot)



