library(tidyverse)
library(gapminder)
library(stringr)


df <- read_csv('./R1_NABat_VettedObservations_NWRS2022.csv')

View(df)

names(df)


bats <- df %>% 
  filter(!is.na(CommonName)) %>% 
  mutate(CommonName = str_replace(CommonName, "Townsend\x92s big-eared bat", 
                                  "Townsend big-eared bat")) %>% 
  select(-"organization_name",-"species_list",-"frame", -"project_name")


nb <- #Placed the below code in the origianl bats df to keep my SHIT together
  bats %>% 
  filter(!is.na(CommonName)) %>% 
  mutate(CommonName = str_replace(CommonName, "Townsend\x92s big-eared bat", 
                                  "Townsend big-eared bat"))

unique(nb$reviewed)

#figure out what your quesiton is!!!!!!
# We want to see the population size changes over time of the bats, maybe use the animate

nb %>% 
  filter(!is.na(ObservationDate)) %>% 
  mutate(ObservationDate = as.factor(ObservationDate)) %>%
  ggplot(aes(x=ObservationDate))+
  geom_bar(aes(color = CommonName))+
  facet_wrap(~admin1, scales = "free")

names(nb)

wdf <- 
nb %>% 
  filter(!is.na(confirmed)) %>% 
  group_by(CommonName, latitude, longitude, RefugeName) %>% 
  summarise(sum = sum(reviewed))

names(wdf)

wdf %>% 
  ggplot(aes(x=latitude, y=longitude, fill = RefugeName))+
  geom_point()+
  geom_density2d(aes(x= sum))

library(leaflet)
library(sf)
myLocation <- c(lon = -95.3632715, lat = 29.7632836)



 m <- leaflet() %>% addTiles()

wdf_sf <- 
  wdf %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

leaflet() %>% 
  addTiles() %>% 
  addMarkers(data = ndf, popup = ~RefugeName)

plot1 <- 
  wdf %>% 
  ggplot(aes(x=longitude, y = latitude))+
  geom_jitter()

ggsave(filename = "./plot1.png", plot = plot1, width = 6, height = 6)

raster_img <- raster::raster("./plot1.png", ext = exten)

wdf %>% 
leaflet() %>% 
  addTiles() %>% 
  addProviderTiles(provider = "Esri.WorldImagery") %>% 
  #addMarkers( popup = ~RefugeName) %>% 
  addCircleMarkers(lat = ~latitude, lng = ~longitude, popup = ~CommonName)



wdf %>% 
  leaflet() %>% 
  addProviderTiles("Esri.WorldImagery") %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   label = ~paste("CommonName=", wdf$CommonName, 
                                  "Refuge=", wdf$RefugeName),
                   clusterOptions = markerClusterOptions(1)) 

# add in some species distribution maps for each species

library(ggspatial)
library(ggmap)

range(wdf$latitude)
range(wdf$longitude)

base = get_map(location=c(-125,40,-110,49), zoom=7, maptype="stamen_terrain")
ggmap(base)

?ggmap
?get_map
get_map(location = c(lat = 40,lon = -110))

?register_google
