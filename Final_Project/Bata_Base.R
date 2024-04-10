library(tidyverse)
library(gapminder)
library(stringr)
library(leaflet)
library(sf)
library(shiny)
library(leaflet.extras)
library(htmltools)

df <- read_csv('./R1_NABat_VettedObservations_NWRS2022.csv')

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

#figure out what your quesiton is!!!!!!
# We want to see the population size changes over time of the bats, maybe use the animate

nb %>% 
  filter(!is.na(ObservationDate)) %>% 
  mutate(ObservationDate = as.factor(ObservationDate)) %>%
  ggplot(aes(x=ObservationDate))+
  geom_bar(aes(color = CommonName))+
  facet_wrap(~admin1, scales = "free")



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



wdf_sf <- 
  wdf %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)




ggsave(filename = "./plot1.png", plot = plot1, width = 6, height = 6)

raster_img <- raster::raster("./plot1.png", ext = exten)

wdf %>% 
leaflet() %>% 
  addTiles() %>% 
  addProviderTiles(provider = "Esri.WorldImagery") %>% 
  #addMarkers( popup = ~RefugeName) %>% 
  addCircleMarkers(lat = ~latitude, lng = ~longitude, popup = ~CommonName)

##### Current Working Map

wdf %>% 
  leaflet() %>% 
  addProviderTiles("Esri.WorldImagery") %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   label = ~paste0("CommonName=", wdf$CommonName, 
                                  "Refuge=", wdf$RefugeName,
                   #clusterOptions = markerClusterOptions(1),
                   group = ~CommonName)) %>%  # Group by CommonName
  addLayersControl( overlayGroups = c("Big brown bat","Brazilian free-tailed bat","California myotis","Canyon bat",
                                  "Fringed myotis","Hoary bat","Little brown myotis","Long-eared myotis","Long-legged myotis",         
                                  "Pallid bat","Silver-haired bat","Spotted bat","Townsend big-eared bat",
                                  "Western red bat","Western small-footed myotis","Yuma myotis" ),
                   options = layersControlOptions(collapsed = FALSE))



bats <- c("Big brown bat","Brazilian free-tailed bat","California myotis","Canyon bat",
"Fringed myotis","Hoary bat","Little brown myotis","Long-eared myotis","Long-legged myotis",         
"Pallid bat","Silver-haired bat","Spotted bat","Townsend big-eared bat",
"Western red bat","Western small-footed myotis","Yuma myotis" )
  
# add in some species distribution maps for each species

#Sweet mother of God it works

wdf %>% 
  leaflet() %>% 
  addProviderTiles("Esri.WorldImagery") %>% 
  addCircleMarkers(radius = 10, label = ~paste0("Common Name =", htmlEscape(CommonName),
                                              
                                              "Refuge =",htmlEscape(RefugeName)),
                   color = "blue", group = wdf$CommonName) %>% 
  addLayersControl(overlayGroups = wdf$CommonName)

?layersControlOptions










