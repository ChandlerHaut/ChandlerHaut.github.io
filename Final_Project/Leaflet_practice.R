library(leaflet)
library(shiny)
library(tidyverse)

my_map <- 
  leaflet() %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>%  #add a default base from a set 
setView(lat  = 46.9105, lng = -98.7084, zoom = 10) %>%   #set location to view based on map
addMarkers(lat  = 46.9105, lng = -98.7084, popup = "Hometown") #Add a popup 

str(quakes)
data(quakes)
quakes1 <- quakes[sample(nrow(quakes), 10)]
#add circle markers
quakes %>% 
  leaflet() %>% 
  addProviderTiles("Esri.WorldImagery") %>% 
  addMarkers(lng = ~long, lat = ~lat) %>%
  addCircleMarkers(lng = ~long, lat = ~lat)

# This is so you get the same result when running the code 
set.seed(122)


quakes_sample <- quakes[sample(nrow(quakes), 50), c("lat", "long", "depth", "mag")]

summary(quakes_sample)

quakes_sample$magrange <- cut(quakes_sample$mag,
                              breaks = c(4,5,6,7), right = FALSE,
                              labels = c("Light[4-5)", "Moderate[5-6)", "Strong[6-7)"))

quakes_sample %>% 
  leaflet() %>% 
  addProviderTiles("Esri.WorldImagery") %>% 
  #addMarkers(lng = ~long, lat = ~lat) %>%
  addCircleMarkers(lng = ~long, lat = ~lat)

pal <- colorFactor(palette = c("yellow", "red", "black"), domain = quakes_sample$magrange)


quakes_sample %>% 
  leaflet() %>% 
  addProviderTiles("Esri.WorldImagery") %>% 
  addCircleMarkers(lng = ~long, lat = ~lat,
                   color = ~pal(x = magrange), #using the palette we saved
                   label = ~paste("Magnitude=", quakes_sample$mag, 
                                 "Type=", quakes_sample$magrange))

#Lets add a legend


quakes_sample %>% 
  leaflet() %>% 
  addProviderTiles("Esri.WorldImagery") %>% 
  addCircleMarkers(lng = ~long, lat = ~lat,
                   color = ~pal(x = magrange), #using the palette we saved
                   label = ~paste("Magnitude=", quakes_sample$mag, 
                                  "Type=", quakes_sample$magrange)) %>% 
  addLegend(position = "bottomright", pal = pal, values = ~magrange, #Add a legend
            title = "Magnitude", opacity = 1) 

 #Clustering the markers, this is within the addCircleMarkers function

quakes_sample %>% 
  leaflet() %>% 
  addProviderTiles("Esri.WorldImagery") %>% 
  addCircleMarkers(lng = ~long, lat = ~lat,
                   color = ~pal(x = magrange), 
                   label = ~paste("Magnitude=", quakes_sample$mag, 
                                  "Type=", quakes_sample$magrange),
                   clusterOptions = markerClusterOptions(freezeAtZoom = 5)) 

#lets add some shapes baby
#https://www.youtube.com/watch?v=URWfykXKRZg&list=PL6wLL_RojB5y8uL3uuIMnJ6JoTIFywQ-r&index=13
# this url will be good to change the shapes of markers


leaflet(data = quakes) %>% 
  addTiles(group = "OSM") %>%
  #addProviderTiles(provider = "Stamen.Toner", group = "Toner") %>% 
  #addProviderTiles(provider = "Stamen.TonerLite", group = "Toner Lite") %>% 
  addMarkers(lng = ~long, lat = ~lat, group = "Markers") %>% 
  addCircleMarkers(lng = ~long, lat = ~lat, group = "Circle Markers") %>% 
  addLayersControl(baseGroups = c("OSM","Toner","Toner Lite"),
                   overlayGroups = c("Markers", "Circle Markers"),
                   options = layersControlOptions(collapsed = FALSE))

#the layers are not populating, the hell

#UPDATING the map using leafletProxy











