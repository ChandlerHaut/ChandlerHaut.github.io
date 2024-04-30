# My Final Project is going to be looking at bat acoustic data for the Pacific NorWest from the BLM.
# I'm doing an interactive map that will filter by species to show a visualization over the Refuges.
# I am also making species distribution maps for each species that has been recorded. 
library(tidyverse)
library(gapminder)
library(stringr)
library(leaflet)
library(sf)
library(shiny)
library(leaflet.extras)
library(htmltools)
library(ggmap)
library(knitr)
library(rmarkdown)
library(sp)

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

wdf <- 
nb %>% 
  filter(!is.na(confirmed)) %>% 
  group_by(CommonName, latitude, longitude, RefugeName) %>% 
  summarise(sum = sum(reviewed))


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




  


#Sweet mother of God it works

wdf %>% 
  leaflet() %>% 
  addProviderTiles("Esri.WorldImagery") %>% 
  addCircleMarkers(radius = 10, label = ~paste0("Common Name =", htmlEscape(CommonName),
                                              "Refuge =",htmlEscape(RefugeName)),
                   color = "blue", group = wdf$CommonName) %>% 
  addLayersControl(baseGroups = wdf$CommonName)
  

wdf %>% 
  leaflet() %>% 
  addProviderTiles("Esri.WorldImagery") %>% 
  addGeoJSON()

?addPolygons
# add in some species distribution maps for each species




register_google("")


p <- get_map(location = c(lat = 40.44041496666531, lon = -112.01505661629491), 
                           zoom = 12, maptype = "terrain")

  
ggmap(p)



bbb <- 
  wdf %>% 
  filter(CommonName == "Big brown bat")

ggmap(p) +
  geom_density_2d(data = bbb, aes(y=latitude, x=longitude), h = 2)+
  ggtitle("Big Brown Bat")

bftb <- 
  wdf %>% 
  filter(CommonName == "Brazilian free-tailed bat")

ggmap(p) +
  geom_density_2d(data = bftb, aes(y=latitude, x=longitude), h = 2)+
  ggtitle("Brazilian Free-tailed Bat")

cm <- 
  wdf %>% 
  filter(CommonName == "California myotis")

ggmap(p) +
  geom_density_2d(data = cm, aes(y=latitude, x=longitude), h = 2)+
  ggtitle("California myotis")

cb <- 
  wdf %>% 
  filter(CommonName == "Canyon bat") 

ggmap(p) +
  geom_density_2d(data = cb, aes(y=latitude, x=longitude), h = 2)+
  ggtitle("Canyon bat")

fm <- 
  wdf %>% 
  filter(CommonName == "Fringed myotis")

ggmap(p) +
  geom_density_2d(data = fm, aes(y=latitude, x=longitude), h = 2)+
  ggtitle("Fringed myotis")

hb <- 
  wdf %>% 
  filter(CommonName == "Hoary bat")

ggmap(p) +
  geom_density_2d(data = hb, aes(y=latitude, x=longitude), h = 2)+
  ggtitle("Hoary Bat")

lbm <- 
  wdf %>% 
  filter(CommonName == "Little brown myotis")

ggmap(p) +
  geom_density_2d(data = lbm, aes(y=latitude, x=longitude), h = 2)+
  ggtitle("Little brown myotis")

lem <- 
  wdf %>% 
  filter(CommonName == "Long-eared myotis") 

ggmap(p) +
  geom_density_2d(data = lem, aes(y=latitude, x=longitude), h = 2)+
  ggtitle("Long-eared myotis")

llm <- 
  wdf %>% 
  filter(CommonName == "Long-legged myotis") 

ggmap(p) +
  geom_density_2d(data = llm, aes(y=latitude, x=longitude), h = 2)+
  ggtitle("Long-legged Myotis")

pb <- 
  wdf %>% 
  filter(CommonName == "Pallid bat") 

ggmap(p) +
  geom_density_2d(data = pb, aes(y=latitude, x=longitude), h = 2)+
  ggtitle("Pallid Bat")

shb <- 
  wdf %>% 
  filter(CommonName == "Silver-haired bat") 

ggmap(p) +
  geom_density_2d(data = shb, aes(y=latitude, x=longitude), h = 2)+
  ggtitle("Silver-haired bat")

sb <- 
  wdf %>% 
  filter(CommonName == "Spotted bat") 

ggmap(p) +
  geom_density_2d(data = sb, aes(y=latitude, x=longitude), h = 2)+
  ggtitle("Spotted Bat")

tbeb<- 
  wdf %>% 
  filter(CommonName == "Townsend big-eared bat") 

ggmap(p) +
  geom_density_2d(data = tbeb, aes(y=latitude, x=longitude), h = 2)+
  ggtitle("Townsend Big-eared Bat")

wrb <- 
  wdf %>% 
  filter(CommonName == "Western red bat") 

ggmap(p) +
  geom_density_2d(data = wrb, aes(y=latitude, x=longitude), h = 2)+
  ggtitle("Western Red Bat")

wsfm<- 
  wdf %>% 
  filter(CommonName == "Western small-footed myotis") 

ggmap(p) +
  geom_density_2d(data = wsfm, aes(y=latitude, x=longitude), h = 2)+
  ggtitle("Western Small-footed Myotis")

ym <- 
  wdf %>% 
  filter(CommonName == "Yuma myotis") 

ggmap(p) +
  geom_density_2d(data = ym, aes(y=latitude, x=longitude), h = 2)+
  ggtitle("Yuma Myotis")



ggmap(p) +
  geom_density_2d(data = bbb, aes(y=latitude, x=longitude, color = "red"), h = 2)


ggmap(p) +
  geom_density_2d(data = lbm, aes(y=latitude, x=longitude), h = 2)+
  ggtitle("yee")
  









######### Capstone Crap

p <- get_map(location = c(lat = 40.44041496666531, lon = -112.01505661629491), 
             zoom = 12, maptype = "hybrid")


ggmap(p)

md <- read_csv("new_mock_data.csv")

data <- data.frame(
  location = c("South Mountain" ,"West Canyon" ,   "Medic Hill"  ,   "Cedar Point"   , "Tickville"),
  lat = c( 40.45831, 40.41274, 40.39375, 40.40603, 40.42912)),
  lon = c( -112.03675,-112.09848,-112.00709,-111.97177,-112.03567))
)


fdf <- merge(md, data, by = "location")

dm <- 
  fdf %>% 
  filter(species == "Deer Mouse")
  
cw_map <- ggmap(p) +
  geom_density_2d(data = dm, aes(y = lat, x = lon), adjust = 1/4)+
  ggtitle("Deer Mouse Distribution")

ggsave("DMD_map.png", plot = cw_map, width = 10, height = 8, units = "in", dpi = 300)
