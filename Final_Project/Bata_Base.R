library(tidyverse)
library(gapminder)
library(stringr)

.libPaths()
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
myLocation <- c(lon = -95.3632715, lat = 29.7632836)
lmap("map", {center: 29.7632836 -95.3632715 zoom = 13})

