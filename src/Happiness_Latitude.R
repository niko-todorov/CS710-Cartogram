library(ggplot2)
library(maps)
library(mapproj)
library(dplyr)
library(tidyr)

#Read in combined happiness data (2015-2019)
world_happiness <- read.csv(file = '../data/happinessdata.csv')

#Edit specific labels to merge with countries dataset
world_happiness$Country[world_happiness$Country=="United States"] <- "USA"
world_happiness$Country[world_happiness$Country=="United Kingdom"] <- "UK"

#Set happiness score of each country to mean of that country over all years
world_happiness<-world_happiness %>% group_by(Country) %>% summarise(Score=mean(Happiness.Score)) %>% drop_na()

#Read in country data
countries<-map_data("world")

#Merge countries and happiness data
happiness_map<-merge(countries,world_happiness,by.x="region",by.y="Country",all=TRUE)
happiness_map<-arrange(happiness_map,group,order)

#MAP
#Happiness by country map
ggplot(happiness_map,aes(x=long,y=lat,group=group,fill=Score)) + 
  coord_quickmap() +
  geom_polygon() + 
  ggtitle("Happiness by Country") +
  labs(fill="Happiness Score") +
  theme_void() + 
  scale_fill_viridis_c(option="cividis")

#POINT
#Get average absolute value of latitude (distance from equator) for each country with the average happiness score
latitude<-happiness_map %>% group_by(region) %>% summarise(score=mean(Score),lat=mean(abs(lat))) %>% drop_na()

#Score versus distance from equator 
ggplot(latitude,aes(x=lat,y=score,color=score)) +
  geom_point() + 
  ggtitle("Happiness Score vs Distance from Equator") +
  xlab(label="Distance from Equator by Latitude") +
  ylab(label="Happiness Score") +
  labs(color="Happiness Score") +
  theme_bw() +
  scale_color_viridis_c(option="cividis")

#BAR
#Factorize latitude into discrete categories
latitude$lat_cat<-factor(cut(latitude$lat,breaks=seq(0,70,5),labels=seq(0,65,5)))
lat_group <- latitude %>% group_by(lat_cat) %>% summarise(score=mean(score),count=n())

#Score versus distance from equator 
ggplot(lat_group,aes(x=lat_cat,y=score,fill=score)) +
  geom_bar(stat="identity") +
  ggtitle("Number of Countries vs Distance from Equator") +
  xlab(label="Distance from Equator by Latitude") +
  ylab(label="Number of Countries") +
  labs(fill="Happiness Score") +
  theme_classic() +
  scale_fill_viridis_c(option="cividis")

#Count versus distance from equator colored by score
ggplot(lat_group,aes(x=lat_cat,y=count,fill=score)) +
  geom_bar(stat="identity") + 
  ggtitle("Happiness vs Distance from Equator") +
  xlab(label="Distance from Equator by Latitude") +
  ylab(label="Happiness Score") +
  labs(fill="Happiness Score") +
  theme_classic() +
  scale_fill_viridis_c(option="cividis")
