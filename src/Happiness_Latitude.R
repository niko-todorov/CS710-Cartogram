library(ggplot2)
library(maps)
library(mapproj)
library(dplyr)
library(tidyr)


world_happiness <- read.csv(file = '../data/happinessdata.csv')

world_happiness$Country[world_happiness$Country=="United States"] <- "USA"
world_happiness$Country[world_happiness$Country=="United Kingdom"] <- "UK"
world_happiness<-world_happiness %>% group_by(Country) %>% summarise(Score=mean(Happiness.Score)) %>% drop_na()

countries<-map_data("world")
happiness_map<-merge(countries,world_happiness,by.x="region",by.y="Country",all=TRUE)
happiness_map<-arrange(happiness_map,group,order)

#MAP
ggplot(happiness_map,aes(x=long,y=lat,group=group,fill=Score)) + 
  coord_quickmap() +
  geom_polygon() + 
  scale_fill_viridis_c()

#POINT
latitude<-happiness_map %>% group_by(region) %>% summarise(score=mean(Score),lat=mean(abs(lat))) %>% drop_na()
ggplot(latitude,aes(x=lat,y=score,color=score)) +
  geom_point() + 
  ggtitle("Happiness Score vs Distance from Equator") +
  xlab(label="Distance from Equator by Latitude") +
  ylab(label="Happiness Score") +
  labs(color="Happiness Score") +
  theme_bw() +
  scale_color_viridis_c()

#BAR
latitude$lat_cat<-factor(cut(latitude$lat,breaks=seq(0,70,5),labels=seq(0,65,5)))
lat_group <- latitude %>% group_by(lat_cat) %>% summarise(score=mean(score),count=n())

#Score versus distance from equator versus 
ggplot(lat_group,aes(x=lat_cat,y=score,fill=score)) +
  geom_bar(stat="identity") +
  ggtitle("Number of Countries vs Distance from Equator") +
  xlab(label="Distance from Equator by Latitude") +
  ylab(label="Number of Countries") +
  labs(fill="Happiness Score") +
  theme_classic() +
  scale_fill_viridis_c()

#Count versus distance from equator colored by score
ggplot(lat_group,aes(x=lat_cat,y=count,fill=score)) +
  geom_bar(stat="identity") + 
  ggtitle("Happiness vs Distance from Equator") +
  xlab(label="Distance from Equator by Latitude") +
  ylab(label="Happiness Score") +
  labs(fill="Happiness Score") +
  theme_classic() +
  scale_fill_viridis_c()
