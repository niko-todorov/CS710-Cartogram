library(ggplot2)
library(maps)
library(mapproj)
library(dplyr)
library(tidyr)


world_happiness <- read.csv(file = '../data/2019.csv')

world_happiness$Country.or.region[world_happiness$Country.or.region=="United States"] <- "USA"
world_happiness$Country.or.region[world_happiness$Country.or.region=="United Kingdom"] <- "UK"
countries<-map_data("world")
happiness_map<-merge(countries,world_happiness,by.x="region",by.y="Country.or.region",all=TRUE)
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
  scale_color_viridis_c()

#BAR
latitude$lat_cat<-factor(cut(latitude$lat,seq(0,70,5)))
lat_group <- latitude %>% group_by(lat_cat) %>% summarise(score=mean(score),count=n())

#Score versus distance from equator versus 
ggplot(lat_group,aes(x=lat_cat,y=score,fill=score)) +
  geom_bar(stat="identity") + 
  scale_fill_viridis_c()

#Count versus distance from equator colored by score
ggplot(lat_group,aes(x=lat_cat,y=count,fill=score)) +
  geom_bar(stat="identity") + 
  scale_fill_viridis_c()
