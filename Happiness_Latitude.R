library(ggplot2)
library(maps)
library(mapproj)
library(dplyr)
library(tidyr)

#setwd("/Users/tobychappell/Documents/CS_Courses/CS_710/Workshop1Group3")
world_happiness <- read.csv(file = 'data/2019.csv')

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
latitude<-happiness_map %>% group_by(region) %>% summarise(score=mean(Score),lat=mean(lat)) %>% drop_na()
latitude$lat_cat<-factor(cut(latitude$lat,seq(-60,70,1)))
lat_group <- latitude %>% group_by(lat_cat) %>% summarise(score=mean(score))
ggplot(lat_group,aes(x=lat_cat,y=score,color=score)) +
  geom_point() + 
  scale_color_viridis_c()

#BAR
latitude$lat_cat<-factor(cut(latitude$lat,seq(-60,70,10)))
lat_group <- latitude %>% group_by(lat_cat) %>% summarise(score=mean(score),count=n())
ggplot(lat_group,aes(x=lat_cat,fill=score)) +
  geom_bar() + 
  scale_fill_viridis_c()

ggplot(lat_group,aes(x=lat_cat,y=count,fill=score)) +
  geom_bar(stat="identity") + 
  scale_fill_viridis_c()
