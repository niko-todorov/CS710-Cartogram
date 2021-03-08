library(ggplot2)
library(maps)
library(mapproj)
library(dplyr)
library(tidyr)


world_happiness <- read.csv(file = 'data/2019.csv')
world_happiness$Country.or.region[world_happiness$Country.or.region=="United States"] <- "USA"
world_happiness$Country.or.region[world_happiness$Country.or.region=="United Kingdom"] <- "UK"
countries<-map_data("world")
happiness_map<-merge(countries,world_happiness,by.x="region",by.y="Country.or.region",all=TRUE)
happiness_map<-arrange(happiness_map,group,order)

#MAP
ggplot(happiness_map,aes(x=long,y=lat,group=group,fill=Score)) + 
  coord_quickmap() +
  geom_polygon()

#POINT
latitude<-happiness_map %>% drop_na()
latitude$lat_cat<-factor(cut(latitude$lat,seq(-50,70,1)))
lat_group <- latitude %>% group_by(lat_cat) %>% summarise(score=mean(Score))
ggplot(lat_group,aes(x=lat_cat,y=score,color=score)) +
  geom_point()

#BAR
latitude$lat_cat<-factor(cut(latitude$lat,seq(-50,70,5)))
lat_group <- latitude %>% group_by(lat_cat) %>% summarise(score=mean(Score),count=n())
ggplot(lat_group,aes(x=lat_cat,fill=score)) +
  geom_bar()

ggplot(lat_group,aes(x=lat_cat,y=count,fill=score)) +
  geom_bar(stat="identity")
