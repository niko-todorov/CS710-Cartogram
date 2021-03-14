packages<-c("ggplot2","tibble","dplyr","tidyr","viridis","maps","mapproj")
install.packages(setdiff(packages, rownames(installed.packages())))
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(maps)
library(mapproj)



world_happiness <- read.csv(file = 'data/2019.csv')

# Score vs GDP
ggplot(world_happiness,aes(x=GDP.per.capita,y=Score)) + geom_point()

world_happiness$gdp <- cut(world_happiness$GDP.per.capita,breaks=seq(-.25,2,.5))  
ggplot(world_happiness,aes(x=gdp,y=Score,fill=gdp)) + 
  geom_boxplot() + 
  scale_fill_viridis_d()

world_happiness$gdp <- cut(world_happiness$GDP.per.capita,breaks=seq(-.25,2,.25))  
gdp_group <- world_happiness %>% group_by(gdp) %>% summarise(score=mean(Score))
ggplot(gdp_group,aes(x=gdp,y=score,fill=score)) + 
  geom_bar(stat="identity") + 
  scale_fill_viridis_c()



world_happiness <- read.csv(file = 'archive/2019.csv')
temp<-read.csv(file='Temperature.csv',strip.white = TRUE)
trim.leading <- function (x)  sub("^\\s+", "", x)
temp$Country <- trim.leading(temp$Country)
happiness_temp<-merge(temp,world_happiness,by.x="Country",by.y="Country.or.region")

#BOXPLOT
happiness_temp$temp_cat <- factor(cut(happiness_temp$Temperature,breaks=seq(-10,30,5)))
ggplot(happiness_temp,aes(x=temp_cat,y=Score,fill=temp_cat)) +
  geom_boxplot() + 
  scale_fill_viridis_d()

#BAR
temp_group <- happiness_temp %>% group_by(temp_cat) %>% summarise(score=mean(Score),count=n())
ggplot(temp_group,aes(x=temp_cat,fill=score)) +
  geom_bar()

ggplot(temp_group,aes(x=temp_cat,y=count,fill=score)) +
  geom_bar(stat="identity") + 
  scale_fill_viridis_c()



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