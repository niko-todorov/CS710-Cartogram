library(ggplot2)
library(maps)
library(mapproj)
library(dplyr)
library(tidyr)
library(gridExtra)

#Read in combined happiness data (2015-2019)
data <- read.csv(file = 'data/pop-happiness.csv')

#Edit specific labels to merge with countries data set
data$Country[data$Country=="United States"] <- "USA"
data$Country[data$Country=="United Kingdom"] <- "UK"

#Set happiness score of each country to mean of that country over all years
world_happiness<-data %>% group_by(Country,Region) %>% summarise(Score=mean(HappinessScore),Population=mean(Population))

#Read in country data
countries<-map_data("world")

#Merge countries and happiness data
happiness_map<-merge(countries,world_happiness,by.x="region",by.y="Country",all=TRUE)
happiness_map<-arrange(happiness_map,group,order)

#MAP
#Happiness by country map
map_plot<-ggplot(happiness_map,aes(x=long,y=lat,group=group,fill=Score)) + 
  coord_quickmap() +
  geom_polygon() + 
  ggtitle("Happiness by Country (2015-2019)") +
  labs(fill="Happiness Score") +
  theme_void() + 
  theme(legend.position='none') +
  scale_fill_viridis_c(option="magma")
map_plot

min_pop<-min((happiness_map %>% drop_na)$Population)
#Happiness by country map, brightness by population
happiness_map$Population[is.na(happiness_map$Population)] <- min_pop-1
happiness_map$pop_cat<-factor(cut(1/log10(happiness_map$Population),breaks=seq(0,1,.04)),labels=c("Large","Medium","Small"))
map_pop_plot<-ggplot(happiness_map) +
  coord_quickmap() +
  geom_polygon(aes(x=long, y=lat, group=group, fill=Score)) +
  geom_polygon(aes(x=long, y=lat, group=group, alpha=pop_cat)) +
  ggtitle("Happiness by Country (2015-2019)") +
  labs(fill="Happiness Score",alpha="Population") +
  theme_void() + 
  scale_fill_viridis_c(option="magma") +
  scale_alpha_manual(values = c(0.1,0.4,.7))
map_pop_plot

#Get average absolute value of latitude (distance from equator) for each country with the average happiness score
latitude<-happiness_map %>% group_by(region) %>% summarise(score=mean(Score),lat=mean(abs(lat)),pop=mean(Population)) %>% drop_na()
min_score<-min(latitude$score)
max_score<-max(latitude$score)

latitude$lat_cat<-factor(cut(latitude$lat,breaks=seq(0,70,5),labels=seq(0,65,5)))
latitude<-latitude %>% group_by(lat_cat) %>% mutate(mean_score=mean(score))
box_plot <- ggplot(latitude,aes(x=score,y=lat_cat,fill=mean_score)) +
  geom_boxplot() +
  ggtitle("Happiness vs Distance from Equator") +
  xlab(label="Happiness Score") +
  ylab(label="Distance from Equator by Latitude") +
  labs(fill="Happiness Score") +
  theme_classic() + 
  theme(legend.position='bottom') +
  scale_fill_viridis_c(option="magma",limits=c(min_score,max_score)) 
box_plot
lay <- rbind(c(1,1,1,1,1),
             c(NA,2,2,2,NA))
grid.arrange(map_plot, box_plot, layout_matrix = lay)

latitude$pop_cat<-factor(cut(1/log10(latitude$pop),breaks=seq(0,1,.01)))
popcat<-latitude %>% group_by(lat_cat) %>% count(lat_cat, pop_cat_max=pop_cat) %>% slice(which.max(n))
latitude<-merge(latitude,popcat,by.x="lat_cat",by.y="lat_cat")

box_pop_plot <- ggplot(latitude) +
  geom_boxplot(aes(x=score,y=lat_cat,fill=mean_score)) +
  geom_boxplot(aes(x=score,y=lat_cat,alpha=pop_cat_max),fill="black") +
  ggtitle("Happiness vs Distance from Equator") +
  xlab(label="Happiness Score") +
  ylab(label="Distance from Equator by Latitude") +
  labs(fill="Happiness Score",alpha="Population") +
  theme_classic() +
  scale_fill_viridis_c(option="magma",limits=c(min_score,max_score)) +
  scale_alpha_manual(values = c(0.1,0.4,.7))
box_pop_plot

lay <- rbind(c(1,1,1,1,1),
             c(NA,2,2,2,NA))
grid.arrange(map_pop_plot, box_pop_plot, layout_matrix = lay)
