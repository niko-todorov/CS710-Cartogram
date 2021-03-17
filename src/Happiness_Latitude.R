library(ggplot2)
library(maps)
library(mapproj)
library(dplyr)
library(tidyr)
library(gridExtra)

#Read in combined happiness data (2015-2019)
world_happiness <- read.csv(file = 'data/happinessdata.csv')

#Read in population data
world_population <- read.csv(file='data/Population.csv') # PopulationMod.csv
world_happiness$Country[world_happiness$Country=="Congo (Brazzaville)"] <- "Republic of Congo"
world_happiness$Country[world_happiness$Country=="Congo (Kinshasa)"] <- "Democratic Republic of the Congo"

world_population$X2015_2019 <- (world_population$X2015+world_population$X2016+world_population$X2017+world_population$X2018+world_population$X2019)/5
world_population <- world_population %>% select(Country.Name,X2015_2019)
world_happiness<-merge(world_population,world_happiness,by.x="Country.Name",by.y="Country")

#Edit specific labels to merge with countries data set
world_happiness$Country[world_happiness$Country=="United States"] <- "USA"
world_happiness$Country[world_happiness$Country=="United Kingdom"] <- "UK"

#Set happiness score of each country to mean of that country over all years
world_happiness<-world_happiness %>% group_by(Country) %>% summarise(Score=mean(HappinessScore),Population=mean(X2015_2019)) %>% drop_na()

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
  scale_fill_viridis_c(option="magma")

#Population by country map
ggplot(happiness_map,aes(x=long,y=lat,group=group,fill=log10(Population))) + 
  coord_quickmap() +
  geom_polygon() + 
  ggtitle("Population by Country (2015-2019)") +
  labs(fill="Population") +
  theme_void() + 
  scale_fill_viridis_c()

#Happiness by country map, brightness by population
happiness_map$Population[is.na(happiness_map$Population)] <- 100000
happiness_map$pop_cat<-factor(cut(1/log10(happiness_map$Population),breaks=seq(0,1,.02)),labels=c("Large","","Medium"," ","Small"))
ggplot(happiness_map) +
  coord_quickmap() +
  geom_polygon(aes(x=long, y=lat, group=group, fill=Score)) +
  geom_polygon(aes(x=long, y=lat, group=group, alpha=pop_cat)) +
  ggtitle("Happiness by Country (2015-2019)") +
  labs(fill="Happiness Score",alpha="Population") +
  theme_void() + 
  scale_fill_viridis_c() +
  scale_alpha_manual(values = c(0, 0.25, 0.5, 0.75, 1))

#POINT
#Get average absolute value of latitude (distance from equator) for each country with the average happiness score
latitude<-happiness_map %>% group_by(region) %>% summarise(score=mean(Score),lat=mean(abs(lat)),pop=mean(Population)) %>% drop_na()

#Score versus distance from equator (not used)
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
lat_group <- latitude %>% group_by(lat_cat) %>% summarise(score=mean(score),pop=mean(pop),count=n())

#Score versus distance from equator 
bar_plot<-ggplot(lat_group,aes(x=lat_cat,y=score,fill=count)) +
  geom_bar(stat="identity") +
  ggtitle("Happiness vs Distance from Equator") +
  xlab(label="Distance from Equator by Latitude") +
  ylab(label="Happiness Score") +
  labs(fill="Number of Countries") +
  theme_classic() + 
  scale_fill_gradient(low = "grey25", high = " light blue")

#Count versus distance from equator colored by score (not used)
ggplot(lat_group,aes(x=lat_cat,y=count,fill=score)) +
  geom_bar(stat="identity") + 
  ggtitle("Number of Countries vs Distance from Equator") +
  xlab(label="Distance from Equator by Latitude") +
  ylab(label="Number of Countries") +
  labs(fill="Happiness Score") +
  theme_classic() +
  scale_fill_viridis_c(option="cividis")

lay <- rbind(c(1,1,1,1,1,1),
             c(NA,2,2,2,2,NA))
grid.arrange(map_plot, bar_plot, layout_matrix=lay)

latitude$lat_cat<-factor(cut(latitude$lat,breaks=seq(0,70,5),labels=seq(0,65,5)))
latitude<-latitude %>% group_by(lat_cat) %>% mutate(mean_score=mean(score))
box_plot <- ggplot(latitude,aes(x=lat_cat,y=score,fill=mean_score)) +
  geom_boxplot() +
  ggtitle("Happiness vs Distance from Equator") +
  xlab(label="Distance from Equator by Latitude") +
  ylab(label="Happiness Score") +
  labs(fill="Average Happiness Score") +
  theme_classic() + 
  scale_fill_viridis_c(option="magma") 
  #scale_fill_gradient(low = "dodgerblue4", high = "cadetblue1")

grid.arrange(map_plot, box_plot, nrow=2)
