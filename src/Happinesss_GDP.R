library(ggplot2)
library(dplyr)

#setwd("/Users/tobychappell/Documents/CS_Courses/CS_710/Workshop1Group3")
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