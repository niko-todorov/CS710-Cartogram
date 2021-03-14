library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)

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
