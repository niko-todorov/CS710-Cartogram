packages<-c("ggplot2","tibble","dplyr","tidyr","viridis","mapproj","purrr")
install.packages(setdiff(packages, rownames(installed.packages())))
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(maps)
library(mapproj)
library(tibble)
library(purrr)
library(readr)
library(ggalt)



happiness <- read_csv("data/happinessdata.csv")
mid <- mean(happiness$Year)
ggplot(happiness,aes(x=Economy,y=HappinessScore,color=Year))+
  geom_point() +
  scale_color_gradient2(midpoint=mid,low="blue",mid="white",
                        high="red",space="Lab")

ggplot(happiness,aes(x=Economy,y=HappinessScore,color=Region))+
  geom_point() +
  geom_encircle()
