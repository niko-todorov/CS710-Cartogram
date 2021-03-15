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



happiness <- tibble(read_csv("data/happinessdata.csv"))
