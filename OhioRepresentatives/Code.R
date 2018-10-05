library(tidyverse)
library(raster)
library(tmap)
library(leaflet)
library(ggplot2)

reps <- read.csv("C:/temp/DataVizFriday/OhioRepresentatives/table.csv")
reps$NAME <- gsub("[^[:digit:]]", "", reps$district)
districts <- shapefile("C:/temp/DataVizFriday/OhioRepresentatives/gz_2010_39_620_l2_500k.shp")

rep_dist <- merge(districts, reps, by='NAME')
rep_dist$party <- as.factor(rep_dist$party)

tm_shape(rep_dist) +
  tm_fill(col= "party", title = "Political Party", palette = c("mediumblue","red3","grey")) +
  tm_borders(col = "white") +
  tm_compass(type = "arrow",size = 1, position = c("left", "top")) +
  tm_scale_bar(breaks = c(0, 50, 100), size = 0.5, position = c("left", "bottom"))+ 
  tm_shape(leadership) + tm_dots(col = "STATE", title = "Leadership Position", palette = "black", shape =  20, size = 1, labels = "") +
  tm_legend(scale = 0.9, legend.outside.position = c("right", "bottom"))

tmap_mode("plot")
