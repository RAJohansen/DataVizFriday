library(maps)
library(tidyverse)
df <- read.csv("C:/R_Packages/DataVizFriday/DataVis_Society/dvs_challenge.csv")

df$red <- (df$data-min(df$data))/(max(df$data)-min(df$data))
df$green <- (df$visualization-min(df$visualization))/(max(df$visualization)-min(df$visualization))
df$blue <- (df$society-min(df$society))/(max(df$society)-min(df$society))

df_sub <- df[1:50,]

mapWorld <- borders("world", colour="gray50", fill="gainsboro", ylim=c(-60, 90))

ggplot() +
  mapWorld +
  geom_point(aes(df$long,df$lat,col = rgb(r=df$red, g=df$green, b=df$blue))) +
  scale_color_identity() +
  theme_classic()
