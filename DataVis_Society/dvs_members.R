library(tidyverse)
df <- read.csv("C:/R_Packages/DataVizFriday/DataVis_Society/dvs_challenge.csv")

df_sub <- df[1:50,]

df$red <- (df$data-min(df$data))/(max(df$data)-min(df$data))
df$green <- (df$visualization-min(df$visualization))/(max(df$visualization)-min(df$visualization))
df$blue <- (df$society-min(df$society))/(max(df$society)-min(df$society))

ggplot(df_sub,aes(long,lat)) +
  geom_point(aes(col = rgb(r=var1, g=var2, b=var3))) +
  theme_classic()
