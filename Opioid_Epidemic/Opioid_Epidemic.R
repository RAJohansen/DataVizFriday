library(tidyverse)
library(geofacet)
df <- read.csv("C:/Users/johansra/Box Sync/DataVizFriday/Opioid_Epidemic/Opioid_Deaths_2013-2016.csv")

#Create geo_faceted map 
df$Deaths <- as.numeric(df$Deaths)

ggplot(df) +
  geom_line(aes(Year, Deaths), color = "lightblue") +
  geom_point(aes(Year, Deaths), color = "#6495ED") +
  facet_geo(~State, grid = us_state_grid2) +
  theme_classic() +
  labs(title = "National Opioid Epidemic",
       subtitle = "Annual Opioid-Related Deaths by State from 2013-2016",
       x = "Year",
       y = "Opioid\nDeaths",
       caption = "Data from: www.cdc.gov/drugoverdose/data/statedeaths") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_text(angle = 0, hjust = 1),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title=element_text(size=16), 
        legend.text=element_text(size=12))
