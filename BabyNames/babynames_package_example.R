library(tidyverse)
library(babynames)
richard <- babynames[which(babynames$name=="Richard"),]
Alexander <- babynames[which(babynames$name=="Alexander"),]

names <- rbind(richard,Alexander)

names_year <- names %>%
  group_by(year,name) %>%
  summarise(count = sum(n))

ggplot(names_year, aes(x = year, y = count, col =name)) +
  labs(x = "Year", y = "Babies Born", title = "My First and Middle Name's Popularity Trend") +
  geom_line( size=1)+
  geom_text(aes(label = "I was \nborn here", x = 1989, y = 25000), col= "black") +
  geom_point(aes(x =1989, y = 15000), col = 'black', size  =3) + 
  theme_bw() +
  theme(axis.text.x=element_text(angle=0),
        axis.title.y = element_text(angle=0, vjust = 0.5))
