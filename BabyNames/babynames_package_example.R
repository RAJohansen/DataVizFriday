library(tidyverse)
library(babynames)
richard <- babynames[which(babynames$name=="Richard"),]
Alexander <- babynames[which(babynames$name=="Alexander"),]

names <- rbind(richard,Alexander)

names_year <- names %>%
  group_by(year,name) %>%
  summarise(count = sum(n))

ggplot(names_year, aes(x = year, y = count, col =name)) +
  geom_line( size=1)+
  geom_text(aes(label = "I was \n Born", x = 1989, y = 25000), col= "black") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90))
