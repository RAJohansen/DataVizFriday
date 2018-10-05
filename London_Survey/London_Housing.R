library(tidyverse)
df <- read.csv("C:/R_Packages/DataVizFriday/London_Survey/London_Housing_Survey_edit.csv")

#Only exploring the first 9 columns
df <- df[,1:9]


df$London_Satisfaction <- as.character(df$London_Satisfaction)
df$London_Satisfaction[df$London_Satisfaction == "Very dissatisfied"] <- "1"
df$London_Satisfaction[df$London_Satisfaction == "Fairly dissatisfied"] <- "2"
df$London_Satisfaction[df$London_Satisfaction == "Neither satisfied nor dissatisfied"] <- "3"
df$London_Satisfaction[df$London_Satisfaction == "Fairly satisfied"] <- "4"
df$London_Satisfaction[df$London_Satisfaction == "Very satisfied"] <- "5"
df$London_Satisfaction[df$London_Satisfaction == "No opinion"] <- NA
df$London_Satisfaction <- as.numeric(df$London_Satisfaction)

ggplot(df) + geom_tile(aes(Age, Race, fill = London_Satisfaction)) +
  facet_wrap(~Gender) +
  scale_fill_gradient("Living in London\n Satisfaction Level \n 1(Very Unsatisfied) -5 (Very Satisfied)", low = "white" ,high = "steelblue") + 
    theme_classic()
