#install.packages("corrr")
library(corrr)
library(tidyverse)
library(scales)
df <- read.csv("C:/Data/my_data.csv")
df_sub <- df[c(9:11,13:20)]
rs1 <- correlate(df_sub)
rs1 %>% 
  rearrange(method = "MDS", absolute = FALSE) %>% 
  shave() %>% 
  rplot(shape = 15, colors = c("red", "green"))



#TEST
d <- mtcars
rs <- correlate(d)
rs %>% 
  rearrange(method = "MDS", absolute = FALSE) %>% 
  shave() %>% 
  rplot(shape = 15, colors = c("red", "green"),scales=list(x=list(rot=90)))
