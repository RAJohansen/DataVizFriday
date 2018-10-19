#Load Tidyverse 
library(tidyverse)

#Create new data set from NPR Chart
df <- tibble::tibble(
  Year = c(2015,2018,2020),
  Real_Deficit = c(439,779,1000))

#Create new data set from NPR Chart
df1 <- tibble::tibble(
  Year = c(2015,2016,2017,2018,2020),
  Real_Deficit = c(439, NA,NA,779,1000),
  Scaled_Deficit = c(439, 808, 1501,1987,2610),
  Bar_Height = c(19,35,65,86,113),
  Scale_Diff = c(23.1,NA,NA,9.0,8.8))

#Plot rescaled Graph following orginal values
ggplot(df) + geom_col(aes(Year, Real_Deficit), fill = "red", width = 1) +
  geom_path(aes(Year, Real_Deficit),color="black", linetype="dashed", size=1.5) +
  geom_text(aes(Year, Real_Deficit,label = Real_Deficit, vjust= 1), color = "white") +
  scale_x_continuous(breaks = seq(2015, 2020, 1)) + 
  labs(title = "United States Deficit from 2015-2020", subtitle = "Rescaled using original image", x = "Year", y = "Deficit \n (billions of $") +
  theme_classic()

#Plot Reprojected Graph following scale of 2015
ggplot(df1) + geom_col(aes(Year, Scaled_Deficit), fill = "red") +
  geom_path(aes(Year, Scaled_Deficit),color="black", linetype="dashed", size=1.5) +
  geom_text(aes(Year, Scaled_Deficit,label = Scaled_Deficit, vjust= 1), color = "white") +
  scale_x_continuous(breaks = seq(2015, 2020, 1)) + 
  labs(title = "United States Deficit from 2015-2020", subtitle = "Reprojected using 2015 scale ($23.1 billion/mm)",
       x = "Year", y = "Deficit \n (billions of $") +
theme_classic()
