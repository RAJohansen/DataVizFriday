install.packages("censusapi")
library(censusapi)
library(tidyverse)
library(raster)
library(tmap)
library(ggplot2)

# Add key to .Renviron
Sys.setenv(CENSUS_KEY="e88090be6af7a914412014546f0b8d4215f85f43")
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")

apis <- listCensusApis()
View(apis)

acs_med_income_2015 <- getCensus(name = "acs/acs5",
                        vintage = 2015, 
                        vars = c("NAME", "B19013_001E"), 
                        region = "tract:*", 
                        regionin = "state:39")

acs_med_income_2016 <- getCensus(name = "acs/acs5",
                                 vintage = 2016, 
                                 vars = c("NAME", "B19013_001E"), 
                                 region = "tract:*", 
                                 regionin = "state:39")

# Est. Median Household Income in the last 12 months B19013_001E
Oh_tract <- shapefile("C:/DataVizFriday/Ohio Tract Income/tl_2016_39_tract/tl_2016_39_tract.shp")

acs_med_income_2015$TRACTCE <- gsub("[^[:digit:]]", "", acs_med_income_2015$tract)
acs_med_income_2016$TRACTCE <- gsub("[^[:digit:]]", "", acs_med_income_2016$tract)

Tract_2015 <- merge(Oh_tract_sub, acs_med_income_2015, by='TRACTCE')
Tract_2016 <- merge(Oh_tract, acs_med_income_2016, by='TRACTCE', duplicateGeoms = FALSE)

Oh_tract$ALAND <- as.numeric(Oh_tract$ALAND)
Oh_tract$AWATER <- as.numeric(Oh_tract$AWATER)
Oh_tract$ALAND <- (Oh_tract$ALAND/1000000)
Oh_tract$AWATER <- (Oh_tract$AWATER/1000000)

Oh_tract_sub$LW_Ratio <- Oh_tract_sub$AWATER/Oh_tract_sub$ALAND

Tract_2015$ALAND <- as.numeric(Tract_2015$ALAND)
Tract_2016$ALAND <- as.numeric(Tract_2016$ALAND)
Tract_2015$AWATER <- as.numeric(Tract_2015$AWATER)
Tract_2016$AWATER <- as.numeric(Tract_2016$AWATER)

Tracts_15 <- acs_med_income_2015 %>% group_by(TRACTCE) %>% 
  summarise(Income= mean(B19013_001E))

tail(sort(Oh_tract_sub$AWATER),10)
Oh_tract_sub <- Oh_tract[which(Oh_tract$AWATER<300),]

tm_shape(Oh_tract_sub) +
  tm_fill(col= "AWATER", breaks = c(0,0.5,1,2,5,10,25,50), palette = "Blues",
          title = "Water Area (km2)") +
  tm_borders(col = "lightgrey") +
  tm_compass(type = "arrow",size = 1, position = c("left", "top")) +
  tm_scale_bar(breaks = c(0, 50, 100), size = 0.5, position = c("left", "bottom")) +
  tm_layout(
    legend.title.size=0.75,
    legend.text.size = 0.6,
    legend.position = c("right","bottom"))

tm_shape(Oh_tract_sub) +
  tm_fill(col= "ALAND", palette = "YlOrBr",
          title = "Land Area (km2)") +
  tm_borders(col = "lightgrey") +
  tm_compass(type = "arrow",size = 1, position = c("left", "top")) +
  tm_scale_bar(breaks = c(0, 50, 100), size = 0.5, position = c("left", "bottom")) +
  tm_layout(
    legend.title.size=0.75,
    legend.text.size = 0.6,
    legend.position = c("right","bottom"))

tm_shape(Oh_tract_sub) +
  tm_fill(col= "LW_Ratio",breaks = c(0,0.0001,0.001, 0.01,0.1,1), palette = "RdBu",
          title = "Water to Land Ratio") +
  tm_borders(col = "white") +
  tm_compass(type = "arrow",size = 1, position = c("left", "top")) +
  tm_scale_bar(breaks = c(0, 50, 100), size = 0.5, position = c("left", "bottom")) +
  tm_layout(
    legend.title.size=0.75,
    legend.text.size = 0.6,
    legend.position = c("right","bottom"))

