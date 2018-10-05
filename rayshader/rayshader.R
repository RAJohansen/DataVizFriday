devtools::install_github("tylermorganwall/rayshader")
library(rayshader)
install.packages("elevatr")
library(elevatr)

#Here, I load a map for the River Derwent in Tasmania with the raster package:
localtif = raster::raster("C:/temp/Cincy.tif")

#And convert it to a matrix:
elmat = matrix(raster::extract(localtif,raster::extent(localtif),buffer=1000),
               nrow=ncol(localtif),ncol=nrow(localtif))

#sphere_shade can shift the sun direction:

elmat %>%
  sphere_shade() %>%
  add_water(detect_water(elmat, cutoff = 0.9)) %>%
  add_shadow(ray_shade(elmat)) %>%
  add_shadow(lamb_shade(elmat)) %>%
  add_shadow(ambient_shade(elmat)) %>% 
  plot_map()



elmat %>%
  sphere_shade(texture = "imhof1") %>% 
  #add_shadow(ray_shade(elmat,zscale=5)) %>%
  add_shadow(ambient_shade(elmat,zscale=5)) %>%
  plot_3d(elmat, water=TRUE, zscale=5, theta=-45,
          waterdepth = 28, wateralpha = 0.6, watercolor = "#88DDFF",
          waterlinecolor = "white", waterlinealpha = 0.5)
