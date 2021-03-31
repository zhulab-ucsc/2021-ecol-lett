library(tidyverse)
library(raster)
library(rgdal)
library(maps)

##### Get mountain extent
# References:
# https://rmgsc.cr.usgs.gov/gme/
# https://onlinelibrary.wiley.com/doi/full/10.1111/tgis.12265

mountains <- raster(paste("./data/k3binary.tif", sep = ""))
plot(mountains)

##### Determine if inventory plots are in mountainous area

inventory <- read_csv(gzfile("./data/FIA raw data.csv.gz"))

plot_coords <- inventory %>%
  dplyr::select(lon, lat) %>%
  distinct()
coordinates(plot_coords) <- ~ lon + lat
proj4string(plot_coords) <- proj4string(mountains)

plot_mountains <- extract(mountains, plot_coords)
plot_mountains[is.na(plot_mountains)] <- 0
plot_mountains_df <- data.frame(plot_coords, mountain = plot_mountains)

maps::map("state")
points(plot_mountains_df[plot_mountains_df$mountain == 1, c("lon", "lat")])

write_csv(plot_mountains_df, gzfile("./data/Coordinates in mountains.csv.gz"))
