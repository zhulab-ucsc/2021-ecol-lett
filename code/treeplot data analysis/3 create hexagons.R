library(tidyverse)
library(raster)
library(rgdal)
library(maps)
library(sf)

# avoid errors in finding C++14 compiler when installing spatialEco
# FYI, package 'spatialEco' requires R >=  3.6.0
if (!require("spatialEco", character.only = TRUE)) {
  source("./code/treeplot data analysis/install spatialEco.R")
}
library(spatialEco)

##### Create hexagonal grids
# References:
# http://strimas.com/spatial/hexagonal-grids/

# get polygon for continental US
study_area <- raster::getData("GADM",
                      country = "USA", level = 0,
                      path = "./data/"
) %>%
  disaggregate() %>%
  geometry()
study_area <- sapply(study_area@polygons, slot, "area") %>%
  {
    which(. == max(.)) # largest piece of land
  } %>%
  study_area[.]

# get polygon for Alaska
study_area_alaska <- raster::getData("GADM",
                             country = "USA", level = 1,
                             path = "./data/"
) %>%
  disaggregate() %>%
  geometry()
study_area_alaska <- sapply(study_area_alaska@polygons, slot, "area") %>%
  {
    which(. == max(.)) # largest state
  } %>%
  study_area_alaska[.]

study_area <- rbind(study_area, study_area_alaska)

size <- 1
# size <- 1.5 # used for supplementary analysis. When doing so, add "_1.5" to file names.
# size <- 0.5 # used for supplementary analysis. When doing so, add "_0.5" to file names.
hex_points <- spsample(study_area, type = "hexagonal", cellsize = size, offset = c(0, 0))
hex_grid <- HexPoints2SpatialPolygons(hex_points, dx = size)
plot(study_area, col = "grey50", bg = "light blue", axes = TRUE)
plot(hex_points, col = "black", pch = 20, cex = 0.5, add = T)
plot(hex_grid, border = "orange", add = T)

##### Assign inventory plots to hexagons

# get plot coordinates
inventory <- read_csv(gzfile("./data/FIA raw data.csv.gz"))

plot_coords <- inventory %>%
  dplyr::select(lon, lat) %>%
  distinct()
coordinates(plot_coords) <- ~ lon + lat
proj4string(plot_coords) <- proj4string(hex_grid)
proj4string(hex_grid) <- proj4string(plot_coords)

# intersects point and polygon feature classes and adds polygon attributes to points
plot_hex <- spatialEco::point.in.poly(plot_coords, hex_grid)
plot_hex_df <- data.frame(plot_coords, hexagon = data.frame(plot_hex)[, "poly.ids"]) %>%
  as_tibble() %>%
  mutate(hexagon = as.factor(hexagon))

plot_hex_df

ggplot(plot_hex_df) +
  geom_point(aes(x = lon, y = lat, col = hexagon)) +
  theme_classic() +
  theme(legend.position = "none")

write_csv(plot_hex_df, gzfile("./data/Coordinates in hexagons.csv.gz"))
