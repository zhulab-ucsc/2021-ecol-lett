# make map for Figure 1

# read in data for studies
studydata <- read.csv("./data/DatasetS1.csv", stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA", ""))

studydata$temptrop <- ifelse(abs(studydata$latitude < 23.4), "trop", "temp")

#####
# (1) Make map of studies
#####

# make new "locations" dataframe with lats and longs, can't remember why I did this, but I think there was some reason why I didn't just use the dataframe directly.

lat <- studydata$latitude
long <- studydata$longitude
n <- studydata$sp_n
taxa <- studydata$taxa
thermy <- studydata$thermy
locations <- data.frame(lat, long, thermy, n, taxa)

# load in a bunch of packages, not 100% sure all are necessary
library(maptools)
library(raster)
library(maps)
library(mapdata)
library(ggmap)
library(rgdal)
library(viridis)

world <- map_data("world")
theme_set(theme_classic())

z <- ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), color = "gray", fill = "gray") +
  coord_fixed(1.2) +
  scale_x_continuous("longitude") +
  scale_y_continuous("latitude") +
  geom_hline(yintercept = 23.5, linetype = "dashed", colour = "dark gray") + # add tropics in as lines
  geom_hline(yintercept = -23.5, linetype = "dashed", colour = "dark gray") +
  geom_jitter(data = locations, aes(y = lat, x = long, fill = taxa), color = "black", shape = 21, alpha = 0.5, size = 2.5, width = 3, height = 3) + # this makes circles be filled with color but with a black border
  scale_fill_discrete() +
  theme(legend.title = element_blank())

z # plot map. takes 20 seconds or so to plot on my computer

ggsave("./figures/resurvey figures/map.pdf", height = 3.5, width = 6)

table(locations$taxa)
# manually add these sample sizes per taxa to figure in Illustrator




# make map for studies that provide species-level data
studydata <- studydata[studydata$species_level_data != "no", ]
studydata <- studydata[studydata$species_level_data != "not yet", ]

lat <- studydata$latitude
long <- studydata$longitude
n <- studydata$sp_n
taxa <- studydata$taxa
thermy <- studydata$thermy
locations <- data.frame(lat, long, thermy, n, taxa)

z <- ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), color = "gray", fill = "gray") +
  coord_fixed(1.2) +
  scale_x_continuous("longitude") +
  scale_y_continuous("latitude") +
  geom_hline(yintercept = 23.5, linetype = "dashed", colour = "dark gray") + # add tropics in as lines
  geom_hline(yintercept = -23.5, linetype = "dashed", colour = "dark gray") +
  geom_jitter(data = locations, aes(y = lat, x = long, fill = taxa), color = "black", shape = 21, alpha = 0.5, size = 2.5, width = 3, height = 3) + # this makes circles be filled with color but with a black border
  scale_fill_discrete() +
  theme(legend.title = element_blank())

z # plot map. takes 20 seconds or so to plot on my computer
ggsave("./figures/resurvey figures/map_species.pdf", height = 3, width = 6)




# plot map of treeplot locations
studydata <- read.csv(file.choose(), stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA", ""))
# read in inventory_combined.csv

lat <- studydata$lat
long <- studydata$lon
locations <- data.frame(lat, long)

z <- ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), color = "gray", fill = "gray") +
  coord_fixed(1.2) +
  xlab("longitude") +
  ylab("latitude") +
  geom_hline(yintercept = 23.5, linetype = "dashed", colour = "dark gray") + # add tropics in as lines
  geom_hline(yintercept = -23.5, linetype = "dashed", colour = "dark gray") +
  geom_point(data = locations, aes(y = lat, x = long), color = "black", alpha = 0.2) +
  coord_sf(xlim = c(-140, -60), ylim = c(-30, 60), expand = FALSE, label_axes) +
  theme(legend.title = element_blank())

z # plot map. takes 20 second

ggsave("./figures/resurvey figures/map_treeplot.pdf", height = 3, width = 3)
