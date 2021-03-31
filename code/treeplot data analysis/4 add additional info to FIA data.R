library(tidyverse)
library(raster)
library(rgdal)
library(maps)

### Load FIA data

inventory <- read_csv(gzfile("./data/FIA raw data.csv.gz"))

# check timespan of data
timescale <- inventory %>%
  group_by(lon, lat) %>%
  dplyr::summarize(
    minyear = min(measyear),
    maxyear = max(measyear)
  ) %>%
  mutate(range = maxyear - minyear) %>%
  filter(range > 0) %>%
  ungroup()

hist(timescale$range)
quantile(timescale$range, c(0.025, 0.975))
timescale %>% arrange(range)
timescale %>% arrange(desc(range))

##### Climatic optima
optima <- read_csv("./data/FIA species list.climate_optima.csv") %>%
  dplyr::select(-c(common_name, Genus, Species, Gensp))
summary(optima)

##### Mountain extent
plot_mountains_df <- read_csv(gzfile("./data/Coordinates in mountains.csv.gz"))

##### Hexagonal grids
plot_hex_df <- read_csv(gzfile("./data/Coordinates in hexagons.csv.gz"))

##### Join with inventory data
inventory_join <- inventory %>%
  left_join(plot_mountains_df, by = c("lon", "lat")) %>%
  left_join(plot_hex_df, by = c("lon", "lat")) %>%
  left_join(optima, by = "spcd")

write_csv(inventory_join, gzfile("./data/FIA data with additional info.csv.gz"))

# count the number of species that have too few climate optima records
fewer10 <- inventory_join %>%
  filter(mountain == 1) %>% # filter for plots in mountains
  filter(dia > 5) %>%
  dplyr::select(spcd, sp.opt_mat.7080, sp_N.7080, gn.opt_mat.7080) %>%
  group_by(spcd) %>%
  slice(1)

length(fewer10$spcd) # all sp
length(fewer10[!is.na(fewer10$gn.opt_mat.7080) & is.na(fewer10$sp.opt_mat.7080), ]$spcd) # sp records < 10, genus records >= 10
length(fewer10[is.na(fewer10$gn.opt_mat.7080), ]$spcd) # sp records< 10, genus records < 10
length(fewer10[!is.na(fewer10$sp.opt_mat.7080), ]$spcd) # sp records > 10
