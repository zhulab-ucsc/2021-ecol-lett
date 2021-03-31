library(tidyverse)
library(doParallel)
library(foreach)
library(raster)

##### Download CHELSA data

### locate files to download

# this function returns the urls of files that need to be downloaded
find_ch_files <- function(variables, layers, timeframes) {
  layers <- layers %>% str_pad(2, pad = "0")
  expand.grid(year = timeframes, variable = variables, layer = layers) %>%
    mutate(file = paste0("CHELSA_", variable, "_", year, "_", layer, "_V1.2.1.tif")) %>%
    mutate(url = paste0("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/timeseries/", variable, "/", file))
}
# I found the link to files here: https://envicloud.wsl.ch/#/?prefix=chelsa%2Fchelsa_V1

timeframes <- 1979:2013
layers <- 1:12
variables <- c("tmean", "tmax", "tmin", "prec") # choose the variables to download data for

ch_files <- find_ch_files(variables, layers, timeframes)

### download files

cores <- detectCores()
cl <- makeCluster(cores[1] - 1)
registerDoParallel(cl)

for (variable in variables) {
  dir.create(paste0("./data/CHELSA/", variable), recursive = T)
}

foreach(i = 1:nrow(ch_files)) %dopar%
  try({
    download.file(ch_files$url[i], destfile = paste0("./data/CHELSA/", ch_files$variable[i], "/", ch_files$file[i]), method = "auto")
  })

for (variable in variables) {
  print(length(list.files(paste0("./data/CHELSA/", variable))))
}
# there should be 35*12=420 files in each folder

stopCluster(cl)

### check files

files <- list.files(path = paste0("./data/CHELSA/", ch_files$variable[1]), full.names = T)
plot(raster(files[[1]]))

##### Get CHELSA climatic variables for selected locations

### get locations of temperate and tropical treeplots

# Please note: Tropical treeplot data should not be downloaded and used directly. Please contact the authors of the following papers to access tropical treeplot data.
# Fadrique, B., Báez, S., Duque, Á., Malizia, A., Blundo, C., Carilla, J., et al. (2018). Widespread but heterogeneous responses of Andean forests to climate change. Nature, 564, 207–212.
# Feeley, K.J., Hurtado, J., Saatchi, S., Silman, M.R. & Clark, D.B. (2013). Compositional shifts in Costa Rican forests due to climate-driven species migrations. Glob. Chang. Biol., 19, 3472–3480.

inventory <- read_csv(gzfile("./data/FIA raw data.csv.gz"))
trop1 <- read_csv("./data/andes.migrations.csv") %>%
  dplyr::select(plot.name = `Plot name`, Elevation = `Elevation (m a.s.l.)`, Latitude, Longitude, TR = Trplot)
trop2 <- read_csv("./data/CR.TR.csv") # tropical 2
trop <- rbind(trop1, trop2)

coord_temp <- inventory %>%
  distinct(lon, lat)
coord_trop <- trop %>%
  distinct(lon = Longitude, lat = Latitude)

coord <- rbind(coord_temp, coord_trop)

### calculate mean annual temperature (MAT)

cores <- detectCores()
cl <- makeCluster(cores[1] - 1)
registerDoParallel(cl)

years <- 1979:2013

coord_climate_list <- foreach(
  j = 1:length(years),
  .combine = rbind,
  .packages = c("raster", "tidyverse")
) %dopar% {
  coord_climate <- coord %>%
    mutate(year = years[j])
  
  # locate files for the corresponding year
  files <- list.files(path = paste0("./data/CHELSA/", "tmean"), pattern = as.character(years[j]), full.names = T)
  
  # get variables at all locations month by month
  for (i in 1:12) {
    coord_climate[, i + 3] <- raster::extract(raster(files[[i]]), SpatialPoints(data.frame(coord_climate$lon, coord_climate$lat)))
  }
  
  coord_climate
}

coord_climate_tmean <- coord_climate_list %>%
  gather(key = "month", value = "value", -lon, -lat, -year) %>%
  group_by(lon, lat, year) %>%
  summarize(mat = mean(value)) %>%
  ungroup() %>%
  mutate(mat = mat / 10 - 272.15) # transform to degree Celsius
write_csv(coord_climate_tmean, gzfile("./data/CHELSA MAT.csv.gz"))

### calculate total annual precipitation (TAP)

coord_climate_list <- foreach(
  j = 1:length(years),
  .combine = rbind,
  .packages = c("raster", "tidyverse")
) %dopar% {
  coord_climate <- coord %>%
    mutate(year = years[j])
  
  # locate files for the corresponding year
  files <- list.files(path = paste0("./data/CHELSA/", "prec"), pattern = as.character(years[j]), full.names = T)
  
  # get variables at all locations month by month
  for (i in 1:12) {
    coord_climate[, i + 3] <- raster::extract(raster(files[[i]]), SpatialPoints(data.frame(coord_climate$lon, coord_climate$lat)))
  }
  
  coord_climate
}

coord_climate_prec <- coord_climate_list %>%
  gather(key = "month", value = "value", -lon, -lat, -year) %>%
  filter(value != 65535) %>% # I notice that CHELSA uses 65535 to represent NA for precipitation data
  group_by(lon, lat, year) %>%
  summarize(tap = mean(value, na.rm = T)) %>% # monthly mean
  mutate(tap = tap * 12) %>% # 12 months
  ungroup()
write_csv(coord_climate_prec, gzfile("./data/CHELSA TAP.csv.gz"))

coord_climate_tmean <- read_csv(gzfile("./data/CHELSA MAT.csv.gz"))
coord_climate_prec <- read_csv(gzfile("./data/CHELSA TAP.csv.gz"))
coord_climate_mean <- left_join(coord_climate_tmean, coord_climate_prec, by = c("lon", "lat", "year"))
write_csv(coord_climate_mean, gzfile("./data/CHELSA MAT TAP.csv.gz"))

### calculate BIO5

coord_climate_list <- foreach(
  j = 1:length(years),
  .combine = rbind,
  .packages = c("raster", "tidyverse")
) %dopar% {
  coord_climate <- coord %>%
    mutate(year = years[j])
  files <- list.files(path = paste0("./data/CHELSA/", "tmax"), pattern = as.character(years[j]), full.names = T)
  for (i in 1:12) {
    coord_climate[, i + 3] <- raster::extract(raster(files[[i]]), SpatialPoints(data.frame(coord_climate$lon, coord_climate$lat)))
  }
  coord_climate
}

coord_climate_bio5 <- coord_climate_list %>%
  gather(key = "month", value = "value", -lon, -lat, -year) %>%
  group_by(lon, lat, year) %>%
  summarize(bio5 = max(value)) %>%
  ungroup() %>%
  mutate(bio5 = bio5 / 10 - 272.15)
write_csv(coord_climate_bio5, gzfile("./data/CHELSA BIO5.csv.gz"))

### calculate BIO6
coord_climate_list <- foreach(
  j = 1:length(years),
  .combine = rbind,
  .packages = c("raster", "tidyverse")
) %dopar% {
  coord_climate <- coord %>%
    mutate(year = years[j])
  files <- list.files(path = paste0("./data/CHELSA/", "tmin"), pattern = as.character(years[j]), full.names = T)
  for (i in 1:12) {
    coord_climate[, i + 3] <- raster::extract(raster(files[[i]]), SpatialPoints(data.frame(coord_climate$lon, coord_climate$lat)))
  }
  coord_climate
}

coord_climate_bio6 <- coord_climate_list %>%
  gather(key = "month", value = "value", -lon, -lat, -year) %>%
  group_by(lon, lat, year) %>%
  summarize(bio6 = min(value)) %>%
  ungroup() %>%
  mutate(bio6 = bio6 / 10 - 272.15)
write_csv(coord_climate_bio6, gzfile("./data/CHELSA BIO6.csv.gz"))

### calculate BIO10 and BIO11

coord_climate_list <- foreach(
  j = 1:length(years),
  .combine = rbind,
  .packages = c("raster", "tidyverse")
) %dopar% {
  coord_climate <- coord %>%
    mutate(year = years[j])
  files <- list.files(path = paste0("./data/CHELSA/", "tmean"), pattern = as.character(years[j]), full.names = T)
  for (i in 1:12) {
    coord_climate[, i + 3] <- raster::extract(raster(files[[i]]), SpatialPoints(data.frame(coord_climate$lon, coord_climate$lat)))
  }
  coord_climate
}

colnames(coord_climate_list) <- c("lon", "lat", "year", as.character(seq(1:12)))

coord_climate_bio10 <- coord_climate_list %>%
  gather(key = "month", value = "value", -lon, -lat, -year) %>%
  mutate(month = as.integer(month)) %>%
  arrange(lon, lat, year, month) %>%
  group_by(lon, lat) %>%
  mutate(
    next1 = lead(value, 1),
    next2 = lead(value, 2)
  ) %>%
  mutate(quartermean = (value + next1 + next2) / 3) %>%
  ungroup() %>%
  group_by(lon, lat, year) %>%
  summarize(bio10 = max(quartermean)) %>%
  ungroup() %>%
  mutate(bio10 = bio10 / 10 - 272.15)
write_csv(coord_climate_bio10, gzfile("./data/CHELSA BIO10.csv.gz"))

coord_climate_bio11 <- coord_climate_list %>%
  gather(key = "month", value = "value", -lon, -lat, -year) %>%
  mutate(month = as.integer(month)) %>%
  arrange(lon, lat, year, month) %>%
  group_by(lon, lat) %>%
  mutate(
    next1 = lead(value, 1),
    next2 = lead(value, 2)
  ) %>%
  mutate(quartermean = (value + next1 + next2) / 3) %>%
  ungroup() %>%
  group_by(lon, lat, year) %>%
  summarize(bio11 = min(quartermean)) %>%
  ungroup() %>%
  mutate(bio11 = bio11 / 10 - 272.15)
write_csv(coord_climate_bio11, gzfile("./data/CHELSA BIO11.csv.gz"))

stopCluster()

### compare BIO variables

coord_climate_tmean <- read_csv(gzfile("./data/CHELSA MAT.csv.gz"))
coord_climate_bio5 <- read_csv(gzfile("./data/CHELSA BIO5.csv.gz"))
coord_climate_bio6 <- read_csv(gzfile("./data/CHELSA BIO6.csv.gz"))
coord_climate_bio10 <- read_csv(gzfile("./data/CHELSA BIO10.csv.gz"))
coord_climate_bio11 <- read_csv(gzfile("./data/CHELSA BIO11.csv.gz"))

coord_climate_compare <- coord_climate_tmean %>%
  left_join(coord_climate_bio5, by = c("lon", "lat", "year")) %>%
  left_join(coord_climate_bio6, by = c("lon", "lat", "year")) %>%
  left_join(coord_climate_bio10, by = c("lon", "lat", "year")) %>%
  left_join(coord_climate_bio11, by = c("lon", "lat", "year")) %>%
  dplyr::select(lon,
                lat,
                year,
                MAT = mat,
                BIO_5 = bio5,
                BIO_6 = bio6,
                BIO_10 = bio10,
                BIO_11 = bio11
  )

cairo_pdf("./figures/treeplot figures/lat MAT correlation.pdf")
ggplot(coord_climate_compare, aes(x = abs(lat), y = MAT)) +
  stat_binhex(bins = 1000) +
  theme_classic() +
  xlab("absolute latitude (°)") +
  ylab("mean annual temperature (°C)")
dev.off()

summary(lm(MAT ~ abs(lat), data = coord_climate_compare))

Hmisc::rcorr(as.matrix(coord_climate_compare %>% dplyr::select(-lon, -lat, -year)))

cairo_pdf("./figures/treeplot figures/bioclim vars correlation.pdf")
PerformanceAnalytics::chart.Correlation(coord_climate_compare %>% dplyr::select(-lon, -lat, -year) %>% sample_n(1000), histogram = TRUE)
dev.off()
