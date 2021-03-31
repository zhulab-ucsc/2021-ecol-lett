library(tidyverse)
library(gstat)
library(sp)
library(spdep)
library(spaMM)
library(nlme)
library(MASS)

##### Prepare data frame (with both temperate and tropical plots)

coord_climate <- read_csv(gzfile("./data/CHELSA MAT TAP roc.csv.gz"))

### get temperate data

inventory_temp <- read_csv(gzfile("./data/FIA plot data with CTI CPI roc.csv.gz")) %>%
  filter(mat_roc > 0 & mat_p < 0.05) %>% # select warming plots
  group_by(hexagon) %>%
  filter(n() > 5) %>% # filter out hexagons with few plots
  dplyr::summarize(
    elev = weighted.mean(elev, w = total_basal, na.rm = TRUE) / 3.2808399 / 1000,
    lon = weighted.mean(lon, w = total_basal, na.rm = TRUE),
    lat = weighted.mean(lat, w = total_basal, na.rm = TRUE),
    mat = weighted.mean(mat, w = total_basal, na.rm = TRUE),
    tap = weighted.mean(tap, w = total_basal, na.rm = TRUE),
    cti_roc = weighted.mean(cti_roc, w = total_basal, na.rm = TRUE),
    cpi_roc = weighted.mean(cpi_roc, w = total_basal, na.rm = TRUE),
    mat_roc = weighted.mean(mat_roc, w = total_basal, na.rm = TRUE),
    tap_roc = weighted.mean(tap_roc, w = total_basal, na.rm = TRUE),
    t_tracking_ratio = weighted.mean(t_tracking_ratio, w = total_basal, na.rm = TRUE),
    p_tracking_ratio = weighted.mean(p_tracking_ratio, w = total_basal, na.rm = TRUE),
    t_tracking_diff = weighted.mean(t_tracking_diff, w = total_basal, na.rm = TRUE),
    p_tracking_diff = weighted.mean(p_tracking_diff, w = total_basal, na.rm = TRUE),
    total_basal = sum(total_basal, na.rm = TRUE),
    number_of_trees = sum(number_of_trees, na.rm = TRUE),
    number_of_plots = n(),
    .groups = "keep"
  ) %>%
  ungroup() %>%
  dplyr::select(
    lon, lat, elev, mat, tap, cti_roc, mat_roc, tap_roc, t_tracking_ratio,
    t_tracking_diff, number_of_trees, number_of_plots
  ) %>%
  drop_na() %>%
  mutate(inventory = "Forest Inventory Analysis dataset")

# count number of aggregated FIA plots
read_csv(gzfile("data/FIA plot data with CTI CPI roc.csv.gz")) %>%
  filter(mat_roc > 0 & mat_p < 0.05) %>% # select warming plots
  group_by(hexagon) %>%
  summarize(n = n()) %>%
  filter(n > 5) %>%
  nrow()

# check size of aggregated plots
hist(inventory_temp$number_of_trees)
median(inventory_temp$number_of_trees)

hist(inventory_temp$number_of_plots)
median(inventory_temp$number_of_plots)
median(inventory_temp$number_of_plots) * pi * 7.32^2 * 4
median(inventory_temp$number_of_plots) * pi * 7.32^2 * 4 / 10000 # area in ha

# count number of temperate plots (unaggregated) with warming trend, cooling trend, or no trend
count_sites <- read_csv(gzfile("data/FIA plot data with CTI CPI roc.csv.gz")) %>%
  mutate(status = case_when(
    mat_roc > 0 & mat_p < 0.05 ~ "warming",
    mat_roc < 0 & mat_p < 0.05 ~ "cooling",
    TRUE ~ "no change"
  )) %>%
  group_by(status) %>%
  count()
count_sites
sum(count_sites$n)

### get tropical data

# Please note: Tropical treeplot data should not be downloaded and used directly. Please contact the authors of the following papers to access tropical treeplot data.
# Fadrique, B., Báez, S., Duque, Á., Malizia, A., Blundo, C., Carilla, J., et al. (2018). Widespread but heterogeneous responses of Andean forests to climate change. Nature, 564, 207–212.
# Feeley, K.J., Hurtado, J., Saatchi, S., Silman, M.R. & Clark, D.B. (2013). Compositional shifts in Costa Rican forests due to climate-driven species migrations. Glob. Chang. Biol., 19, 3472–3480.

trop1 <- read_csv("./data/andes.migrations.csv") %>% # tropical dataset 1
  dplyr::select(plot.name = `Plot name`, elev = `Elevation (m a.s.l.)`, lat = Latitude, lon = Longitude, cti_roc = Trplot) %>%
  mutate(inventory = "Fadrique et al. (2018)")
trop2 <- read_csv("./data/CR.TR.csv") %>% # tropical 2
  dplyr::select(plot.name, elev = Elevation, lat = Latitude, lon = Longitude, cti_roc = TR) %>%
  mutate(inventory = "Feeley et al. (2013)")
trop <- bind_rows(trop1, trop2)

inventory_trop <- trop %>%
  left_join(coord_climate, by = c("lon", "lat")) %>%
  filter(mat_roc > 0 & mat_p < 0.05) %>% # select warming plots
  dplyr::select(lon, lat, elev, mat, tap, cti_roc, mat_roc, tap_roc, inventory) %>%
  mutate(elev = elev / 1000) %>%
  filter(abs(cti_roc) <= 0.5) %>% # remove outliers
  mutate(t_tracking_ratio = cti_roc / mat_roc) %>%
  mutate(t_tracking_diff = mat_roc - cti_roc) %>%
  drop_na()

# count number of tropical plots with warming trend, cooling trend, or no trend
count_sites <- trop %>%
  left_join(coord_climate, by = c("lon", "lat")) %>%
  mutate(status = case_when(
    mat_roc > 0 & mat_p < 0.05 ~ "warming",
    mat_roc < 0 & mat_p < 0.05 ~ "cooling",
    TRUE ~ "no change"
  )) %>%
  group_by(status) %>%
  count()
count_sites
sum(count_sites$n)

inventory_combined <- bind_rows(inventory_temp, inventory_trop) %>%
  mutate(zone = ifelse(abs(lat) <= 23.4, "tropical", "temperate")) %>%
  mutate(zone = fct_relevel(zone, c("tropical", "temperate"))) %>%
  mutate(group = "1")

dim(inventory_combined)

# check elevation and latitude correlation
cairo_pdf("./figures/treeplot figures/elevation vs latitude correlation.pdf")
ggplot(inventory_combined) +
  geom_point(aes(x = abs(lat), y = elev)) +
  geom_smooth(aes(x = abs(lat), y = elev), method = "lm") +
  theme_classic() +
  ylab("elevation (km)") +
  xlab("absolute latitude (°)")
dev.off()
summary(lm(elev ~ abs(lat), data = inventory_combined))

# save full dataset
write_csv(
  inventory_combined
  %>% dplyr::select(lon, lat, elev, mat, cti_roc, mat_roc, t_tracking_ratio),
  "data/DatasetS3.csv"
)

##### Tropical and temperate numerical summaries

tro_mat_roc <- as.numeric(unlist(inventory_combined %>% filter(zone == "tropical") %>% dplyr::select(mat_roc)))
mean(tro_mat_roc)
sd(tro_mat_roc) / sqrt(length(tro_mat_roc))
tem_mat_roc <- as.numeric(unlist(inventory_combined %>% filter(zone == "temperate") %>% dplyr::select(mat_roc)))
mean(tem_mat_roc)
sd(tem_mat_roc) / sqrt(length(tem_mat_roc))

tro_cti_roc <- as.numeric(unlist(inventory_combined %>% filter(zone == "tropical") %>% dplyr::select(cti_roc)))
mean(tro_cti_roc)
sd(tro_cti_roc) / sqrt(length(tro_cti_roc))
tem_cti_roc <- as.numeric(unlist(inventory_combined %>% filter(zone == "temperate") %>% dplyr::select(cti_roc)))
mean(tem_cti_roc)
sd(tem_cti_roc) / sqrt(length(tem_cti_roc))

tro_t_tracking_ratio <- as.numeric(unlist(inventory_combined %>% filter(zone == "tropical") %>% dplyr::select(t_tracking_ratio)))
mean(tro_t_tracking_ratio)
sd(tro_t_tracking_ratio) / sqrt(length(tro_t_tracking_ratio))
tem_t_tracking_ratio <- as.numeric(unlist(inventory_combined %>% filter(zone == "temperate") %>% dplyr::select(t_tracking_ratio)))
mean(tem_t_tracking_ratio)
sd(tem_t_tracking_ratio) / sqrt(length(tem_t_tracking_ratio))
t.test(tro_t_tracking_ratio, tem_t_tracking_ratio)
mean(tro_t_tracking_ratio) / mean(tem_t_tracking_ratio)

##### Fit spatial models

### check for spatial dependence

inventory_combined_spatial <- inventory_combined
coordinates(inventory_combined_spatial) <- ~ lon + lat
plot(variogram(mat_roc * 10 ~ abs(lat) + elev, inventory_combined_spatial, cutoff = 5))
plot(variogram(cti_roc * 10 ~ abs(lat) + elev, inventory_combined_spatial, cutoff = 5))
plot(variogram(t_tracking_ratio ~ abs(lat) + elev, inventory_combined_spatial, cutoff = 5))

coords <- cbind(inventory_combined$lon, inventory_combined$lat)
coords_nb <- knn2nb(knearneigh(coords, k = 1, longlat = T))
moran.test(resid(lm(mat_roc * 10 ~ abs(lat) + elev, inventory_combined_spatial)), nb2listw(coords_nb, style = "W"))
moran.test(resid(lm(cti_roc * 10 ~ abs(lat) + elev, inventory_combined_spatial)), nb2listw(coords_nb, style = "W"))
moran.test(resid(lm(t_tracking_ratio ~ abs(lat) + elev, inventory_combined_spatial)), nb2listw(coords_nb, style = "W"))

### fit lme

model_mat_1 <- lme(mat_roc * 10 ~ zone + elev, data = inventory_combined, random = ~ 1 | group, correlation = corGaus(1, form = ~ lon + lat), control = lmeControl(opt = "optim"))
summary(model_mat_1)

model_mat_2 <- lme(mat_roc * 10 ~ abs(lat) + elev, data = inventory_combined, random = ~ 1 | group, correlation = corGaus(1, form = ~ lon + lat), control = lmeControl(opt = "optim"))
summary(model_mat_2)

model_cti_1 <- lme(cti_roc * 10 ~ zone + elev, data = inventory_combined, random = ~ 1 | group, correlation = corGaus(1, form = ~ lon + lat), control = lmeControl(opt = "optim"))
summary(model_cti_1)

model_cti_2 <- lme(cti_roc * 10 ~ abs(lat) + elev, data = inventory_combined, random = ~ 1 | group, correlation = corGaus(1, form = ~ lon + lat), control = lmeControl(opt = "optim"))
summary(model_cti_2)

model_tracking_1 <- lme(t_tracking_ratio ~ zone + elev, data = inventory_combined, random = ~ 1 | group, correlation = corGaus(1, form = ~ lon + lat), control = lmeControl(opt = "optim"))
summary(model_tracking_1)

model_tracking_2 <- lme(t_tracking_ratio ~ abs(lat) + elev, data = inventory_combined, random = ~ 1 | group, correlation = corGaus(1, form = ~ lon + lat), control = lmeControl(opt = "optim"))
summary(model_tracking_2)

### summarize results

coef_df_mat_1 <- as.data.frame(summary(model_mat_1)$tTable) %>%
  mutate(
    response = "Warming rate",
    predictor = c("Intercept", "Latitudinal zone", "Elevation (km)")
  )
coef_df_mat_2 <- as.data.frame(summary(model_mat_2)$tTable) %>%
  mutate(
    response = "Warming rate",
    predictor = c("Intercept", "Absolute latitude (°)", "Elevation (km)")
  )
coef_df_cti_1 <- as.data.frame(summary(model_cti_1)$tTable) %>%
  mutate(
    response = "Thermophilization rate",
    predictor = c("Intercept", "Latitudinal zone", "Elevation (km)")
  )
coef_df_cti_2 <- as.data.frame(summary(model_cti_2)$tTable) %>%
  mutate(
    response = "Thermophilization rate",
    predictor = c("Intercept", "Absolute latitude (°)", "Elevation (km)")
  )
coef_df_tracking_1 <- as.data.frame(summary(model_tracking_2)$tTable) %>%
  mutate(
    response = "Temperature tracking score_1",
    predictor = c("Intercept", "Latitudinal zone", "Elevation (km)")
  )
coef_df_tracking_2 <- as.data.frame(summary(model_tracking_2)$tTable) %>%
  mutate(
    response = "Temperature tracking score_2",
    predictor = c("Intercept", "Absolute latitude (°)", "Elevation (km)")
  )

coef_df <- rbind(coef_df_mat_1, coef_df_mat_2, coef_df_cti_1, coef_df_cti_2, coef_df_tracking_1, coef_df_tracking_2) %>%
  dplyr::select(response, predictor, coef = Value, se = Std.Error, t = `t-value`, p = `p-value`) %>%
  mutate(lower = coef - 1.96 * se) %>%
  mutate(upper = coef + 1.96 * se)
coef_df
write_csv(coef_df, "./data/coef summary.csv")

##### Make figures

### map

world <- map_data("world")

cairo_pdf("./figures/treeplot figures/forest inventory plot map.pdf", height = 6, width = 6)
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), color = "gray", fill = "gray") +
  coord_fixed(1.2, xlim = c(-140, -60), ylim = c(-30, 60)) +
  xlab("longitude") +
  ylab("latitude") +
  geom_hline(yintercept = 23.5, linetype = "dashed", colour = "darkgray") + # add tropics in as lines
  geom_hline(yintercept = -23.5, linetype = "dashed", colour = "darkgray") +
  geom_point(data = inventory_combined, aes(x = lon, y = lat, fill = inventory), color = "black", shape = 21, alpha = 0.2, size = 2.5) +
  theme_classic() +
  theme(legend.title = element_blank())
dev.off()

cairo_pdf("./figures/treeplot figures/forest inventory plot map_bw.pdf", height = 3, width = 3)
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), color = "gray", fill = "gray") +
  coord_fixed(1.2, xlim = c(-140, -60), ylim = c(-30, 60)) +
  xlab("longitude") +
  ylab("latitude") +
  geom_hline(yintercept = 23.5, linetype = "dashed", colour = "darkgray") + # add tropics in as lines
  geom_hline(yintercept = -23.5, linetype = "dashed", colour = "darkgray") +
  geom_point(data = inventory_combined, aes(x = lon, y = lat), fill = "black", color = "black", shape = 21, alpha = 0.15, size = 2.5) +
  theme_classic()
dev.off()

### expected vs. observed tracking

cairo_pdf("./figures/treeplot figures/compare expected with observed TR.pdf")
ggplot() +
  geom_point(data = inventory_combined, aes(x = mat_roc * 10, y = cti_roc * 10, color = zone)) +
  scale_color_viridis_d(begin = 0.9, end = 0.1) +
  xlim(0, 0.7) +
  ylim(-0.6, 0.6) +
  coord_fixed() +
  geom_abline(slope = 1, intercept = 0, lty = 2, alpha = 1) +
  geom_hline(yintercept = 0, lty = 3, alpha = 0.5) +
  xlab("expected thermophilization rate (°C/decade)") +
  ylab("observed thermophilization rate (°C/decade)") +
  theme_classic() +
  labs(color = "latitudinal zone")
dev.off()

### boxplots comparing temperate and tropical zones

cairo_pdf("./figures/treeplot figures/boxplot_temperature tracking.pdf", width = 2, height = 4)
ggplot() +
  geom_boxplot(data = inventory_combined, aes(x = zone, y = t_tracking_ratio, fill = zone), outlier.alpha = 0) +
  scale_fill_manual(values = c("#D16103", "#4E84C4")) +
  geom_jitter(data = inventory_combined, aes(x = zone, y = t_tracking_ratio), width = 0.2, alpha = 0.3, size = 0.7) +
  ylim(-3, 3) +
  geom_hline(yintercept = 1, lty = 2, alpha = 0.5) +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.5) +
  ylab("temperature tracking") +
  xlab("") +
  theme_classic() +
  guides(fill = FALSE)
dev.off()

cairo_pdf("./figures/treeplot figures/boxplot_warming rate.pdf", width = 2, height = 4)
ggplot() +
  geom_boxplot(data = inventory_combined, aes(x = zone, y = mat_roc * 10, fill = zone), outlier.alpha = 0) +
  scale_fill_manual(values = c("#D16103", "#4E84C4")) +
  geom_jitter(data = inventory_combined, aes(x = zone, y = mat_roc * 10), width = 0.2, alpha = 0.3, size = 0.7) +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.5) +
  ylab("warming rate (°C/decade)") +
  xlab("") +
  theme_classic() +
  guides(fill = FALSE)
dev.off()

cairo_pdf("./figures/treeplot figures/boxplot_thermophilization rate.pdf", width = 2, height = 4)
ggplot() +
  geom_boxplot(data = inventory_combined, aes(x = zone, y = cti_roc * 10, fill = zone), outlier.alpha = 0) +
  scale_fill_manual(values = c("#D16103", "#4E84C4")) +
  geom_jitter(data = inventory_combined, aes(x = zone, y = cti_roc * 10), width = 0.2, alpha = 0.3, size = 0.7) +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.5) +
  ylab("thermophilization rate (°C/decade)") +
  xlab("") +
  theme_classic() +
  guides(fill = FALSE)
dev.off()

### scatter plots and regression lines to show latitudinal trends

# simulate to get 95% prediction intervals
newdat <- expand.grid(
  lon = seq(-132, -64, length.out = 10),
  lat = seq(-25, 50, length.out = 50),
  elev = seq(0, max(inventory_combined$elev), length.out = 10),
  group = "1"
)

predict_new <- function(model, newdat, boot_num = 1000, param_uncert = T) {
  boot_fit <- matrix(NA, nrow = nrow(newdat), ncol = boot_num)
  for (i in 1:boot_num) {
    new_model <- model
    
    if (param_uncert) {
      mu <- fixef(model)
      Sigma <- vcov(model)
      fixed_new <- MASS::mvrnorm(1, mu, Sigma) # randomly sample coefficients for fixed effects
      
      new_model$coefficients$fixed <- fixed_new # change the coefficients in the model
      # new_model$coefficients$random
      # new_model$sigma
      # new_model$modelStruct$corStruct
    }
    
    boot_fit[, i] <- predict(new_model, newdat, level = length(new_model$coefficients$random)) # using the largest value of "level"
    
    print(i)
  }
  
  newdat_summary <- cbind(newdat, as.data.frame(boot_fit)) %>%
    dplyr::select(-group) %>%
    gather(key = "sample", value = "value", -lon, -lat, -elev) %>%
    group_by(abs(lat)) %>%
    dplyr::summarize(
      mean = mean(value),
      lower = quantile(value, 0.025),
      upper = quantile(value, 0.975),
      sd = sd(value),
      se = sd(value) / sqrt(n())
    ) %>%
    ungroup()
  return(newdat_summary)
}

cairo_pdf("./figures/treeplot figures/trend_temperature tracking.pdf", width = 6, height = 4)
CI <- predict_new(model_tracking_2, newdat, 1000, T)
ggplot(data = inventory_combined, aes(x = abs(lat), y = t_tracking_ratio)) +
  geom_point(data = inventory_combined, aes(x = abs(lat), y = t_tracking_ratio), size = 2, alpha = 0.5) +
  # geom_smooth(method = "lm", color = "coral", se = FALSE) +
  geom_line(data = CI, aes(x = `abs(lat)`, y = mean), col = "coral", lty = 1) +
  geom_line(data = CI, aes(x = `abs(lat)`, y = lower), col = "coral", lty = 2) +
  geom_line(data = CI, aes(x = `abs(lat)`, y = upper), col = "coral", lty = 2) +
  ylim(-3, 3) +
  geom_hline(yintercept = 1, lty = 2, alpha = 0.5) +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.5) +
  ylab("temperature tracking") +
  xlab("absolute latitude") +
  theme_classic()
dev.off()

cairo_pdf("./figures/treeplot figures/trend_warming rate.pdf", width = 6, height = 4)
CI <- predict_new(model_mat_2, newdat, 1000, T)
ggplot(data = inventory_combined, aes(x = abs(lat), y = mat_roc * 10)) +
  geom_point(size = 2, alpha = 0.5) +
  # geom_smooth(method = "lm", color = "coral", se = FALSE) +
  geom_line(data = CI, aes(x = `abs(lat)`, y = mean), col = "coral", lty = 1) +
  geom_line(data = CI, aes(x = `abs(lat)`, y = lower), col = "coral", lty = 2) +
  geom_line(data = CI, aes(x = `abs(lat)`, y = upper), col = "coral", lty = 2) +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.5) +
  ylab("warming rate (°C/decade)") +
  xlab("absolute latitude") +
  theme_classic()
dev.off()

cairo_pdf("./figures/treeplot figures/trend_thermophilization rate.pdf", width = 6, height = 4)
CI <- predict_new(model_cti_2, newdat, 1000, T)
ggplot(data = inventory_combined, aes(x = abs(lat), y = cti_roc * 10)) +
  geom_point(size = 2, alpha = 0.5) +
  # geom_smooth(method = "lm", color = "coral", se = FALSE) +
  geom_line(data = CI, aes(x = `abs(lat)`, y = mean), col = "coral", lty = 1) +
  geom_line(data = CI, aes(x = `abs(lat)`, y = lower), col = "coral", lty = 2) +
  geom_line(data = CI, aes(x = `abs(lat)`, y = upper), col = "coral", lty = 2) +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.5) +
  ylab("thermophilization rate (°C/decade)") +
  xlab("absolute latitude") +
  theme_classic()
dev.off()

##### Make predictions at specified latitudes for temperature tracking score from model

newdat <- expand.grid(
  lon = seq(-132, -64, length.out = 10),
  lat = 0,
  elev = seq(0, max(inventory_combined$elev), length.out = 10),
  group = "1"
)

predict_new(model_tracking_2, newdat, 1000, T)

newdat <- expand.grid(
  lon = seq(-132, -64, length.out = 10),
  lat = 45,
  elev = seq(0, max(inventory_combined$elev), length.out = 10),
  group = "1"
)

predict_new(model_tracking_2, newdat, 1000, T)
