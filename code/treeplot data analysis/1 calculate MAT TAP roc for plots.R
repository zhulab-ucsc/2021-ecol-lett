# Executing this piece of code takes up much time. The output has been saved in the repo.
library(tidyverse)
library(ggpubr)

coord_climate_mean <- read_csv(gzfile("./data/CHELSA MAT TAP.csv.gz"))

##### Calculate ROC (rate of change)
mat_regression <- coord_climate_mean %>%
  drop_na(mat) %>%
  group_by(lon, lat) %>%
  do(broom::tidy(lm(mat ~ year, .))) %>%
  filter(term == "year") %>%
  dplyr::select(lon, lat, mat_roc = estimate, mat_p = p.value)

tap_regression <- coord_climate_mean %>%
  drop_na(tap) %>%
  group_by(lon, lat) %>%
  do(broom::tidy(lm(tap ~ year, .))) %>%
  filter(term == "year") %>%
  dplyr::select(lon, lat, tap_roc = estimate, tap_p = p.value)

coord_climate <- coord_climate_mean %>%
  group_by(lon, lat) %>%
  dplyr::summarize(
    mat = mean(mat, na.rm = TRUE),
    tap = mean(tap, na.rm = TRUE),
    .groups = "keep"
  ) %>%
  ungroup() %>%
  left_join(mat_regression, by = c("lon", "lat")) %>%
  left_join(tap_regression, by = c("lon", "lat"))
write_csv(coord_climate, gzfile("./data/CHELSA MAT TAP roc.csv.gz"))

##### Check assumption of regression models for MAT

plot_mountains_df <- read_csv(gzfile("./data/Coordinates in mountains.csv.gz"))

group_by_coord <- group_split(coord_climate_mean %>%
                                left_join(plot_mountains_df, by = c("lon", "lat")) %>%
                                filter(mountain == 1 | is.na(mountain)) %>% # either from temperate mountainous areas or from tropical studies (all tropical plots here are in mountains)
                                group_by(lon, lat))

set.seed(42)
rep <- 1000 # randomly choose locations
sample_list <- sample(1:length(group_by_coord), rep)
check_df <- data.frame(
  id = rep(NA, rep),
  mountain = rep(NA, rep),
  slope = rep(NA, rep),
  p = rep(NA, rep),
  norm_p = rep(NA, rep)
)
for (i in 1:rep) {
  m <- lm(mat ~ year, data = group_by_coord[[i]])
  check_df$id[i] <- i
  check_df$mountain[i] <- group_by_coord[[i]]$mountain[1]
  check_df$slope[i] <- m$coefficients[2]
  check_df$p[i] <- summary(m)$coefficients[2, 4]
  check_df$norm_p[i] <- shapiro.test(m$residuals)$p.value
  print(i)
}

# total number of locations with a significant warming trend
n_total <- check_df %>%
  filter(
    slope > 0,
    p < 0.05
  ) %>%
  count()
n_total

# total number of locations with a significant warming trend but violates the normal assumption
n_violate <- check_df %>%
  filter(
    slope > 0,
    p < 0.05,
    norm_p < 0.05
  ) %>%
  count()
n_violate

n_violate / n_total

cairo_pdf("./figures/treeplot figures/temp regression qqplot.pdf")
set.seed(42)
i <- sample(1:1000, 1)
m <- lm(mat ~ year, data = group_by_coord[[i]])
check_df$id[i] <- i
check_df$mountain[i] <- group_by_coord[[i]]$mountain[1]
check_df$slope[i] <- m$coefficients[2]
check_df$p[i] <- summary(m)$coefficients[2, 4]
check_df$norm_p[i] <- shapiro.test(m$residuals)$p.value
ggqqplot(m$residuals)
dev.off()
