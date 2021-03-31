library(tidyverse)

##### Summarize climatic optima on the plot level

inventory_join <- read_csv(gzfile("./data/FIA data with additional info.csv.gz"))

inventory_optima <- inventory_join %>%
  filter(mountain == 1) %>% # filter for plots in mountains
  filter(dia > 5) %>%
  mutate(opt_mat=if_else(!is.na(sp.opt_mat.7080),sp.opt_mat.7080,gn.opt_mat.7080)) %>% # take genus value when sp value not available
  mutate(opt_tap=if_else(!is.na(sp.opt_tap.7080),sp.opt_tap.7080,gn.opt_tap.7080)) %>%
  # mutate(opt_mat=sp.opt_mat.7080) %>%
  # mutate(opt_tap=sp.opt_tap.7080) %>% # turn on these two lines and turn off the previous two lines if we do not take genus value even when sp value not available. after doing so, we create output files with "_sponly"
  mutate(basal=pi*dia^2/4)%>%
  dplyr::select( lon,  lat,measyear,opt_mat, opt_tap, basal )  %>% 
  group_by(lon, lat, measyear) %>% 
  dplyr::summarise(
    cti_basal = weighted.mean(opt_mat / 10, w=basal, na.rm = TRUE),  # weigh by basal area
    cpi_basal = weighted.mean(opt_tap, w=basal, na.rm = TRUE),
    cti_ind = mean(opt_mat / 10, na.rm = TRUE),  # weigh by number of individuals
    cpi_ind = mean(opt_tap, na.rm = TRUE),
    .groups="keep"
  ) %>%
  ungroup() %>% 
  group_by( lon, lat) %>%
  drop_na() %>%
  filter(n() > 1) # more than 1 year of records

##### Linear regression to calculate CTI roc and CPI roc

cti_regression <- inventory_optima %>%
  do(broom::tidy(lm(cti_basal ~ measyear, .))) %>%
  filter(term == "measyear") %>%
  dplyr::select(lon, lat, cti_roc = estimate)

cpi_regression <- inventory_optima %>%
  do(broom::tidy(lm(cpi_basal ~ measyear, .))) %>%
  filter(term == "measyear") %>%
  dplyr::select(lon, lat, cpi_roc = estimate)

##### Join with CHELSA MAT roc and TAP roc

coord_climate <- read_csv(gzfile("data/CHELSA MAT TAP roc.csv.gz"))
inventory_process <- inventory_join %>%
  filter(mountain == 1) %>% # filter for plots in mountains
  filter(dia > 5) %>%
  mutate(basal=pi*dia^2/4)%>% 
  group_by(lon, lat,elev, hexagon) %>% # summarize by plot
  dplyr::summarize(
    total_basal=sum(basal, na.rm = TRUE)/length(unique(measyear)),
    number_of_trees = n()/length(unique(measyear)),
    number_of_plots = length(unique(lat, lon)),
    .groups="keep"
  ) %>%
  left_join(cti_regression, by = c("lon", "lat")) %>%
  left_join(cpi_regression, by = c("lon", "lat")) %>%
  left_join(coord_climate, by = c("lon", "lat")) %>% # MAT TAP roc
  mutate(
    t_tracking_ratio = cti_roc / mat_roc, # MAT tracking (as ratio)
    p_tracking_ratio = cpi_roc / tap_roc # TAP tracking (as ratio)
  ) %>%
  mutate(
    t_tracking_diff = mat_roc - cti_roc, # MAT tracking (as difference)
    p_tracking_diff = tap_roc - cpi_roc # TAP tracking (as difference)
  ) %>%
  drop_na()

summary(inventory_process)
write_csv(inventory_process, gzfile("data/FIA plot data with CTI CPI roc.csv.gz"))
