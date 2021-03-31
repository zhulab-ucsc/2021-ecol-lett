# analyze resurvey data at assemblage level (called "communities" in order to align with community temperature index of tree plot data)

library(ggplot2)
theme_set(theme_classic()) # this works better here than setting "+theme_classic()" on each command, which causes +theme() not to work

library(nlme) # switch to using nlme instead of lme4 because tree plot analysis used lme4
# library(lme4) # used lme4 at first; a couple things, like confint(my_model) work in lme4 but not nlme...
library(emmeans)


# read in data
studydata <- read.csv("./data/DatasetS1.csv", stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA", ""))
dim(studydata)

# make variable for binning studies as tropical or temperate
studydata$temptrop <- ifelse(abs(studydata$latitude) < 23.4, "trop", "temp")
table(studydata$temptrop)
# temp trop
# 76   17
summary(studydata$latitude)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# -46.90   31.03   40.85   35.48   48.95   63.43

#####
# Make figure of observed shift vs. expected shift (each dot = a distinct study, with associated standard error)
#####

# data plotted is the weighted shift & weighted shift se (i.e., includes mean shifts, upper limit shifts, & lower limit shifts)

a <- ggplot(data = studydata, aes(x = expected_shift, y = weighted_shift)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_abline(intercept = 0, slope = 0, linetype = "dotted") +
  geom_point(aes(color = temptrop), size = 2.5, alpha = .85) +
  scale_x_continuous("expected shift (m)") +
  scale_y_continuous("observed shift (m)") +
  geom_errorbar(aes(ymin = weighted_shift - weighted_se, ymax = weighted_shift + weighted_se),
                position = "dodge", size = 0.3
  ) +
  scale_color_viridis_d(name = "latitudinal zone", begin = 0.1, end = 0.9, labels = c("temperate", "tropical"))

a
ggsave("./figures/resurvey figures/obs_vs_exp_lat_full.pdf", height = 4.5, width = 6.66)



#####
# first have to make a new dataframe that has a row for shifts for each assemblage (i.e., convert from wide to long layout)
# shift from dataframe that has each assemblage as its own row
# to each measurement of shift (lower limit vs. mean elevation vs. upper limit) as its own row
#####

# there is definitely a better way to do this, but the code below works

# summarize shifts at mean elevations
mean <- studydata[, c(2, 20:23)]
head(mean)
mean$sp_n <- mean$sp_n_mean
mean$shift <- mean$shift_mean
mean$shift_se <- mean$shift_mean_se
mean$type <- "mean"
head(mean)
mean <- mean[, c(1:2, 6:8)]

# summarize shifts at lower elevation limits
lower <- studydata[, c(2, 25:27)]
head(lower)
lower$sp_n <- lower$sp_n_lower_limit
lower$shift <- lower$shift_lower_limit
lower$shift_se <- lower$shift_lower_limit_se
lower$type <- "lower"
lower <- lower[, c(1, 5:8)]

# summarize shifts at upper elevation limits
upper <- studydata[, c(2, 29:31)]
head(upper)
upper$sp_n <- upper$sp_n_upper
upper$shift <- upper$shift_upper_limit
upper$shift_se <- upper$shift_upper_limit_se
upper$type <- "upper"
upper <- upper[, c(1, 5:8)]

# combine these in a new dataframe
studydata_long <- rbind(mean, upper, lower)
studydata_long <- studydata_long[!is.na(studydata_long$shift), ]
dim(studydata_long)
table(studydata_long$type)
# lower  mean upper
# 50    59    60

# add variables to this new studydata_long dataframe

# add expected shift
studydata_long$expected_shift <- studydata$expected_shift[match(studydata_long$study, studydata$study)]

# add latitude & temperate/tropical binary variable
studydata_long$latitude <- studydata$latitude[match(studydata_long$study, studydata$study)]
studydata_long$temptrop <- studydata$temptrop[match(studydata_long$study, studydata$study)]

# add spatial scale
studydata_long$scale <- studydata$study_spatial_scale[match(studydata_long$study, studydata$study)]

# add duration
studydata_long$duration <- as.numeric(studydata$duration[match(studydata_long$study, studydata$study)])

# add thermy
studydata_long$thermy <- studydata$thermy[match(studydata_long$study, studydata$study)]

# add temp change
studydata_long$tempchange <- studydata$temp_change[match(studydata_long$study, studydata$study)]

# add taxa
studydata_long$taxa <- studydata$taxa[match(studydata_long$study, studydata$study)]

# make an absolute latitude variable
studydata_long$abs_latitude <- abs(studydata_long$latitude)

# create responsiveness variable ( = "temperature tracking" variable)

studydata_long$responsiveness <- studydata_long$shift / studydata_long$expected_shift

# create alternative metrics to define temperature tracking, based on differences rather than ratio
# per recommendation during second round of review

# first where overshoots are positive and lags are negative
studydata_long$responsiveness_diff <- studydata_long$shift - studydata_long$expected_shift

# and second where both overshoots and lags are negative
studydata_long$responsiveness_abs_diff <- abs(studydata_long$shift - studydata_long$expected_shift) * -1


######
# analysis of "absolute tracking", using rate of shift (shift/decade) as response variable
######

studydata_long$abs_tracking <- (studydata_long$shift / studydata_long$duration) * 10


studydata_long$type <- as.factor(studydata_long$type)
studydata_long$type <- relevel(studydata_long$type, "mean") # so that model summary gives deviations from mean elevation shifts for the "type" factor

studydata_long$temptrop <- as.factor(studydata_long$temptrop)
studydata_long$temptrop <- relevel(studydata_long$temptrop, "temp")

lmm_categorical <- lme(data = studydata_long, abs_tracking ~ temptrop + sp_n + type + scale + duration, random = ~ 1 | study)

hist(resid(lmm_categorical))
summary(lmm_categorical)
# confint(lmm_categorical) # works when using lmer (lme4) but not after switching to lme (nlme)

emmeans(lmm_categorical, specs = "temptrop") # to get model estimated means for tropics and temperate zone
temptropmeans <- emmeans(lmm_categorical, specs = "temptrop")
pairs(temptropmeans) # contrast between temperate and tropical...

studydata_long$temptrop <- as.factor(studydata_long$temptrop)
studydata_long$temptrop <- relevel(studydata_long$temptrop, "trop")

ggplot(data = studydata_long, aes(x = temptrop, y = abs_tracking)) +
  geom_boxplot(fill = c("#D16103", "#4E84C4"), outlier.shape = NA) +
  geom_jitter(alpha = 0.3, size = 0.7, width = 0.2) +
  scale_y_continuous("elevational shift (m/decade)", limits = c(-35, 150)) +
  scale_x_discrete("", labels = c("tropical", "temperate"))
ggsave("./figures/resurvey figures/Figure_assemblage_abs_tracking_long_boxplot.pdf", height = 4, width = 2)

# estimate from a model that does not differentiate between tropical and temperate...
lmm_categorical2 <- lme(data = studydata_long, abs_tracking ~ sp_n + type + scale + duration, random = ~ 1 | study)
summary(lmm_categorical2)


# same but for absolute latitude

lmm_continuous <- lme(abs_tracking ~ abs_latitude + type + scale + sp_n + duration, random = ~ 1 | study, data = studydata_long)
summary(lmm_continuous)

grpmeans <- emmeans(lmm_continuous, "type")
pairs(grpmeans) # contrasts between range types



predictions <- emmeans(lmm_continuous, specs = c("abs_latitude"), at = list(abs_latitude = c(seq(from = 0, to = 64, by = 1)))) # this is an emmGrid object
predictions <- as.data.frame(predictions) # make this a dataframe to more easily plot predictions
# this is the model-estimated value of absolute tracking at different latitudes, with confidence levels

theme_set(theme_classic()) # this works better here than setting "+theme_classic()" on each command, which causes +theme() not to work

ggplot(data = studydata_long, aes(y = abs_tracking, x = abs_latitude)) +
  geom_ribbon(data = predictions, aes(x = abs_latitude, ymin = lower.CL, ymax = upper.CL), inherit.aes = FALSE, fill = "gray", alpha = 0.65) +
  geom_point(size = 2, alpha = .5) + # can add size or color = type to plot different range types as different colors
  geom_line(data = predictions, aes(x = abs_latitude, y = emmean), linetype = "solid", color = "coral2", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(-35, 150), "elevational shift (m/decade)") +
  scale_x_continuous("absolute latitude")
ggsave("./figures/resurvey figures/study_model_abs_tracking_predictions.pdf", height = 4, width = 6)


######
# make a plot of rate of temperature changes in this dataset
######

head(studydata)
studydata$temp_rate <- (studydata$temp_change / studydata$duration) * 10

ggplot(data = studydata, aes(y = temp_rate, x = abs(latitude))) +
  geom_point(size = 2, alpha = .5) +
  scale_y_continuous("warming rate (°C/decade)") +
  scale_x_continuous("absolute latitude")
ggsave("./figures/resurvey figures/resurvey_warming_vs_latitude.pdf", height = 4, width = 6)



#####
# Fit models with temperature tracking (called "responsiveness" in the dataframe) as the response variable (this is at the assemblage level)
#####

# get rid of 3 studies where temperatures cooled
studydata_long <- studydata_long[studydata_long$tempchange > 0, ]

#####
# fit a mixed-effects model for latitude as a categorical variable (tropical vs. temperate)
#####

# two variables should be factors
studydata_long$type <- as.factor(studydata_long$type)
studydata_long$scale <- as.factor(studydata_long$scale)

studydata_long$type <- relevel(studydata_long$type, "mean") # so that model summary gives deviations from mean elevation shifts for the "type" factor
studydata_long$temptrop <- relevel(studydata_long$temptrop, "temp") # so that model summary gives deviations from temperate zone

# fit model
lmm_categorical <- lme(data = studydata_long, responsiveness ~ temptrop + sp_n + type + scale + duration, random = ~ 1 | study)

hist(resid(lmm_categorical))
summary(lmm_categorical)
# confint(lmm_categorical)

library(MuMIn)
r.squaredGLMM(lmm_categorical)

emmeans(lmm_categorical, specs = "temptrop") # to get model estimated means for tropics and temperate zone
temptropmeans <- emmeans(lmm_categorical, specs = "temptrop")
pairs(temptropmeans) # contrast between temperate and tropical...



# plot raw data for categorical comparison (quite close to model estimates for mean of tropical and temperate)

studydata_long$temptrop <- as.factor(studydata_long$temptrop)
studydata_long$temptrop <- relevel(studydata_long$temptrop, "trop")

ggplot(data = studydata_long, aes(x = temptrop, y = responsiveness)) +
  geom_boxplot(fill = c("#D16103", "#4E84C4"), outlier.shape = NA) +
  geom_jitter(alpha = 0.3, size = 0.7, width = 0.2) +
  scale_y_continuous("temperature tracking", limits = c(-1.5, 3)) +
  scale_x_discrete("", labels = c("tropical", "temperate"))
ggsave("./figures/resurvey figures/Figure_assemblage_long_boxplot.pdf", height = 4, width = 2)


#####
# fit a mixed-effects model for latitude as a continuous variable
#####

dim(studydata_long) # 162 measurements of range shifts
length(unique(studydata_long$study)) # from 90 different assemblages

# fit model
lmm_continuous <- lme(responsiveness ~ abs_latitude + type + scale + duration + sp_n, random = ~ 1 | study, data = studydata_long)

# look at model fit
hist(resid(lmm_continuous))

# look at model results
summary(lmm_continuous, scipen = 999)
# confint(lmm_continuous)

# overall r2 of model
r.squaredGLMM(lmm_continuous)


##
# now have to
# (1) plot model predictions (e.g. predictions against latitude for type = mean and scale = local) WITH confidence intervals
# (2) make a plot of model coefficients
#
# struggled for a long time trying to figure out how to do these for a linear mixed model with factors that have multiple levels...

# calculate marginal means for different levels of predictors
library(emmeans)
grpmeans <- emmeans(lmm_continuous, "type")
pairs(grpmeans) # contrasts between range types -

predictions <- emmeans(lmm_continuous, specs = c("abs_latitude"), at = list(abs_latitude = c(seq(from = 0, to = 64, by = 1)))) # this is an emmGrid object
predictions <- as.data.frame(predictions) # make this a dataframe to more easily plot predictions
# this is the model-estimated value of responsiveness (= temperature tracking) at different latitudes, with confidence levels

#####
# make figure showing study level data
#####

theme_set(theme_classic()) # this works better here than setting "+theme_classic()" on each command, which causes +theme() not to work

ggplot(data = studydata_long, aes(y = responsiveness, x = abs_latitude)) +
  geom_ribbon(data = predictions, aes(x = abs_latitude, ymin = lower.CL, ymax = upper.CL), inherit.aes = FALSE, fill = "gray", alpha = 0.5) +
  geom_point(size = 2, alpha = .5) + # can add size or color = type to plot different range types as different colors
  geom_line(data = predictions, aes(x = abs_latitude, y = emmean), linetype = "solid", color = "coral2", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_continuous(limits = c(-2, 3), "temperature tracking") + # can make y limits narrower c(-1, 2.2) to show bulk of data...
  scale_x_continuous("absolute latitude")
ggsave("./figures/resurvey figures/study_model_predictions.pdf", height = 4, width = 6)




#####
# do a sanity check to test whether results of assemblage model are driven by assemblages with a small number of species
# specifically, do same analysis but restricted to assemblages with 10 or more species
#####

studydata_long$temptrop <- as.factor(studydata_long$temptrop)
studydata_long$temptrop <- relevel(studydata_long$temptrop, "temp")

dim(studydata_long[studydata_long$sp_n > 9, ]) # 135
length(unique(studydata_long$study[studydata_long$sp_n > 9])) # 74

lmm_categorical_10sp <- lme(data = studydata_long[studydata_long$sp_n > 9, ], responsiveness ~ temptrop + sp_n + type + scale + duration, random = ~ 1 | study)
summary(lmm_categorical_10sp)

lmm_10sp <- lme(responsiveness ~ abs_latitude + type + scale + duration + sp_n, random = ~ 1 | study, data = studydata_long[studydata_long$sp_n > 9, ])
summary(lmm_10sp)

r.squaredGLMM(lmm_10sp)

studydata_long$temptrop <- as.factor(studydata_long$temptrop)
studydata_long$temptrop <- relevel(studydata_long$temptrop, "trop")

ggplot(data = studydata_long[studydata_long$sp_n > 9, ], aes(x = temptrop, y = responsiveness)) +
  geom_boxplot(fill = c("#D16103", "#4E84C4"), outlier.shape = NA) +
  geom_jitter(alpha = 0.3, size = 0.7, width = 0.2) +
  scale_y_continuous("temperature tracking", limits = c(-1.5, 3)) +
  scale_x_discrete("", labels = c("tropical", "temperate"))
ggsave("./figures/resurvey figures/Figure_assemblage_long_boxplot_10.pdf", height = 4, width = 2)

predictions <- emmeans(lmm_10sp, specs = c("abs_latitude"), at = list(abs_latitude = c(seq(from = 0, to = 64, by = 1)))) # this is an emmGrid object
predictions <- as.data.frame(predictions) # make this a dataframe to more easily plot predictions and confidence intervals

ggplot(data = studydata_long[studydata_long$sp_n > 9, ], aes(y = responsiveness, x = abs_latitude)) +
  geom_ribbon(data = predictions, aes(x = abs_latitude, ymin = lower.CL, ymax = upper.CL), inherit.aes = FALSE, fill = "gray", alpha = 0.5) +
  geom_point(size = 2, alpha = .5) + # can add size or color = type to plot different range types as different colors
  geom_line(data = predictions, aes(x = abs_latitude, y = emmean), linetype = "solid", color = "coral2", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_continuous(limits = c(-2, 3), "temperature tracking") + # can make y limits narrower c(-1, 2.2) to show bulk of data...
  scale_x_continuous("absolute latitude")
ggsave("./figures/resurvey figures/study_model_predictions_10.pdf", height = 4, width = 6)


#####
# the resurvey data is taxonomically diverse.
# but data from birds for both temperate and tropical zones; fit models for ONLY BIRDS
#####

dim(studydata_long[studydata_long$taxa == "bird", ]) # 40
length(unique(studydata_long$study[studydata_long$taxa == "bird"])) # 19
summary(studydata_long$latitude[studydata_long$taxa == "bird"])

studydata_long$temptrop <- as.factor(studydata_long$temptrop)
studydata_long$temptrop <- relevel(studydata_long$temptrop, "trop")

ggplot(data = studydata_long[studydata_long$taxa == "bird", ], aes(x = temptrop, y = responsiveness)) +
  geom_boxplot(fill = c("#D16103", "#4E84C4"), outlier.shape = NA) +
  geom_jitter(alpha = 0.3, size = 0.7, width = 0.2) +
  scale_y_continuous("temperature tracking", limits = c(-1.5, 3)) +
  scale_x_discrete("", labels = c("tropical", "temperate"))
ggsave("./figures/resurvey figures/Figure_assemblage_long_boxplot_bird.pdf", height = 4, width = 2)

studydata_long$temptrop <- relevel(studydata_long$temptrop, "temp")

lmm_categorical_bird <- lme(data = studydata_long[studydata_long$taxa == "bird", ], responsiveness ~ temptrop + sp_n + type + scale + duration, random = ~ 1 | study)
summary(lmm_categorical_bird)

lmm_bird <- lme(responsiveness ~ abs_latitude + type + scale + duration + sp_n, random = ~ 1 | study, data = studydata_long[studydata_long$taxa == "bird", ])
summary(lmm_bird)
r.squaredGLMM(lmm_bird)


predictions <- emmeans(lmm_bird, specs = c("abs_latitude"), at = list(abs_latitude = c(seq(from = 4, to = 54, by = 1)))) # this is an emmGrid object
predictions <- as.data.frame(predictions) # make this a dataframe to more easily plot predictions and confidence intervals

ggplot(data = studydata_long[studydata_long$taxa == "bird", ], aes(y = responsiveness, x = abs_latitude)) +
  geom_ribbon(data = predictions, aes(x = abs_latitude, ymin = lower.CL, ymax = upper.CL), inherit.aes = FALSE, fill = "gray", alpha = 0.5) +
  geom_point(size = 2, alpha = .5) + # can add size or color = type to plot different range types as different colors
  geom_line(data = predictions, aes(x = abs_latitude, y = emmean), linetype = "solid", color = "coral2", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_continuous(limits = c(-2, 3), "temperature tracking") + # can make y limits narrower c(-1, 2.2) to show bulk of data...
  scale_x_continuous("absolute latitude")
ggsave("./figures/resurvey figures/study_model_predictions_birds.pdf", height = 4, width = 6)


# produce similar plots for amphibians, arthropods and plants to look at initial patterns; not enough data to fit models

studydata_long$temptrop <- as.factor(studydata_long$temptrop)
studydata_long$temptrop <- relevel(studydata_long$temptrop, "trop")

# amphibians
ggplot(data = studydata_long[studydata_long$taxa == "amphibian", ], aes(x = temptrop, y = responsiveness)) +
  geom_boxplot(fill = c("#D16103", "#4E84C4"), outlier.shape = NA) +
  geom_jitter(alpha = 0.3, size = 0.7, width = 0.2) +
  scale_y_continuous("temperature tracking", limits = c(-1.5, 3)) +
  scale_x_discrete("", labels = c("tropical", "temperate"))
ggsave("./figures/resurvey figures/Boxplot_amphibian.pdf", height = 4, width = 2)

ggplot(data = studydata_long[studydata_long$taxa == "amphibian", ], aes(y = responsiveness, x = abs_latitude)) +
  geom_point(size = 2, alpha = .5) + # can add size or color = type to plot different range types as different colors
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_continuous(limits = c(-1.5, 3), "temperature tracking") + # can make y limits narrower c(-1, 2.2) to show bulk of data...
  scale_x_continuous(limits = c(0, 65), "absolute latitude")
ggsave("./figures/resurvey figures/Scatterplot_amphibians.pdf", height = 4, width = 6)


# arthropods

ggplot(data = studydata_long[studydata_long$taxa == "arthropod", ], aes(x = temptrop, y = responsiveness)) +
  geom_boxplot(fill = c("#D16103", "#4E84C4"), outlier.shape = NA) +
  geom_jitter(alpha = 0.3, size = 0.7, width = 0.2) +
  scale_y_continuous("temperature tracking", limits = c(-1.5, 3)) +
  scale_x_discrete("", labels = c("tropical", "temperate"))
ggsave("./figures/resurvey figures/Boxplot_arthropods.pdf", height = 4, width = 2)

ggplot(data = studydata_long[studydata_long$taxa == "arthropod", ], aes(y = responsiveness, x = abs_latitude)) +
  geom_point(size = 2, alpha = .5) + # can add size or color = type to plot different range types as different colors
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_continuous(limits = c(-1.5, 3), "temperature tracking") + # can make y limits narrower c(-1, 2.2) to show bulk of data...
  scale_x_continuous(limits = c(0, 65), "absolute latitude")
ggsave("./figures/resurvey figures/Scatterplot_arthropods.pdf", height = 4, width = 6)

# plants

ggplot(data = studydata_long[studydata_long$taxa == "plant", ], aes(x = temptrop, y = responsiveness)) +
  geom_boxplot(fill = c("#D16103", "#4E84C4"), outlier.shape = NA) +
  geom_jitter(alpha = 0.3, size = 0.7, width = 0.2) +
  scale_y_continuous("temperature tracking", limits = c(-1.5, 3)) +
  scale_x_discrete("", labels = c("tropical", "temperate"))
ggsave("./figures/resurvey figures/Boxplot_plants.pdf", height = 4, width = 2)

ggplot(data = studydata_long[studydata_long$taxa == "plant", ], aes(y = responsiveness, x = abs_latitude)) +
  geom_point(size = 2, alpha = .5) + # can add size or color = type to plot different range types as different colors
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_continuous(limits = c(-1.5, 3), "temperature tracking") + # can make y limits narrower c(-1, 2.2) to show bulk of data...
  scale_x_continuous(limits = c(0, 65), "absolute latitude")
ggsave("./figures/resurvey figures/Scatterplot_plants.pdf", height = 4, width = 6)


#######
# analyze alternate metrics of calculating temperature tracking
#######

# first for temperature tracking as a difference between expected and observed shifts ("overshoots" are positive values)

studydata_long$type <- relevel(studydata_long$type, "mean") # so that model summary gives deviations from mean elevation shifts for the "type" factor
studydata_long$temptrop <- relevel(studydata_long$temptrop, "temp") # so that model summary gives deviations from temperate zone

# fit model
lmm_categorical <- lme(data = studydata_long, responsiveness_diff ~ temptrop + sp_n + type + scale + duration, random = ~ 1 | study)

hist(resid(lmm_categorical))
summary(lmm_categorical)

r.squaredGLMM(lmm_categorical)

emmeans(lmm_categorical, specs = "temptrop") # to get model estimated means for tropics and temperate zone
temptropmeans <- emmeans(lmm_categorical, specs = "temptrop")
pairs(temptropmeans) # contrast between temperate and tropical...

studydata_long$temptrop <- as.factor(studydata_long$temptrop)
studydata_long$temptrop <- relevel(studydata_long$temptrop, "trop")

ggplot(data = studydata_long, aes(x = temptrop, y = responsiveness_diff)) +
  geom_boxplot(fill = c("#D16103", "#4E84C4"), outlier.shape = NA) +
  geom_jitter(alpha = 0.3, size = 0.7, width = 0.2) +
  scale_y_continuous("temperature tracking (difference)") +
  scale_x_discrete("", labels = c("tropical", "temperate"))
ggsave("./figures/resurvey figures/Figure_assemblage_long_boxplot_difference.pdf", height = 4, width = 2)

# fit a mixed-effects model for latitude as a continuous variable

lmm_continuous <- lme(responsiveness_diff ~ abs_latitude + type + scale + duration + sp_n, random = ~ 1 | study, data = studydata_long)

# look at model fit
hist(resid(lmm_continuous))

# look at model results
summary(lmm_continuous, scipen = 999)

# overall r2 of model
r.squaredGLMM(lmm_continuous)

grpmeans <- emmeans(lmm_continuous, "type")
pairs(grpmeans) # contrasts between range types -

predictions <- emmeans(lmm_continuous, specs = c("abs_latitude"), at = list(abs_latitude = c(seq(from = 0, to = 64, by = 1)))) # this is an emmGrid object
predictions <- as.data.frame(predictions) # make this a dataframe to more easily plot predictions
# this is the model-estimated value of responsiveness (= temperature tracking) at different latitudes, with confidence levels

# make figure showing study level data

theme_set(theme_classic()) # this works better here than setting "+theme_classic()" on each command, which causes +theme() not to work

ggplot(data = studydata_long, aes(y = responsiveness_diff, x = abs_latitude)) +
  geom_ribbon(data = predictions, aes(x = abs_latitude, ymin = lower.CL, ymax = upper.CL), inherit.aes = FALSE, fill = "gray", alpha = 0.5) +
  geom_point(size = 2, alpha = .5) + # can add size or color = type to plot different range types as different colors
  geom_line(data = predictions, aes(x = abs_latitude, y = emmean), linetype = "solid", color = "coral2", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_continuous("temperature tracking (difference)") + # can make y limits narrower c(-1, 2.2) to show bulk of data...
  scale_x_continuous("absolute latitude")
ggsave("./figures/resurvey figures/study_model_predictions_difference.pdf", height = 4, width = 6)







# second for temperature tracking as absolute value of difference between expected and observed shifts ("overshoots" are negative values)

studydata_long$type <- relevel(studydata_long$type, "mean") # so that model summary gives deviations from mean elevation shifts for the "type" factor
studydata_long$temptrop <- relevel(studydata_long$temptrop, "temp") # so that model summary gives deviations from temperate zone

# fit model
lmm_categorical <- lme(data = studydata_long, responsiveness_abs_diff ~ temptrop + sp_n + type + scale + duration, random = ~ 1 | study)

hist(resid(lmm_categorical))
summary(lmm_categorical)

r.squaredGLMM(lmm_categorical)

temptropmeans <- emmeans(lmm_categorical, specs = "temptrop") # to get model estimated means for tropics and temperate zone
temptropmeans
pairs(temptropmeans) # contrast between temperate and tropical...

studydata_long$temptrop <- as.factor(studydata_long$temptrop)
studydata_long$temptrop <- relevel(studydata_long$temptrop, "trop")

ggplot(data = studydata_long, aes(x = temptrop, y = responsiveness_abs_diff)) +
  geom_boxplot(fill = c("#D16103", "#4E84C4"), outlier.shape = NA) +
  geom_jitter(alpha = 0.3, size = 0.7, width = 0.2) +
  scale_y_continuous("temperature tracking (absolute value of difference)") +
  scale_x_discrete("", labels = c("tropical", "temperate"))
ggsave("./figures/resurvey figures/Figure_assemblage_long_boxplot_abs_difference.pdf", height = 4, width = 2)

# fit a mixed-effects model for latitude as a continuous variable

lmm_continuous <- lme(responsiveness_abs_diff ~ abs_latitude + type + scale + duration + sp_n, random = ~ 1 | study, data = studydata_long)

# look at model fit
hist(resid(lmm_continuous))

# look at model results
summary(lmm_continuous, scipen = 999)

# overall r2 of model
r.squaredGLMM(lmm_continuous)

grpmeans <- emmeans(lmm_continuous, "type")
grpmeans
pairs(grpmeans) # contrasts between range types -

predictions <- emmeans(lmm_continuous, specs = c("abs_latitude"), at = list(abs_latitude = c(seq(from = 0, to = 64, by = 1)))) # this is an emmGrid object
predictions <- as.data.frame(predictions) # make this a dataframe to more easily plot predictions
# this is the model-estimated value of responsiveness (= temperature tracking) at different latitudes, with confidence levels

# make figure showing study level data

theme_set(theme_classic()) # this works better here than setting "+theme_classic()" on each command, which causes +theme() not to work

ggplot(data = studydata_long, aes(y = responsiveness_abs_diff, x = abs_latitude)) +
  geom_ribbon(data = predictions, aes(x = abs_latitude, ymin = lower.CL, ymax = upper.CL), inherit.aes = FALSE, fill = "gray", alpha = 0.5) +
  geom_point(size = 2, alpha = .5) + # can add size or color = type to plot different range types as different colors
  geom_line(data = predictions, aes(x = abs_latitude, y = emmean), linetype = "solid", color = "coral2", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_continuous("temperature tracking (absolute value of difference)") + # can make y limits narrower c(-1, 2.2) to show bulk of data...
  scale_x_continuous("absolute latitude")
ggsave("./figures/resurvey figures/study_model_predictions_abs_difference.pdf", height = 4, width = 6)
