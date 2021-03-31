# analyze resurvey data at species level

library(ggplot2)
theme_set(theme_classic()) # this works better here than setting "+theme_classic()" on each command, which causes +theme() not to work

library(lme4)
library(emmeans) 
library(MuMIn)


#####
# Fit linear mixed model with responsiveness (= temperature tracking) as the response variable and study as a random factor (species level)
#####

# read in species level data
spdata_raw <- read.csv("DatasetS2.csv", stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA","")) 
dim(spdata_raw)

# make a new dataframe that is in long layout (vs. wide layout) 
# this is a decidedly non-elegant way to do it but it works..

mean <- spdata_raw[,c(1:2,10)]
head(mean)
mean$change <- mean$change_mean_elevation
mean$type <- "mean"
mean <- mean[,-3]

upper <- spdata_raw[,c(1:2,11)]
head(upper)
upper$change <- upper$change_upper_limit
upper$type <- "upper"
upper <- upper[,-3]

lower <- spdata_raw[,c(1:2,9)]
head(lower)
lower$change <- lower$change_low_limit
lower$type <- "lower"
lower <- lower[,-3]

spdata <- rbind(mean, upper, lower) 
spdata <- spdata[!is.na(spdata$change),]
dim(spdata)

# add assemblage attributes from study data (the other dataframe)
# read in data for studies 
studydata <- read.csv("DatasetS1.csv", stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA","")) 

# add expected shift
spdata$expected_shift <- studydata$expected_shift[match(spdata$study, studydata$study)]

# add latitude
spdata$latitude <- studydata$latitude[match(spdata$study, studydata$study)]

# add spatial scale
spdata$scale <- studydata$study_spatial_scale[match(spdata$study, studydata$study)]

# add duration
spdata$duration <- as.numeric(studydata$duration[match(spdata$study, studydata$study)])

# add temp change
spdata$tempchange <- studydata$temp_change[match(spdata$study, studydata$study)]

# make responsiveness variable ( = temperature tracking)
spdata$responsiveness <- spdata$change / spdata$expected_shift

length(unique(spdata$study))
# remove Kuhn study because it duplicates earlier but more extensive (more species sampled) Lenoir study (both are for French plants using same data...)
spdata <- spdata[spdata$study != "Kuhn et al. 2016",]

spdata$abs_latitude <- abs(spdata$latitude)
summary(spdata$abs_latitude)

spdata$temptrop <- ifelse(spdata$abs_latitude < 23.4, "trop", "temp")
table(spdata$temptrop)




######
# analysis of "absolute tracking", using rate of shift (shift/decade) as response variable
######

spdata$abs_tracking <- (spdata$change/spdata$duration) * 10

spdata$type <- as.factor(spdata$type)
spdata$type <- relevel(spdata$type, "mean")

#fit using lme4, as lme4 better handles models with multiple non-nested random effects compared to nlme
lmm_categorical <- lmer(data = spdata, abs_tracking ~ temptrop + type + scale + (1|study) + (1|latin_name))
# Note: This lmer function is equivalent to the following lme function from the nlme package, but the former is much faster.
# spdata_with_dummy <- spdata %>%
#   mutate(dummy = 1) %>%
#   mutate(
#     temptrop = as.factor(temptrop),
#     type = as.factor(type),
#     scale = as.factor(scale),
#     study = as.factor(study),
#     latin_name = as.factor(latin_name),
#     dummy = as.factor(dummy)
#   )
# spdata_grouped <- groupedData(abs_tracking ~ 1 | dummy, spdata_with_dummy)
# nlme::lme(abs_tracking ~ temptrop + type + scale, random = pdBlocked(list(pdIdent(~ 0 + study), pdIdent(~ 0 + latin_name))), data = spdata_grouped, control = list(allow.n.lt.q = TRUE))

hist(resid(lmm_categorical))
summary(lmm_categorical)
confint(lmm_categorical)

emmeans(lmm_categorical, specs = "temptrop", pbkrtest.limit = 6200)  # to get model estimated means for tropics and temperate zone; this takes a long time to run
temptropmeans <- emmeans(glmm_categorical, specs = "temptrop", pbkrtest.limit = 6200) 
pairs(temptropmeans)

spdata$temptrop <- as.factor(spdata$temptrop)
spdata$temptrop <- relevel(spdata$temptrop, "trop")

ggplot(data = spdata, aes(x = temptrop, y = abs_tracking)) +
  geom_boxplot(fill = c("#D16103", "#4E84C4"), outlier.shape = NA) +
  geom_jitter(alpha = 0.3, size = 0.7, width = 0.2) +
  scale_y_continuous ("elevational shift (m/decade)", limits = c(-150, 350)) +
  scale_x_discrete ("", labels = c("tropical", "temperate"))
ggsave("Figure_sp_abs_tracking_long_boxplot.pdf", height = 4, width = 2)

# estimate from a model that does not differentiate between tropical and temperate...
lmm_categorical <- lmer(data = spdata, abs_tracking ~  type + scale + (1|study) + (1|latin_name))
summary(lmm_categorical)


# same but for absolute latitude

lmm_continuous <- lmer(abs_tracking ~ abs_latitude + type + scale +  (1|study) + (1|latin_name), data = spdata)
summary(lmm_continuous)

grpmeans <- emmeans(lmm_continuous, "type")
pairs(grpmeans) # contrasts between range types - different range types do not have significantly different contrasts

predictions <- emmeans(lmm_continuous, specs = c("abs_latitude"), at = list(abs_latitude = c(seq(from = 0, to = 64, by =1))), pbkrtest.limit = 6400) # this is an emmGrid object
predictions <- as.data.frame(predictions) # make this a dataframe to more easily plot predictions 
# this is the model-estimated value of absolute tracking at different latitudes, with confidence levels

ggplot(data = spdata, aes(y = abs_tracking , x = abs_latitude))+ 
  geom_ribbon(data = predictions, aes(x = abs_latitude, ymin = lower.CL, ymax = upper.CL), inherit.aes = FALSE, fill = "gray", alpha = 0.95) + 
  geom_point(size = 2, alpha = .5) + # can add size or color = type to plot different range types as different colors
  geom_line(data = predictions, aes(x = abs_latitude, y = emmean), linetype = "solid", color = "coral2", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(-150, 350), "elevational shift (m/decade)") + 
  scale_x_continuous("absolute latitude")
ggsave("sp_model_abs_tracking_predictions.pdf", height = 4, width = 6)




######
# analysis of temperature tracking (main analysis)
######

spdata <- spdata[spdata$tempchange > 0, ] # study is focused on response to warming temperatures (99.5% of data meets this criteria)

# summary statistics of spdata dataframe
dim(spdata) # 6141 rows
table(spdata$type) # numbers of lower limit, upper limit & mean elevation shifts
length(unique(spdata$study)) # 72 assemblages
length(unique(spdata$latin_name)) # 2950 sp
summary(spdata$duration) # with durations from 10 to 215 years
summary(spdata$tempchange) # with temp changes from .11 C to + 2.4 C ; mean = 0.93, median = 0.75
summary(spdata$responsiveness) # wide variation



summary(spdata$responsiveness[spdata$abs_latitude > 23.4]) # average temperature tracking score for temperate species based on raw data
summary(spdata$responsiveness[spdata$abs_latitude < 23.4]) # average temperature tracking score for tropical species based on raw data

# fit lmm to properly test differences between tropical and temperate (i.e., a simple t test would be inappropriate because need to include random effect & methodological covariates...)
spdata$type <- as.factor(spdata$type)
spdata$type <- relevel(spdata$type, "mean")

ttest_lmm <- lmer(data = spdata, responsiveness ~ temptrop + type + scale + duration + (1|study) + (1|latin_name))
summary(ttest_lmm)
confint(ttest_lmm)

emmeans(ttest_lmm, specs = "temptrop", pbkrtest.limit = 6200)  # to get model estimated means for tropics and temperate zone; this takes a long time to run
temptropmeans <- emmeans(ttest_lmm, specs = "temptrop", pbkrtest.limit = 6200) 
pairs(temptropmeans) # to get contrast between temperate and tropical

spdata$temptrop <- as.factor(spdata$temptrop)
spdata$temptrop <- relevel(spdata$temptrop, "trop")

spdata <- spdata[!is.na(spdata$responsiveness),] # some NA values crept in that mess up the boxplot

ggplot(data = spdata, aes(x = temptrop, y = responsiveness)) +
  geom_boxplot(fill = c("#D16103", "#4E84C4"), outlier.shape = NA) +
  geom_jitter(alpha = 0.05, size = 0.7, width = 0.2) +
  scale_y_continuous ("temperature tracking", limits = c(-7,7)) + # this shows 99% of data
  scale_x_discrete ("", labels = c("tropical", "temperate"))
ggsave("Figure_species_boxplot.pdf", height = 4, width = 2) 



#######
# fit model
#######

# response variable = responsiveness (= temperature tracking)
spdata$type <- as.factor(spdata$type)
spdata$type <- relevel(spdata$type, "mean")

lmm_sp1 <- lmer(data = spdata, responsiveness ~ abs_latitude + type + scale + duration + (1|study) + (1|latin_name))
summary(lmm_sp1)
confint(lmm_sp1)

r.squaredGLMM(lmm_sp1)


grpmeans <- emmeans(lmm_sp1, "type", pbkrtest.limit = 6200) # takes a loooong time to run
grpmeans
pairs(grpmeans) # contrasts between range types - different range types do not have significantly different contrasts

# note that the line below also takes a long time to run...
predictions <- emmeans(lmm_sp1, specs = c("abs_latitude"), at = list(abs_latitude = c(seq(from = 0, to = 64, by =1))), pbkrtest.limit = 6200) # this is an emmGrid object
predictions <- as.data.frame(predictions) # make this a dataframe to more easily plot predictions and confidence intervals

ggplot(data = spdata, aes(y = responsiveness, x = abs_latitude))+ 
  geom_ribbon(data = predictions, aes(x = abs_latitude, ymin = lower.CL, ymax = upper.CL), inherit.aes = FALSE, fill = "gray", alpha = 0.95) + 
  geom_point(size = 2, alpha = .1) + 
  geom_line(data = predictions, aes(x = abs_latitude, y = emmean), linetype = "solid", color = "coral2", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_continuous(limits = c(-7, 7), "temperature tracking") + # can make y limits narrower...
  scale_x_continuous("absolute latitude")
ggsave("species_model_predictions.pdf", height = 4, width = 6)


#####
# do same but only for birds
#####

spdata$taxa <- studydata$taxa[match(spdata$study, studydata$study)]
table(spdata$taxa) # 1991 birds in species level dataset
length(unique(spdata$latin_name[spdata$taxa=="bird"])) # 726 species
length(unique(spdata$study[spdata$taxa=="bird"])) # 19 studies of birds in speces level dataset
summary(spdata$abs_latitude[spdata$taxa=="bird"])

spdata$temptrop <- as.factor(spdata$temptrop)
spdata$temptrop <- relevel(spdata$temptrop, "trop")

ggplot(data = spdata[spdata$taxa=="bird",], aes(x = temptrop, y = responsiveness)) +
  geom_boxplot(fill = c("#D16103", "#4E84C4"), outlier.shape = NA) +
  geom_jitter(alpha = 0.05, size = 0.7, width = 0.2) +
  scale_y_continuous ("temperature tracking", limits = c(-7,7)) + # this shows 99% of data
  scale_x_discrete ("", labels = c("tropical", "temperate"))
ggsave("Figure_species_boxplot_birds.pdf", height = 4, width = 2) 


ttest_lmm_bird <- lmer(data = spdata[spdata$taxa=="bird",], responsiveness ~ temptrop + type + scale + duration + (1|study) + (1|latin_name))
summary(ttest_lmm_bird)
confint(ttest_lmm_bird)

lmm_sp_bird <- lmer(data = spdata[spdata$taxa=="bird",], responsiveness ~ abs_latitude + type + scale + duration + (1|study) + (1|latin_name))
r.squaredGLMM(lmm_sp_bird)
summary(lmm_sp_bird)
confint(lmm_sp_bird)

predictions <- emmeans(lmm_sp_bird, specs = c("abs_latitude"), at = list(abs_latitude = c(seq(from = 4, to = 54, by =1))), pbkrtest.limit = 5900) # this is an emmGrid object
predictions <- as.data.frame(predictions) # make this a dataframe to more easily plot predictions and confidence intervals

ggplot(data = spdata[spdata$taxa=="bird",], aes(y = responsiveness, x = abs_latitude))+ 
  geom_ribbon(data = predictions, aes(x = abs_latitude, ymin = lower.CL, ymax = upper.CL), inherit.aes = FALSE, fill = "gray", alpha = 0.95) + 
  geom_point(size = 2, alpha = .1) + # can add size or color = type to plot different range types as different colors
  geom_line(data = predictions, aes(x = abs_latitude, y = emmean), linetype = "solid", color = "coral2", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_continuous(limits = c(-7, 7), "temperature tracking") + # can make y limits narrower...
  scale_x_continuous("absolute latitude")
ggsave("species_model_predictions_bird.pdf", height = 4, width = 6)
