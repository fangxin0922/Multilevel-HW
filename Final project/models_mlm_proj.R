pm <- read.csv("C:\\Users\\jenni\\Downloads\\final_ds.csv", header=TRUE, sep=",", na.strings=c("","NA"))
hemis <- read.csv("C:\\Users\\jenni\\Downloads\\hemisphere.csv", header=TRUE, sep=",", na.strings=c("","NA"))

pm <- merge(pm, hemis, by.x ="country", by.y ="Country.Name")

library(dplyr)
library(tidyr)
##install.packages("xts")
library(xts)
##install.packages("lubridate")
library(lubridate)

## create a time variable to account for trend
pm$date <- as.Date(pm$date, format = "%Y-%m-%d")
unique_dates <- sort(unique(pm$date))
pm$time_index <- match(pm$date, unique_dates)

## create a month variable to account for seasonality
pm$month <- month(pm$date)
pm$month <- as.factor(pm$month)

## testing ##

## model 1 is residential percent change
library(lmerTest)
detach("package:lmerTest", unload = TRUE)
#install.packages("lme4")
library(lme4)
pm$hdicode <- as.factor(pm$hdicode)
pm$hdicode <- factor(pm$hdicode, levels = c("Low", "Medium", "High", "Very High"))
pm$hdicode <- relevel(pm$hdicode, ref = "Very High")
pm <- pm %>%
  mutate(week_index = ceiling(time_index / 7))
pm <- pm %>%
  mutate(year_index = ceiling(time_index / (52 * 7)))
pm <- pm %>%
  mutate(workplaces_reversed = -workplaces_percent_change_from_baseline)

x <- lmer(a_mean ~ workplaces_reversed*hdicode  + time_index + month*hemisphere +
              (1 + time_index | Mobility_SiteName), control = lmerControl(optimizer = "bobyqa"),
     data = pm)

summary(x)

### plot interaction
##install.packages("ggeffects")
library(ggeffects)
library(ggplot2)
interaction_effects <- ggeffect(x, c("workplaces_reversed", "hdicode"))
plot(interaction_effects)

### plot month*hemisphere just to see how its handling seasonality
interaction_effects2 <- ggeffect(x, c("month", "hemisphere"))
plot(interaction_effects2)

## plot random effects
library(sjPlot)
custom_palette <- rainbow(length(unique(pm$Mobility_SiteName)))
plot_model(x, type="pred",
           terms=c("time_index", "Mobility_SiteName"),
           pred.type="re", ci.lvl = NA, colors = custom_palette)
