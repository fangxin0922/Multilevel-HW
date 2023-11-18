pm <- read.csv("C:\\Users\\jenni\\Downloads\\final_ds.csv", header=TRUE, sep=",", na.strings=c("","NA"))
hemis <- read.csv("C:\\Users\\jenni\\Downloads\\hemisphere.csv", header=TRUE, sep=",", na.strings=c("","NA"))
popdens <- read.csv("C:\\Users\\jenni\\Downloads\\pop_density.csv", header=TRUE, sep=",", na.strings=c("","NA"))

pm <- merge(pm, hemis, by.x ="country", by.y ="Country.Name")
pm <- merge(pm, popdens, by.x = "country", by.y = "Country.Name")
pm <- rename(pm, "pop_density" = "X2020")

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
##detach("package:lmerTest", unload = TRUE)
#install.packages("lme4")
library(lme4)
options(scipen = 999)
pm$hdicode <- as.factor(pm$hdicode)
pm <- pm %>%
  mutate(workplaces_reversed = -workplaces_percent_change_from_baseline)
pm$hdicode <- factor(relevel(pm$hdicode, ref = "Very High"), levels = c("Low", "Medium", "High", "Very High"))


x <- lmer(a_mean ~ workplaces_reversed*hdicode  + time_index + month*hemisphere + pop_density +
              (1 + time_index | Mobility_SiteName), control = lmerControl(optimizer = "bobyqa"),
     data = pm)

summary(x)

### plot interaction
##install.packages("ggeffects")
library(ggeffects)
library(ggplot2)
interaction_effects <- ggeffect(x, c("workplaces_reversed", "hdicode"))
p <- plot(interaction_effects) 
p + ggtitle("Assocation between change in # of people at workplaces and PM2.5 concentrations during the COVID-19 lockdowns") +
  labs(colour = "Human Development Index", x = "% Change from baseline in # people at workplaces",
       y = "PM2.5 concentration (ug/m3)", caption = "Figure 1. Interaction plot estimating PM2.5 concentrations from changes in workplace mobility patterns across levels of HDI") 
  
  ### plot month*hemisphere just to see how its handling seasonality
interaction_effects2 <- ggeffect(x, c("month", "hemisphere"))
plot(interaction_effects2)

## plot random effects
library(sjPlot)
custom_palette <- rainbow(length(unique(pm$Mobility_SiteName)))
plot_model(x, type="pred",
           terms=c("time_index", "Mobility_SiteName"),
           pred.type="re", ci.lvl = NA, colors = custom_palette,
           title = "Plot of random slopes for the effect of time on PM2.5 by site",
           legend.title = "Monitoring Site") +
  labs(caption = "Figure 2. Plot of random slopes for the effect of time on PM2.5 by monitoring site.
       This plot provides evidence of the differential effect of time on PM2.5 concentrations by site",
       x = "Time index (February-October 2020)",
       y = "PM2.5 concentration (ug/m3)")


##evaluating the variance components (required for the EDA)
null_model <- lme4:: lmer(a_mean ~ 1 + (1 | Mobility_SiteName), data = pm,
                   control = lmerControl(optimizer = "bobyqa"))
summary(null_model)

sjPlot::tab_model(null_model)
##ICC = 0.26
