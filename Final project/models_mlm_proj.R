library(dplyr)
library(tidyr)
##install.packages("xts")
library(xts)
##install.packages("lubridate")
library(lubridate)

pm <- read.csv("C:\\Users\\jer403\\Downloads\\final_ds.csv", header=TRUE, sep=",", na.strings=c("","NA"))
hemis <- read.csv("C:\\Users\\jer403\\Downloads\\hemisphere.csv", header=TRUE, sep=",", na.strings=c("","NA"))
popdens <- read.csv("C:\\Users\\jer403\\Downloads\\pop_density.csv", header=TRUE, sep=",", na.strings=c("","NA"))

# Merging datasets
pm <- merge(pm, hemis, by.x ="country", by.y ="Country.Name")
pm <- merge(pm, popdens, by.x = "country", by.y = "Country.Name")
pm <- rename(pm, "pop_density" = "X2020")

## create a time variable to account for trend
pm$date <- as.Date(pm$date, format = "%Y-%m-%d")
unique_dates <- sort(unique(pm$date))
pm$time_index <- match(pm$date, unique_dates)

## create a month variable to account for seasonality
pm$month <- month(pm$date)
pm$month <- as.factor(pm$month)

##descriptive statistics
#install.packages("tableone")
library(tableone)

library(tableone)
#install.packages("htmlTable")
library(htmlTable)

pm$hdicode <- as.factor(pm$hdicode)
pm <- pm %>%
  mutate(workplaces_reversed = -workplaces_percent_change_from_baseline)

selected_columns <- c("hdi_2020", "workplaces_reversed", 
                      "residential_percent_change_from_baseline", "a_mean", "pop_density")

my_table <- CreateTableOne(vars = selected_columns, 
                           strata = "hdicode", 
                           data = pm)

# Print the table
tab2Mat <- print(my_table, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tab2Mat, file = "myTable.csv")

## number of sites in each HDI
site_counts <- pm %>%
  group_by(hdicode, Mobility_SiteName) %>%
  summarise(n = n_distinct(Mobility_SiteName)) %>%
  summarise(total_sites = sum(n))

# Print the result
print(site_counts)

### testing ###

## model 1 is residential percent change
library(lme4)
#install.packages("gtsummary")
library(gtsummary)
pm$hdicode <- factor(relevel(pm$hdicode, ref = "Very High"), levels = c("Low", "Medium", "High", "Very High"))

x <- lmer(a_mean ~ workplaces_reversed*hdicode  + time_index + month*hemisphere + pop_density +
              (1 | Mobility_SiteName), control = lmerControl(optimizer = "bobyqa"),
     data = pm)

summary(x)

tbl <- tbl_regression(x)

### plot interaction
#install.packages("ggeffects")
library(ggeffects)
library(ggplot2)
interaction_effects <- ggeffect(x, c("workplaces_reversed", "hdicode"))
p <- plot(interaction_effects) 
p + ggtitle("Assocation between PM2.5 concentrations during the COVID-19 lockdowns and % reduction in workplace population") +
  labs(colour = "Human Development Index", x = "% reduction workplace population",
       y = "PM2.5 concentration (ug/m3)", caption = "Figure 1. Interaction plot estimating PM2.5 concentrations from changes in workplace mobility patterns across levels of HDI") 
  
### plot month*hemisphere just to see how its handling seasonality
interaction_effects2 <- ggeffect(x, c("month", "hemisphere"))
plot(interaction_effects2)

## plot random effects
library(sjPlot)
install.packages("glmmTMB")
library(glmmTMB)
custom_palette <- rainbow(length(unique(pm$Mobility_SiteName)))
plot_model(x, type = "re", 
           title = "Variability in estimated random intercepts across monitoring sites") +
  labs(x = "Monitoring site", y = "Estimated deviation in random intercept from overall mean",
       caption = "Figure 2. Variability in estimated deviations of random intercepts for PM2.5 across monitoring sites. 
       Each point represents the estimated difference in the random intercept for a specific mobility site from the overall mean intercept.")




##evaluating the variance components (required for the EDA)
null_model <- lme4:: lmer(a_mean ~ 1 + (1 | Mobility_SiteName), data = pm,
                   control = lmerControl(optimizer = "bobyqa"))
summary(null_model)

sjPlot::tab_model(null_model) ##ICC = 0.26


##model 2 
library(lmerTest)
y <- lmer(a_mean ~ residential_percent_change_from_baseline*hdicode  + time_index + month*hemisphere + pop_density +
            (1 | Mobility_SiteName), control = lmerControl(optimizer = "bobyqa"),
          data = pm)

summary(y)

## plot model 2
interaction_effects_mod2 <- ggeffect(y, c("residential_percent_change_from_baseline", "hdicode"))
p <- plot(interaction_effects_mod2) 
p + ggtitle("Assocation between PM2.5 concentrations during the COVID-19 lockdowns and increases in time spent at residential locations") +
  labs(colour = "Human Development Index", x = "% increase in time spent in residential locations",
       y = "PM2.5 concentration (ug/m3)", caption = "Figure 2. Interaction plot estimating PM2.5 concentrations from changes in residential mobility patterns across levels of HDI") 

