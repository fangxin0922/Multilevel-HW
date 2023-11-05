##
## This script loads the cleaned PEDS data and looks at some simple descriptives.
##

options( digits=3 )

# load packages 
library(tidyverse)
library(arm)
library(readstata13)
library(ggsci) # for cool color pallets


dat = readRDS( "cleaned_data.rds" )
head( dat )

# aggregate data by time and tx group.
haz <- dat %>% group_by(EN, time.f, mtime) %>% 
  summarise(ch_haz = mean(ch_haz), 
            ch_stunted = mean(ch_stunted),
            .groups = "drop" )
haz 

# Plot two outcomes by time and treatment group  
ggplot(haz, aes(x=time.f, y=ch_haz, group=EN, color=EN)) +
  geom_line(lwd=1.2) + geom_point() +
  labs(x="Time", y="Average", 
       title="Average HAZ by intervention arm and time point",
       color="Intervention arm") +
  theme(legend.position="bottom")


ggplot(haz, aes(x=time.f, y=ch_stunted, group=EN, color=EN)) +
  geom_line(lwd=1.2) + geom_point() +
  labs(x="Time", y="Proportion", 
       title="Stunting by intervention arm and time point",
       color="Intervention arm") +
  theme(legend.position="bottom") + scale_colour_startrek() 



# Look at population differences in proportion stunted at different time points
# between tx groups at time points
head( haz )
haz.w = haz %>%
  pivot_wider( values_from = c(ch_haz, ch_stunted), 
               names_from = "EN" ) 

head( haz.w )
haz.w = mutate( haz.w, Delta_stunt = ch_stunted_Tx - ch_stunted_Co )
haz.w

## ----Simple multilevel model ------------------------------------------------------------
M1 = lmer(ch_haz ~ mtime * EN  + (1 + mtime|ch_id) + (1|lhw_id), 
              data=dat,
              control = lmerControl(optimizer = "bobyqa"))
summary(M1)

# Look at some of the random effects
qplot( coef(M1)$ch_id[[1]] )
qplot( coef(M1)$lhw_id[[1]] )

### Q5 ###
##add time.f and remove main effect of mtime

M2 = lmer(ch_haz ~ mtime:EN + EN + time.f + (1 + mtime|ch_id) + (1|lhw_id), 
          data=dat,
          control = lmerControl(optimizer = "bobyqa"))

summary(M2)

##Question 5 response: The final parameters include the random intercepts for each ch_id, the random slopes 
## for each mtime, and the random intercepts for each lhw_id. For fixed effects the parameters include
## the overall intercept, and parameter estimates for time.f6, time.f12, time.f18, time.f24 (reference group = 0 months), 
## the main effect for EN, and the interaction term mtime:ENCo. This is a total of 10 parameter estimates. 

### Q6 ###
groups = expand.grid( ch_id = -1,
lhw_id = -1,
time.f = unique( dat$time.f ),
EN = unique( dat$EN ) )
times = parse_number( levels( dat$time.f ) )
groups$mtime = times[ groups$time.f ]

predictions = predict(M2, newdata = groups, allow.new.levels =TRUE)

library(ggplot2)

##plot of just the predictions
p2 <- ggplot(groups, aes(x = time.f, y = predictions, color = factor(EN))) +
  geom_line(aes(group = interaction(ch_id, EN)))  +
  labs(title = "Treatment Trajectories for Hypothetical Children",
       x = "Time",
       y = "HAZ")  

## plot the predictions over the original plot from the exploratory section ## this is what needs to be turned in.
ggplot(haz, aes(x = time.f, y = ch_haz, group = EN, color = EN)) +
  geom_line(lwd = 0.1) + geom_point() +
  labs(x = "Time", y = "HAZ", 
       title = "Predicted and observed HAZ by intervention arm and time point",
       color = "Intervention arm", 
       caption = "Thick lines represent predicted values, thin lines represent observed values") +
  theme(legend.position = "bottom") +
  geom_line(data = groups, aes(x = time.f, y = predictions, color = factor(EN)), lwd = 1.5)

## Q6 response: The model seems to generally capture the overall trends in HAZ by treatment status; however, for the treatment group 
## there is a predicted sharper decrease in HAZ in the first 6 months than what was observed. 

### Q7 ### 
dat$pred <- predict(M2, dat)
set.seed(143)
index <- sample(unique(dat$ch_id), size=16)

subset <- dat[dat$ch_id %in% index,]

ggplot(data=subset)+
  geom_line(aes(x=time, y=ch_haz, group = EN, color="Observed"))+
  geom_line(aes(x=time, y=pred, group = EN, color="Predicted"))+
  facet_wrap( ~ ch_id )+
  labs(title = "A sample of 16 Childrien's HAZ", x="Time", y="HAZ")

## Q7 Response: The predicted values do appear to follow the observed data in a generally reasonable way, 
## there are individuals for which the predicted values are not great, but the general trends are fairly consistent. 

## Q8 ##

##a. Assess whether EN significantly impacts baseline HAZ. Does this result make sense?
#subset to just baseline measurements. We removed the random term for ch_id since we are only looking at a single measurement. 

# we kept the random intercept for lhw_id to account for clustering by lhw_id. According to this simple model, not controlling for any potential confounders,
## we find that EN is not significantly associated with baseline HAZ. With the limited knowledge I have about this dataset, yes this makes sense, as if we are looking cross-sectionally
##simply at baseline, it makes sense that the effect of the intervention (which would have also just started...) would not be reflected in the HAZ measurements.
baseline <- filter(dat, time == 1)

library(lmerTest)
M1 = lmer(ch_haz ~  EN + (1|lhw_id), 
          data=dat,
          control = lmerControl(optimizer = "bobyqa"))

summary(M1)

##b Assess whether EN significantly impacts child growth over time.
## Modelling the effect of EN over time with a mixed effect model, allowing for the effect of time on HAZ to vary by child ID (and a random intercept for each child ID and lcw ID),
## with fixed effects for the intervention, time, and their interaction, we find no significant association of EN on HAZ overtime. 
M2 = lmer(ch_haz ~ mtime:EN + EN + time.f + (1 + mtime|ch_id) + (1|lhw_id), 
          data=dat,
          control = lmerControl(optimizer = "bobyqa"))

summary(M2)

## c. No the estimated impact does not seem notable, and it is not significant. The parameter estimate for the interaction term, allowing the effect of EN to vary over time = 0.00302.
## we can also estimate the predicted HAZ at time 0-24 months for the treatment group vs the control group. We see that there is not a large difference between the two treatment groups. 
install.packages("ggeffects")
library(ggeffects)

ggpredict(M2, c("time.f", "EN"))
