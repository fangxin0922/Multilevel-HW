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
  theme(legend.position="bottom") + scale_colour_startrek() 


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
display( M1 )


# Look at some of the random effects
qplot( coef(M1)$ch_id[[1]] )
qplot( coef(M1)$lhw_id[[1]] )


