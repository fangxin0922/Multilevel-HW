##
## This script loads the PEDS data and saves the cleaned data to a nice file for further analysis.
##


# load packages 
library(tidyverse)
library(arm)
library(readstata13)
library(ggsci) # for cool color pallets


# load data 
dat <- read.dta13("peds.dta")
nrow( dat )


## ----levels--------------------------------------------------------------
# look at first few rows
head(dat)

# how many observations do we have at each level 
table(dat$time) # 5 time points at level 1
length(unique(dat$ch_id)) # 1489 kids at level 2 
length(unique(dat$lhw_id)) # 80 clusters at level 3 



# drop kids with missing treatment group
# 11 kids weren't eligible for the study
# 1 kid is missing treatment group, not sure why 
dat <- dat[complete.cases(dat[,1:4]),]



cid = sample( dat$ch_id, 2 )
two_child = filter( dat, ch_id %in% cid )
two_child

kids = dat %>% drop_na() %>%
  group_by( ch_id ) %>% 
  summarise( p_stunt = mean( ch_stunted ),
             n = n()
  )
table( kids$n )
qplot( kids$p_stunt)  



## ----make new vars------------------------------------------------------------
# create variables 
# time 
dat$mtime = (dat$time - 1)*6
dat$time.f <- as.factor(dat$mtime)
levels(dat$time.f)

# make levels have nice names
levels(dat$time.f) <- c("0 mo", "6 mo", "12 mo", "18 mo", "24 mo")

# treatment  
dat$treatment.f <- as.factor(dat$treatment)
levels(dat$treatment.f) <- c("RS", "EN", "RS+EN","Control")
dat$treatment.f <- relevel(dat$treatment.f, ref="Control")

# Using our original treatment coding:
dat$EN = ifelse( dat$treatment %in% c(1, 4), "Co", "Tx" )
dat$RS = ifelse( dat$treatment %in% c(2, 4), "Co", "Tx" )

# Check our releveling, etc.
table( dat$EN, dat$treatment.f )


## ----- missing data --------
dat <- filter( dat,
               !is.na( ch_stunted ),
               !is.na( ch_haz ) )



## ----- save dataset --------

table( dat$EN, dat$RS )

head( dat )
nrow( dat )

dat = dplyr::select( dat, -ch_height, -ch_weight )

saveRDS( dat, file="cleaned_data.rds" )



