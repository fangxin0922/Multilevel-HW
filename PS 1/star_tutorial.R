
# Analyzing Tenn STAR with cluster-robust Standard Errors
# A demo


##
## Loading the data
##


stud <- read.csv('stud_dat.csv') ## student-level data
teach <- read.csv('teach_dat.csv') ## teacher-level data

dat <- merge(stud, teach) ## merge the datasets. Automatically merges by clid, class id, the only variable in common

dat <- na.omit(dat) ## for simplicity, we'll delete everyone missing any variables

names(dat)
nrow( dat )

head(dat$tmathssk) ## math scores after kindergarten, the primary outcome

dat$small_class <- dat$cltypek == 'small class' ## create an indicator for being in a small class



##
## Estimating treatment effect with cluster-robust standard errors.
##

## The vanilla OLS estimate of tx impact (SEs are wrong)
mod <- lm(tmathssk ~ small_class, data = dat) 
summary(mod)


library( sandwich )
library( lmtest )
coeftest( mod, vcov. = vcovCL, cluster = dat$clid )



