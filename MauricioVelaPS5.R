library(foreign)

options(stringsAsFactors=F)
#set wd
setwd("C:/Users/MauricioAndresVela/Documents/R/Clase/PS5")

#load data
dat <- read.dta("anes_timeseries_2012_stata12.dta")

#Keep only some variables
dat <- dat[,c("ft_dpc", "econ_ecpast", "dem_hisp", "libcpre_self", "pid_self", "dem_racecps_black", "dem_edu")]

#recode levels of education

levels(dat$dem_edu)[3:10]= "No high school"
levels(dat$dem_edu)[4:5]= "High school"
levels(dat$dem_edu)[c(5:7,9)]= "Bachelor Degree"
levels(dat$dem_edu)[6:7]= "Master or higher degree"
levels(dat$dem_edu)[7]= "-8. Don't know" #assume other means don't know

#Replace values -2,-8 or -9 to NA for all variables

dat <- as.data.frame(lapply(dat, function(x) {
  replace(x,x %in% c(-2,-8,-9), NA)
  replace(x,substr(x, 1, 2) %in% c("-2","-8","-9"), NA)
}))


#Replace dependent variable to be value between 0 and 1
dat$ft_dpc <- dat$ft_dpc * 0.01

#Create test and training data
set.seed(123)
sel <- sample(x=1:nrow(dat), 500) #select 500 random obs to be taking out
train <- dat[-sel,]
test <- dat[sel,]





anes$libcpre_self <- as.numeric(anes$libcpre_self)
anes$libcpre_self[anes$libcpre_self < 4] <- NA
anes$libcpre_self <- anes$libcpre_self -3