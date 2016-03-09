library(foreign)

options(stringsAsFactors=F)
#set wd
setwd("C:/Users/MauricioAndresVela/Documents/R/Clase/PS5")

#load data
dat <- read.dta("anes_timeseries_2012_stata12.dta")

#Keep only some variables
dat <- dat[,c("ft_dpc", "econ_ecpast", "dem_hisp", "libcpre_self", "pid_self", "dem_racecps_black", "dem_edu")]

#recode levels

levels(dat$dem_edu)[3:10]= "No high school"
levels(dat$dem_edu)[4:5]= "High school"
levels(dat$dem_edu)[c(5:7,9)]= "Bachelor Degree"
levels(dat$dem_edu)[6:7]= "Master or higher degree"
levels(dat$dem_edu)[7]= "-8. Don't know" 



anes$libcpre_self <- as.numeric(anes$libcpre_self)
anes$libcpre_self[anes$libcpre_self < 4] <- NA
anes$libcpre_self <- anes$libcpre_self -3