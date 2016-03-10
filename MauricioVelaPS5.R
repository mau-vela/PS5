library(foreign)
library(mice)

options(stringsAsFactors=F)
#set wd
setwd("C:/Users/MauricioAndresVela/Documents/R/Clase/PS5")

##1
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

#Fix levels from factor variables
dat[,c("econ_ecpast", "dem_hisp", "pid_self", "dem_edu")]<- 
  lapply(dat[,c("econ_ecpast", "dem_hisp",  "pid_self",  "dem_edu")],
         function(x) as.factor(as.character(x)))  
       
       
#Put liberal-conservative scale as numeric
dat$libcpre_self <- as.numeric(substr(dat$libcpre_self, 1, 2))

#Create test and training data
set.seed(1234)
sel <- sample(x=1:nrow(dat), 500) #select 500 random obs to be taking out
train <- dat[-sel,]
test <- dat[sel,]

#Use multiple imputation for NA
micetrain <- mice(train, 10)
micetest <- mice(test, 10)

#Run lm model with only some variables
results1 <- lm.mids(ft_dpc ~ econ_ecpast+pid_self+dem_edu, 
                          data=micetrain)
#Run lm model with all variables selected
results2 <- lm.mids(ft_dpc ~ econ_ecpast+dem_hisp+libcpre_self+pid_self+dem_racecps_black+dem_edu, 
                    data=micetrain)
  
  
#show results
summary(results1)
summary(results2)

##2
#predict for test data using only one mice iteration
predicted1 <- predict(results1$analyses[[1]], complete(micetest,1))
predicted2 <- predict(results2$analyses[[1]], complete(micetest,1))

##3, 4, 5 and 6
#Create function to return statistics based on arguments (1) a vector of \true" observed outcomes (y), and (2) a matrix of predictions
fit_stats <- function(y, predmat, r=NULL,  s_RMSE=T,s_MAD=T,s_RMSLE=T,s_MAPE=T,s_MEAPE=T, s_MRAE=T){ 
  RMSE <- MAD <- RMSLE <- MAPE <- MEAPE <- MRAE <- NULL
  #calculate e
  e <- abs(predmat-y)
  #calculate a
  a <- 100*e/abs(y)
  #RMSE
  n <- nrow(predmat)
  if (s_RMSE==T) RMSE <- apply(e,2, function(x) sqrt(sum(x^2)/n))
  #MAD
  if (s_MAD==T) MAD <- apply(e, 2, median)
  #RMSLE
  if (s_RMSLE==T) RMSLE <- apply(predmat, 2, function(x) sqrt(sum((log(x+1) - log(y+1))^2))/n)
  #MAPE
  if (s_MAPE==T) MAPE <- apply(a, 2, function(x) sum(x)/n)
  # calculate MEAPE
  if (s_RMSE==T) MEAPE <- apply(a, 2, median)
  #MRAE
  if (s_MRAE==T & !is.null(r)) MRAE <- apply(e, 2, function(x) median(x/abs(r-y)))
  # combine all the stats
  output <- rbind(RMSE, MAD, RMSLE, MAPE, MEAPE, MRAE)
  return(output)
} 

#use the function with predictions 
#Create predict matrix
predmat <- matrix(c(predicted1, predicted2), ncol=2)
#observed y_i
y <- test[,"ft_dpc"]
#naive r
r <- rnorm(length(y),mean(y), sd(y))
fit_stats(y, predmat, r=r, s_MAD = F, s_MAPE=F)



