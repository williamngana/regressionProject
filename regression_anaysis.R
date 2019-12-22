directory <- 'C:/Users/William James Ngana/Desktop/Qualifying Year/Regression/Project/Project_data_Cars.csv' #this is here the data is located on my computer
CarsData <- read.csv(directory, header = TRUE)# loading the data into R

mileage<-CarsData$Mileage #set x as the milage data
price<-CarsData$Price  #set y as the price data

# next par plots the actual graph with x lables and y labes
plot(mileage,price, main="Plot of the Mileage vs Price",
     xlab = "Car Milage(Miles)", ylab = "Car Price($)", pch=16)
## The next part of this activity asks that I calculate the least sqares reression line
## as well as R^2 value the correlation coefficient, the t-ststistic and the p-valies

## this part I'm calculating the regression line
cars.lm<-lm(price~mileage) #fisrt calculate the linear model with the lm function
abline(cars.lm,col= "red") # this plots the regresson line
modelSummary <- summary(cars.lm) 
modelCoeffs <- modelSummary$coefficients #the cofficents
beta.estimate <- modelCoeffs["mileage", "Estimate"]  # get beta estimate for speed
std.error <- modelCoeffs["mileage", "Std. Error"]  # get std.error for speed
t_value <- beta.estimate/std.error  # calc t statistic

## now I am determninig the r^2 value
rSquared<-modelSummary$r.squared #from the linear model find the r^2 value
rSquared # this shows the r^2 value
#Corelation coefficent
cor_co<-cor(price,mileage)

## Actiity 2
mileage<-CarsData$Mileage #set x as the milage data
price<-CarsData$Price  #set y as the price data
make<- CarsData$Make
model<-CarsData$Model
trim<-CarsData$Trim
type <-CarsData$Type
cyl <-CarsData$Cyl
liter<-CarsData$Liter
doors<-CarsData$Doors
cruise<-CarsData$Cruise
sound<-CarsData$Sound
leather<-CarsData$Leather
#4

# A) Calculating the regerssion models of each explanatory variables
cars2.lm<-lm(price~cyl)
rcyl<-summary(cars2.lm)$r.squared
cars3.lm<-lm(price~liter)
rliter<-summary(cars3.lm)$r.squared
cars4.lm<-lm(price~doors)
rdoors<-summary(cars4.lm)$r.squared
cars5.lm<-lm(price~cruise)
rcruise<-summary(cars5.lm)$r.squared
cars6.lm<-lm(price~sound)
rsound<-summary(cars6.lm)$r.squared
cars7.lm<-lm(price~leather)
rleather<-summary(cars7.lm)$r.squared
cars.lm<-lm(price~mileage)
rmile<-summary(cars.lm)$r.squared 


# B)Calculating six regession models 
newcars.lm <-lm(price~cyl+liter)
rcliter<-summary(newcars.lm)$r.squared

newcars1.lm <-lm(price~cyl+doors)
rcprice<-summary(newcars1.lm)$r.squared

newcars2.lm <-lm(price~cyl+cruise)
rccruise<-summary(newcars2.lm)$r.squared

newcars3.lm <-lm(price~cyl+sound)
rcsound<-summary(newcars3.lm)$r.squared

newcars4.lm <-lm(price~cyl+leather)
rcleather<-summary(newcars4.lm)$r.squared

newcars5.lm <-lm(price~cyl+mileage)
rcmile<-summary(newcars5.lm)$r.squared

#C) using R to condunct a stepwise regerssion procedure
library(MASS)
full.model <- lm(price~mileage+cyl+liter+doors+sound+leather+cruise)
step.model <- stepAIC(full.model, direction="both",trace = FALSE)
summary(step.model)
summary(full.model)

#5
library(tidyverse)
library(caret)
library(leaps)



models<- regsubsets(price~mileage+cyl+liter+doors+sound+leather+cruise,data = CarsData, nvmax=7)
summary(models)
## code used to determine the best 
res.sum <- summary(models)
data.frame(
  Adj.R2 = which.max(res.sum$adjr2))


## Activity 3

## For question 7 we are first gonna use the regression equation calcuater in question 5
final.model<-lm(price~cyl+cruise+leather+mileage+doors+sound)
model.res <-resid(final.model)
model.pred <- predict(final.model)
par(mar = rep(2, 4))
## this was used to draw all the plots of the residual vs variable. the variable was just switched
## and the titles and labels changed
plot(mileage,model.res, main="Plot of Residual vs Mileage (vertical line at mileage=8000)",
     xlab = "Car Milage(Miles)", ylab = "Residual", pch=10)
abline(v=8000,col='red') #only run this when looking at the milege plot

## for question 8 we are transforming the price to log(price) and sqrt(price)

#first I'm transforming it to log(price)
log.price<-mutate(CarsData,Price=log(Price))$Price
#now transform it to sqrt(price)
sqrt.price<-mutate(CarsData,Price=sqrt(Price))$Price

## now I will create a linear model for both using the equation in question
log.model<-lm(log.price~cyl+cruise+leather+mileage+doors+sound)
log.model.res <-resid(log.model)
log.model.pred <- predict(log.model)
log.model.r <- summary(log.model)$r.squared

plot(mileage,log.model.res, main="Plot of Residual vs Mileage For the Log Transformation",
     xlab = "Car Milage(Miles)", ylab = "Residual", pch=10)

plot(cyl,log.model.res, main="Plot of Residual vs Cyl For the Log Transformation",
     xlab = "Car Milage(Miles)", ylab = "Residual", pch=10)

plot(cruise,log.model.res, main="Plot of Residual vs Cruise For the Log Transformation",
     xlab = "Car Milage(Miles)", ylab = "Residual", pch=10)

plot(leather,log.model.res, main="Plot of Residual vs Leather For the Log Transformation",
     xlab = "Car Milage(Miles)", ylab = "Residual", pch=10)

plot(doors,log.model.res, main="Plot of Residual vs Doors For the Log Transformation",
     xlab = "Car Milage(Miles)", ylab = "Residual", pch=10)

plot(sound,log.model.res, main="Plot of Residual vs Sound For the Log Transformation",
     xlab = "Car Milage(Miles)", ylab = "Residual", pch=10)

plot(log.model.pred,log.model.res, main="Plot of Residual vs Predicted Value For the Log Transformation",
     xlab = "Car Milage(Miles)", ylab = "Residual", pch=10)

sqrt.model<-lm(sqrt.price~cyl+cruise+leather+mileage+doors+sound)
sqrt.model.res <-resid(sqrt.model)
sqrt.model.pred <- predict(sqrt.model)
sqrt.model.r <- summary(sqrt.model)$r.squared

plot(mileage,sqrt.model.res, main="Plot of Residual vs Mileage For the Square-root Transformation",
     xlab = "Car Milage(Miles)", ylab = "Residual", pch=10)

plot(cyl,sqrt.model.res, main="Plot of Residual vs Cyl For the Square-root Transformation",
     xlab = "Car Milage(Miles)", ylab = "Residual", pch=10)

plot(cruise,sqrt.model.res, main="Plot of Residual vs Cruise For the Square-root Transformation",
     xlab = "Car Milage(Miles)", ylab = "Residual", pch=10)

plot(leather,sqrt.model.res, main="Plot of Residual vs Leather For the Square-root Transformation",
     xlab = "Car Milage(Miles)", ylab = "Residual", pch=10)

plot(doors,sqrt.model.res, main="Plot of Residual vs Doors For the Square-root Transformation",
     xlab = "Car Milage(Miles)", ylab = "Residual", pch=10)

plot(sound,sqrt.model.res, main="Plot of Residual vs Sound For the Square-root Transformation",
     xlab = "Car Milage(Miles)", ylab = "Residual", pch=10)

plot(sqrt.model.pred,sqrt.model.res, main="Plot of Residual vs Predicted Value For the Square-root Transformation",
     xlab = "Car Milage(Miles)", ylab = "Residual", pch=10)

# Activity 4
#final.model
#This code will identfy the outliers
CarsData$cooksd <- cooks.distance(final.model)
CarsData$outlier <- ifelse(CarsData$cooksd < 4/nrow(CarsData),'keep','delete')
outs <- filter(CarsData, outlier == "delete") # this filters out the outliers and make a data of them
newCarsData <- filter(CarsData, outlier == "keep"& Make == "Cadillac" & Type == "Convertible") # this is the data with the outliers removed

## now preform the anaylis over
nmileage<-newCarsData$Mileage #set x as the milage data
nprice<-newCarsData$Price  #set y as the price data
nmake<- newCarsData$Make
nmodel<-newCarsData$Model
ntrim<-newCarsData$Trim
ntype <-newCarsData$Type
ncyl <-newCarsData$Cyl
nliter<-newCarsData$Liter
ndoors<-newCarsData$Doors
ncruise<-newCarsData$Cruise
nsound<-newCarsData$Sound
nleather<-newCarsData$Leather

## new analysis for the data without outliers
library(MASS)
nfull.model <- lm(nprice~nmileage+ncyl+nliter+ndoors+nsound+nleather+ncruise  )
nstep.model <- stepAIC(nfull.model, direction="both",trace = FALSE)
summary(nstep.model)
summary(nfull.model)

#5
library(tidyverse)
library(caret)
library(leaps)



nmodels<- regsubsets(nprice~nmileage+ncyl+nliter+ndoors+nsound+nleather+ncruise,data = newCarsData, nvmax=7)
summary(nmodels)
## code used to determine the best 
res.sum <- summary(nmodels)
data.frame(
  Adj.R2 = which.max(res.sum$adjr2))

newfit<- lm(price~ liter+cruise+leather+sound+mileage+doors)
summary(newfit)
