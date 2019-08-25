library(MASS)
library(ISLR)
library(fpp2)

#Problem 3-8
attach(Auto)
names(Auto)
mpg.lm <- lm(mpg ~ horsepower, data=Auto)
summary(mpg.lm)
plot(horsepower, mpg)
abline(mpg.lm, lwd=3)
Confint(mpg.lm)
names(mpg.lm)
mpg.pred <- predict(mpg.lm, data.frame(horsepower=c(98,99,100)), interval = "prediction")
mpg.pred <- predict(mpg.lm, data.frame(horsepower=c(98,99,100)), interval = "confidence")
mpg.pred
par(mfrow=c(2,2))
plot(mpg.lm)
#Problem 3-9
attach(Auto)
Auto %>% as.data.frame() %>% GGally::ggpairs()
names(Auto)
pairs(Auto[,1:9], pch = 19, lower.panel = NULL)
names(Auto)
my_data <- Auto[, c(1,2,3,4,5,6,7,8)]
(res <- cor(my_data))
lm.Auto.fit=lm(mpg~.-name,data=Auto) 
summary(lm.Auto.fit)
par(mfrow=c(2,2))
plot(lm.Auto.fit)
lm.Auto.fit.interact=lm(mpg~.-name+weight:horsepower+origin:weight,data=Auto) 
summary(lm.Auto.fit.interact)
lm.Auto.fit.nonlinear=lm(mpg~.-name+I(horsepower^2),data=Auto) 
summary(lm.Auto.fit.nonlinear)
checkresiduals(lm.Auto.fit.nonlinear)
#Problem 3-10
names(Carseats)
attach(Carseats)
lm.carseat <- lm(Sales ~ Price+Urban+US, data=Carseats)
contrasts(US)
summary(lm.carseat)
lm.carseat.smaller.model <- lm(Sales ~ Price+US, data=Carseats)
summary(lm.carseat.smaller.model)
anova(lm.carseat, lm.carseat.smaller.model)
confint(lm.carseat.smaller.model)
par(mfrow=c(2,2))
plot(lm.carseat.smaller.model)
#Problem 3-11
set.seed (1)
x=rnorm(100)
y=2*x+rnorm(100)
lm.y.no_intercept <- lm(y~x+0)
summary(lm.y.no_intercept)
lm.x.no_intercept <- lm(x~y+0)
summary(lm.x.no_intercept)
# With intercept
lm.y.with_intercept <- lm(y~x)
summary(lm.y.with_intercept)
lm.x.with_intercept <- lm(x~y)
summary(lm.x.with_intercept)

#Problem 3-12
#same coeff estimate
# Based on 3.38, to have the same coeff, the variance predictor, i.e., X
# should be the same as response, i.e., Y.
x=rnorm(100)
y=1*x
lm.y.no_intercept <- lm(y~x+0)
summary(lm.y.no_intercept)
lm.x.no_intercept <- lm(x~y+0)
summary(lm.x.no_intercept)

#Prblem 3-13
set.seed(1)
x<-rnorm(100, mean=0, sd=1)
eps<-rnorm(100, mean = 0, sd=0.25)
y<--1+0.5*x+eps
plot(x,y, pch=19)
lm.y <- lm(y~x)
summary(lm.y)
abline(lm.y, col = "blue")
legend(-2.5, 0, legend=c("Linear Regression"),
       col=c("blue"), lty=1, cex=0.8)
lm.y.2 <- lm(y~I(x^2))
summary(lm.y.2)
abline(lm.y.2, col = "red")
confint(lm.y)
confint(lm.y.2)

#Problem 3-14
set.seed(1)
x1=runif(100)
x2=0.5*x1+rnorm(100)/10
y=2+2*x1+0.3*x2+rnorm(100)
(cor(x1,x2))
plot(x1,x2,pch=19)
lm.y <- lm(y~x1+x2)
summary(lm.y)
lm.y.x1 <- lm(y~x1)
summary(lm.y.x1)
lm.y.x2 <- lm(y~x2)
summary(lm.y.x2)
x1=c(x1, 0.1)
x2=c(x2, 0.8)
y=c(y,6)
plot(x1,y)
lm.y.x1 <- lm(y~x1)
summary(lm.y.x1)
lm.y.x2 <- lm(y~x2)
summary(lm.y.x2)
par(mfrow=c(2,2))
plot(lm.y.x1)

#Problem 3-15
names(Boston)
attach(Boston)
summary(lm(crim~., data=Boston))
lm.fit3 <- lm(crim ~ poly(dis ,3)+poly(medv, 3), data=Boston)
summary(lm.fit3)

