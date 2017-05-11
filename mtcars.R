library(car)
library(ggplot2)
library(dplyr)
#library(GGAlly)
data(mtcars)
#mtcars$cyl <- factor(mtcars$cyl)
#mtcars$vs <- factor(mtcars$vs)
#mtcars$gear <- factor(mtcars$gear)
#mtcars$carb <- factor(mtcars$carb)
#mtcars$am <- factor(mtcars$am, labels = c("Automatic", "Manual"))
str(mtcars)


fit <- lm(mpg~., mtcars)
vif(fit)
fit2 <-lm(am~., mtcars)
vif(fit2)

mtcars<-mutate(mtcars, acc.wt = qsec/wt)
mtcars<-mutate(mtcars, hp.wt = hp/wt)
mtcars<-mutate(mtcars, d.disp = disp/wt)
mtcars<-mutate(mtcars, hp.qsec.sq = wt/sqrt(qsec))
#best <- step(lm(mpg ~ ., data = mtcars), direction = "both")

cor(mtcars$am, mtcars$mpg)

boxplot(mpg ~ am, data = mtcars, xlab = "Transmission Type", ylab = "Miles per gallon", main="Miles per gallon by Transmission Type")

boxplot(mpg ~ am, data = mtcars)


best = step(lm(mpg ~ ., mtcars),trace=0,steps=1000)$coefficients
summary(stepmodel)
plot(mtcars$wt, mtcars$qsec)
abline(mtcars$wt, mtcars$qsec)

plot(mtcars$wt, mtcars.resid, ylab="Residuals", xlab="Weight", main="mtcars weight") 


ggplot(mtcars, aes(x=wt, y=qsec)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()            # Add a loess smoothed fit curve with confidence region

