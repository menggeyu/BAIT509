#Exercise 3.1
library(tidyverse)
library(knitr)
set.seed(87)
dat <- tibble(x = c(rnorm(100), rnorm(100)+5)-3,
              y = sin(x^2/5)/x + rnorm(200)/10 + exp(1))
kable(head(dat))

#1. Eyeball the above scatterplot of the data. What would you say is a reasonable estimate of the mean of Y at X=0, Why?
# Since the scatterplot is central symmetry around 0

#2. Estimate using loess and kNN (you choose the hyperparameters).
#kNN k=5
k=5
dat$d <- abs(dat$x-0)
dat1<-arrange(dat,dat$d)
sub1<-dat1[1:k,]
sub1
y.pre<-mean(sub1$y)
y.pre

#loess
dat.loess<-filter(dat,dat$d<1)
y.pre2<-mean(dat.loess$y)
y.pre2

#3. when r=0.01, there is no observation and we can not make the prediction

#4.The trade off
#small value-- overfit the data, we have small sample, we will have highly variable. The bias will be small.
#lager value-- maybe all of the data will be taken into consideration. The variance is small. The bias will be so high.

#Exercise 3.2
library(tidyverse)
xgrid <- seq(-5, 4, length.out=1000)
kNN_estimates <- map_dbl(xgrid, function(t){
  dat$distance<-abs(dat$x-t)
  dat.arrange<-arrange(dat,dat$distance)
  sub2<-dat.arrange[1:5,]
  yhat<-mean(sub2$y)
  return(yhat)
  ## YOUR CODE HERE FOR kNN
  ## Note: The variable "x" here is a single value along the grid.
  ## Hint1: Extend your code for kNN from the previous exercise.
  ## Hint2: Say you store the prediction in the variable "yhat".
  ##         Then in a new line of code, write: return(yhat)
})
loess_estimates <- map_dbl(xgrid, function(t){
  dat$distance<-abs(dat$x-t)
  dat.loess2<-filter(dat,dat$distance<1)
  yhat2<-mean(dat.loess2$y)
  return(yhat2)
  ## YOUR CODE HERE FOR LOESS
  ## Note: The variable "x" here is a single value along the grid.
  ## Hint1: Extend your code for loess from the previous exercise.
  ## Hint2: Say you store the prediction in the variable "yhat".
  ##         Then in a new line of code, write: return(yhat)
})
est <- tibble(x=xgrid, kNN=kNN_estimates, loess=loess_estimates) %>% 
  gather(key="method", value="estimate", kNN, loess)
ggplot() +
  geom_point(data=dat, mapping=aes(x,y), colour="orange") +
  geom_line(data=est, 
            mapping=aes(x,estimate, group=method, colour=method)) +
  theme_bw()


