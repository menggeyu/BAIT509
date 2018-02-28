genreg <- function(n){
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  eps <- rnorm(n)
  y <- 5-x1+2*x2+eps
  tibble(x1=x1, x2=x2, y=y)
}
library(tidyverse)
genreg(10)
# number of observations

#1
dat<-genreg(1000)

#2
dat<-mutate(dat,
            yhat=5,
            yhat1=5-x1,
            yhat2=5+2*x2,
            yhat12=5-x1+2*x2)
#3
mse<-mean((dat$yhat-dat$y)^2)
mse1<-mean((dat$yhat1-dat$y)^2)
mse2<-mean((dat$yhat2-dat$y)^2)
mse12<-mean((dat$yhat12-dat$y)^2)

mse
mse1
mse2
mse12

#Oracle classification
gencla <- function(n) {
  x <- rnorm(n) 
  pB <- 0.8/(1+exp(-x))
  y <- map_chr(pB, function(x) 
    sample(LETTERS[1:3], size=1, replace=TRUE,
           prob=c(0.2, x, 0.8-x)))
  tibble(x=x, y=y)
}

#1. When X=1

pB <- 0.8/(1+exp(-1))
pB
pA=0.2
pC=0.8-pB
pA
pB
pC

#look the mode to predict y

#When X=-2
pB1 <- 0.8/(1+exp(2))
pB1
pA1=0.2
pC1=0.8-pB
pA1
pB1
pC1

#3. generate data
dat2<-gencla(1000)
dat2
#4
dat2<-mutate(dat2,
             yhat=sapply(x,function(x_)
               if (x<0)"C" else"B"))
            

dat2
#5
1-mean(dat2$yhat==dat2$y)
