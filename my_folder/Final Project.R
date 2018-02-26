###Import Data###
library(fpp)
mydata <- read.csv(file.choose(), header=TRUE)
str(mydata)

x<-data.frame(mydata$energy)
energy<-ts(x,start=1997 ,frequency = 12)
energy
y<-data.frame(mydata$total.passengers)
tpassengers<-ts(y,start=1997 ,frequency = 12)
z<-data.frame(mydata$total.area)
area<-ts(z,start=1997 ,frequency = 12)
p<-data.frame(mydata$mean.temp)
temperature<-ts(p,start=1997 ,frequency = 12)



#Explortory analysis
plot(energy, main="Monthly use of energy in YVR", ylab="use of energy")
Acf(energy,main="ACF plot of monthly use of energy in YVR")
seasonplot(energy, main = "Seasonal plot of energy use in YVR", year.labels.left = TRUE, col=1:5, pch=19)

plot(mydata$energy~mydata$mean.temp, xlab="mean temperature",ylab = "use of energy",main="Scatterplot of energy vs. mean temperature")
lines(lowess(mydata$mean.temp,mydata$energy),col="red")

plot(temperature, main="Time plot of mean temperature", ylab="temperature")


plot(mydata$energy~mydata$total.passengers, xlab="number of passengers",ylab = "use of energy",main="Scatterplot of energy vs. passengers")
lines(lowess(mydata$total.passengers,mydata$energy),col="red")

plot(tpassengers, main="Time plot of number of total passengers", ylab="number of passengers")

plot(mydata$energy~mydata$total.area, xlab="area of airport",ylab = "use of energy",main="Scatterplot of energy vs. area of YVR")
lines(lowess(mydata$total.area,mydata$energy),col="red")

plot(area, main="Time plot area", ylab="area")


#calendar transformation
monthdays <- rep(c(31,28,31,30,31,30,31,31,30,31,30,31),14) # there are 14 years of data, so repeat this 14 times
monthdays[38 + (4*12)*(0:2)] <- 29 # add leap years; extra day to every 4th February
plot(energy/monthdays, main="Average energy consumption in YVR per day", ylab="Energy use", xlab="Years")
#Box-Cox transformation
lambda <- BoxCox.lambda(energy) ; 
plot(BoxCox(energy,lambda), ylab="Transformed energy use",
     xlab="Year", main="Transformed monthly energy use in YVR")

#decomposition
energy1<-ts(mydata$energy,start=c(1997,1),frequency=12)
energy1
fit <- stl(energy1, s.window="periodic") # Seasonal and Trend decomposition using Loess
plot(fit,main="Decoposition of use of energy")


#Training set and test set
energy.training<-window(energy, start=1997,end=2008-0.05)
energy.training
energy.test<-window(energy, start=2008)
energy.test

#graoh with different methods
# Graph
plot(energy.training, ylab="Energy use", xlab="Year", main="Different forecast methods", xlim=c(1997, 2010),ylim=c(5000,8500))

lines(energy.test,col="chartreuse1")
lines(meanf(energy.training, h=36)$mean, col="deepskyblue4")
lines(naive(energy.training, h=36)$mean,col="brown3")
lines(rwf(energy.training, drift=TRUE, h=36)$mean, col="goldenrod3")
lines(snaive(energy.training, h=36)$mean,col="violetred4")
legend("topleft",lty=1,col=c("black","chartreuse1","deepskyblue4", "deepskyblue4","goldenrod3", "violetred4"), legend=c("Training set","Test set","Mean method","Naive method","Drift method", "Seasonal Naive method"))

energy.mean <- meanf(energy.training, h=36)
energy.naive <- naive(energy.training, h=36)
energy.drift <- rwf(energy.training, drift=TRUE, h=36)
energy.seasonalnaive <- snaive(energy.training, h=36)

rbind(accuracy(energy.mean, energy.test)[2,c(2,3,5,6)],
      accuracy(energy.naive, energy.test)[2,c(2,3,5,6)],
      accuracy(energy.drift, energy.test)[2,c(2,3,5,6)],
      accuracy(energy.seasonalnaive, energy.test)[2,c(2,3,5,6)])

#ETS model
sbest <- ets(energy.training)
sbest
plot(energy.training, ylab="Energy use", xlab="Year", main="ETS forecast methods", xlim=c(1997, 2010),ylim=c(5000,8500))
lines(energy.test,col="black")
energy.ets1 <- forecast(ets(energy.training, model="ANA", damped=FALSE),h=36)
lines(lines(fitted(sbest), col="blue",lty=2))
lines(energy.ets$mean, col="blue",lwd=3)
legend("topleft",lty=c(1,2,1),lwd=c(1,1,4),col=c("black","blue","blue"), legend=c("Training set/Test set","fitted value","forecast value"))
#AAA
plot(energy.training, ylab="Energy use", xlab="Year", main="ETS forecast methods", xlim=c(1997, 2010),ylim=c(5000,8500))
lines(energy.test,col="black")
energy.aaa<-ets(energy.training, model="AAA", damped=FALSE)
energy.aaa
energy.ets <- forecast(ets(energy.training, model="AAA", damped=FALSE),h=36)
lines(lines(fitted(energy.aaa), col="blue",lty=2))
lines(energy.ets$mean, col="blue",lwd=3)
legend("topleft",lty=c(1,2,1),lwd=c(1,1,4),col=c("black","blue","blue"), legend=c("Training set/Test set","fitted value","forecast value"))

accuracy(energy.ets, energy.test)
accuracy(energy.mean, energy.test)

#training set
rbind(accuracy(energy.ets1, energy.test)[1,c(2,3,5,6)],
      accuracy(energy.ets, energy.test)[1,c(2,3,5,6)],
      accuracy(energy.drift, energy.test)[1,c(2,3,5,6)])

#test set
rbind(accuracy(energy.ets1, energy.test)[2,c(2,3,5,6)],
      accuracy(energy.ets, energy.test)[2,c(2,3,5,6)],
      accuracy(energy.drift, energy.test)[2,c(2,3,5,6)])
    

#Residual diagnostics
res <- residuals(energy.ets) # residuals from the multiplicative damped trend method fitted to the training set

plot(res, main="Residuals from ETS(A,A,A)", ylab="", xlab="Year")
abline(0,0, lty=2)
Acf(res, main="ACF plot of residuals from ETS(A,A,A)",lag.max = 48)

Box.test(res, type="Ljung", lag=24)
hist(res,main="Histogram of residuals from ETS(A,A,A)",breaks = 10,xlim=c(-400,600),ylim = c(0,50))
mean(res)

#ARIMA model
#1
diff1<-diff(energy.training,12)
diff1
plot(diff1,main="Time plot after seasonal differencing")
Acf(diff1,main="ACF plot after seasonal differencing",lag.max = 60)
diff2<-diff(diff1)
plot(diff2,main="Time plot after seasonal & first order differencing")
Acf(diff2,main="ACF plot after seasonal & first order differencing",lag.max = 60)
Pacf(diff2, main="PACF plot after seasonal & first order differencing",lag.max = 60)

energy.fit.1 <- auto.arima(energy.training,D=1, stepwise=FALSE, approximation=FALSE)
summary(energy.fit.1)

energy.fit.2<-Arima(energy.training, order=c(0,1,1), seasonal=c(0,1,1), include.drift=FALSE, include.constant = TRUE)
summary(energy.fit.2)

energy.fit.3<-Arima(energy.training, order=c(0,1,1), seasonal=c(1,1,0), include.drift=FALSE, include.constant = TRUE)
summary(energy.fit.3)

energy.fit.4<-Arima(energy.training, order=c(0,1,1), seasonal=c(1,1,1), include.drift=FALSE, include.constant = TRUE)
summary(energy.fit.4)

energy.fit.5<-Arima(energy.training, order=c(1,1,0), seasonal=c(0,1,1), include.drift=FALSE, include.constant = TRUE)
summary(energy.fit.5)

energy.fit.6<-Arima(energy.training, order=c(1,1,0), seasonal=c(1,1,0), include.drift=FALSE, include.constant = TRUE)
summary(energy.fit.6)

energy.fit.7<-Arima(energy.training, order=c(1,1,0), seasonal=c(1,1,1), include.drift=FALSE, include.constant = TRUE)
summary(energy.fit.7)

energy.fit.8<-Arima(energy.training, order=c(1,1,1), seasonal=c(0,1,1), include.drift=FALSE, include.constant = TRUE)
summary(energy.fit.8)

energy.fit.9<-Arima(energy.training, order=c(1,1,1), seasonal=c(1,1,0), include.drift=FALSE, include.constant = TRUE)
summary(energy.fit.9)

energy.arima1 <- forecast(energy.fit.2,h=36)
energy.arima2 <- forecast(energy.fit.3,h=36)
energy.arima3 <- forecast(energy.fit.4,h=36)
energy.arima4 <- forecast(energy.fit.5,h=36)
energy.arima5 <- forecast(energy.fit.6,h=36)
energy.arima6 <- forecast(energy.fit.7,h=36)
energy.arima7 <- forecast(energy.fit.8,h=36)
energy.arima8 <- forecast(energy.fit.9,h=36)

rbind(accuracy(energy.arima1, energy.test)[2,c(2,3,5,6)],
      accuracy(energy.arima2, energy.test)[2,c(2,3,5,6)],
      accuracy(energy.arima3, energy.test)[2,c(2,3,5,6)],
      accuracy(energy.arima4, energy.test)[2,c(2,3,5,6)],
      accuracy(energy.arima5, energy.test)[2,c(2,3,5,6)],
      accuracy(energy.arima6, energy.test)[2,c(2,3,5,6)],
      accuracy(energy.arima7, energy.test)[2,c(2,3,5,6)],
      accuracy(energy.arima8, energy.test)[2,c(2,3,5,6)])

#2
plot(energy.training, ylab="Energy use", xlab="Year", main="ARIMA(1,1,0)(0,1,1) model", xlim=c(1997, 2010),ylim=c(5000,8500))
lines(energy.test,col="black")
energy.arima <- forecast(energy.fit.5,h=36)
lines(lines(fitted(energy.fit.5), col="blue",lty=2))
lines(energy.arima4$mean, col="blue",lwd=3)
legend("topleft",lty=c(1,2,1),lwd=c(1,1,4),col=c("black","blue","blue"), legend=c("Training set/Test set","fitted value","forecast value"))

#5
rbind(accuracy(energy.ets, energy.test)[2,c(2,3,5,6)],
      accuracy(energy.mean, energy.test)[2,c(2,3,5,6)],
      accuracy(energy.naive, energy.test)[2,c(2,3,5,6)],
      accuracy(energy.drift, energy.test)[2,c(2,3,5,6)],
      accuracy(energy.seasonalnaive, energy.test)[2,c(2,3,5,6)],
      accuracy(energy.arima, energy.test)[2,c(2,3,5,6)])

#6
res1 <- residuals(energy.arima4) 

plot(res1, main="Residuals from ARIMA(1,1,0)(0,1,1) model", ylab="", xlab="Year")
abline(0,0, lty=2)
Acf(res1, main="ACF plot of residuals from ARIMA(1,1,0)(0,1,1) model",lag.max = 60)

Box.test(res1, type="Ljung", lag=24)
hist(res1,main="Histogram of residuals from ARIMA(1,1,0)(0,1,1) model",breaks = 10,xlim=c(-400,600),ylim = c(0,50))
mean(res1)

#compare method
rbind(accuracy(energy.ets, energy.test)[2,c(2,3,5,6)],
      accuracy(energy.mean, energy.test)[2,c(2,3,5,6)],
      accuracy(energy.naive, energy.test)[2,c(2,3,5,6)],
      accuracy(energy.drift, energy.test)[2,c(2,3,5,6)],
      accuracy(energy.seasonalnaive, energy.test)[2,c(2,3,5,6)],
      accuracy(energy.arima4, energy.test)[2,c(2,3,5,6)])

#final model
energy.final<-ets(energy, model="AAA", damped=FALSE)
energy.final
energy.ets.final <- forecast(ets(energy, model="AAA", damped=FALSE),h=36)
energy.ets.final
energy.ets.final$mean

plot(energy, ylab="Energy use", xlab="Year", main="Forecast by ETS(A,A,A) model", xlim=c(1997, 2013),ylim=c(5000,10000))
lines(energy.ets.final$mean, col="blue")
legend("topleft",lty=c(1,1),lwd=c(1,1),col=c("black","blue"), legend=c("Entire data","forecast value"))
