rm(list=ls())
setwd("/home/matt/Documents/EEE4022S Project/Stats Analysis")
dir()
#Section A
require(MASS)
require(plyr)
library(plyr)

pilot_data<-read.csv("Charging Circuit Data.csv")

#Diagnostic Plotting

par(mfrow = c(1,1))
plot(log(pilot_data$Value) ~ pilot_data$Time, data = pilot_data)
hist(log(pilot_data$Value))

#Fitting Models 

model.m1<-lm(exp(-Value)~Time, data = pilot_data)
model.m2<-lm(exp(Value)~Time, data = pilot_data)
model.m3<-lm(log(Value)~Time, data = pilot_data)
model.m4<-lm(Value~log(Time)+I(log(Time)^2), data = pilot_data)

AIC(model.m1, model.m2, model.m3, model.m4)

summary(model.m1)
summary(model.m2)
summary(model.m3)
summary(model.m4)

#Model Plots

Time=seq(from=1,to=92,length.out=1000)
y=predict(model.m4,newdata=data.frame(Time),interval="confidence")
Value = 13.65
x=predict(model.m4, newdata = data.frame(Value),interval = "prediction")

dev.off()
matplot(Time, y, type = "l", lty = 1, lwd = 2,
        ylim = c(12.86, 12.99), col = c("black", "red","green"),
        cex.lab = 0.8, cex.axis = 0.7, las =1,
        xlab = "Time /minutes",
        ylab = "Battery Voltage /V",
        main = "Points plot of Charging Voltage")
points(pilot_data$Value ~ pilot_data$Time)

summary(model.m4)
png('Charging_Data.png')
dev.copy(png,'charging_data.png')
dev.off()

