rm(list=ls())
setwd("/home/matt/Documents/EEE4022S Project/Stats Analysis")
dir()
#Section A
require(MASS)
require(plyr)
library(plyr)

raw_data_1<-read.csv("RAW Flume Data 1.csv")
raw_data_2<-read.csv("RAW Flume Data 2.csv")
raw_data_3<-read.csv("RAW Flume Data 3.csv")

head(raw_data_1)
head(raw_data_2)

#Diagnostic Plots
par(mfrow = c(1,1))
hist(raw_data_2$Pump.Deviation..m3s.1, xlab = "Flow in m3/s", main = "Deviation of Flowmeter from Calculated Values")
sd(as.numeric(raw_data_2$Pump.Deviation..m3s.1), na.rm = T)
mean(as.numeric(raw_data_2$Pump.Deviation..m3s.1), na.rm = T)

par(mfrow = c(2,2))

hist(raw_data_1$Dev.Pump.m3, xlab = "Flow in m3/s", main = "Deviation of Arduino from Flowmeter")
hist(raw_data_1$Dev.Weir.m3 , xlab = "Flow in m3/s", main = "Deviation of Arduino from Weir Calculations")
hist(raw_data_1$Pump.Weir.m3, xlab = "Flow in m3/s", main = "Deviation of Pump Flow from Calculated Values")

par(mfrow = c(2,2))

hist(raw_data_3$Dev.Pump.m3, xlab = "Flow in m3/s", main = "Deviation of Arduino from Flowmeter")
hist(raw_data_3$Dev.Weir.m3 , xlab = "Flow in m3/s", main = "Deviation of Arduino from Weir Calculations")
hist(raw_data_3$Pump.Weir.m3, xlab = "Flow in m3/s", main = "Deviation of Pump Flow from Calculated Values")


#Fitting to Dataset 1
data1.model1 <- lm(Flow.m3 ~ Arduino.Flow.m3, data=raw_data_1)
summary(data1.model1)
par(mfrow = c(1,1))
plot(raw_data_1$Flow.m3 ~ raw_data_1$Arduino.Flow.m, data=raw_data_1, xlab = "Flow Rate in m3/s", ylab = "Flow Rate in m3/s", main = "Measured Flow vs Arduino Calulation")
abline(data1.model1, col = "red")

par(mfrow = c(2,2))
plot(data1.model1)


data1.model2 <- lm(raw_data_1$Flow.m3[c(2:14,5)] ~ raw_data_1$Arduino.Flow.m[c(2:14,6)])
par(mfrow = c(1,1))
summary(data1.model2)
plot(raw_data_1$Flow.m3[c(2:14,1)] ~ raw_data_1$Arduino.Flow.m[c(2:14,6)], xlab = "Flow Rate in m3/s", ylab = "Flow Rate in m3/s", main = "Measured flow vs Arduino Calulation")
abline(data1.model2, col = "blue")

par(mfrow = c(2,2))
plot(data1.model2)



par(mfrow = c(1,1))
data1.model3 <- lm(raw_data_1$Weir.Flow.m3[c(2:14,8)] ~ raw_data_1$Arduino.Flow.m[c(2:14,6)])
plot(raw_data_1$Weir.Flow.m3[c(2:14,8)] ~ raw_data_1$Arduino.Flow.m[c(2:14,1)], xlab = "Flow Rate in m3/s", ylab = "Flow Rate in m3/s", main = "Calculated flow vs Arduino Calulation")
abline(data1.model3, col = "dark green")


summary(data1.model3)
par(mfrow = c(2,2))
plot(data1.model3)

#Fitting to Dataset 3
data2.model1 <- lm(Flow.m3 ~ Arduino.Flow.m3, data=raw_data_3)
summary(data2.model1)
par(mfrow = c(1,1))
plot(raw_data_3$Flow.m3 ~ raw_data_3$Arduino.Flow.m, data=raw_data_3, xlab = "Flow Rate in m3/s", ylab = "Flow Rate in m3/s", main = "Measured Flow vs Arduino Calulation")
abline(data2.model1, col = "red")

par(mfrow = c(2,2))
plot(data2.model1)


par(mfrow = c(1,1))
data2.model2 <- lm(Weir.Flow.m3 ~ Arduino.Flow.m3, data = raw_data_3)
plot(raw_data_3$Weir.Flow.m3 ~ raw_data_3$Arduino.Flow.m3, xlab = "Flow Rate in m3/s", ylab = "Flow Rate in m3/s", main = "Calculated flow vs Arduino Calulation")
abline(data2.model2, col = "blue")


data_3_subset <- subset(raw_data_3, raw_data_3$Measured.Water.Height.mm<256) 


summary(data2.model3)
par(mfrow = c(2,2))
plot(data2.model2)

par(mfrow = c(1,1))
data2.model3 <- lm(Weir.Flow.m3 ~ Arduino.Flow.m3, data = data_3_subset)
plot(data_3_subset$Weir.Flow.m3 ~ data_3_subset$Arduino.Flow.m3, xlab = "Flow Rate in m3/s", ylab = "Flow Rate in m3/s", main = "Calculated flow vs Arduino Calulation")
abline(data2.model3, col = "dark green")

data_3_subset <- subset(raw_data_3, raw_data_3$Measured.Water.Height.mm<256) 

summary(data2.model3)
par(mfrow = c(2,2))
plot(data2.model3)

summary(data2.model1)
summary(data2.model2)
summary(data2.model3)

anova(data2.model1)

#Pump vs Weir Calculations
par(mfrow = c(1,1))
hist(raw_data_2$Pump.Deviation..m3s.1, xlab = "Flow in m3/s", main = "Deviation of Flowmeter from Calculated Values")

#Correlations
cor.test(raw_data_1$Flow.m3, raw_data_1$Arduino.Flow.m3)
cor.test(raw_data_1$Weir.Flow.m3, raw_data_1$Arduino.Flow.m3)

cor.test(raw_data_1$Flow.m3[c(2:14,5)], raw_data_1$Arduino.Flow.m3[c(2:14,6)])
cor.test(raw_data_1$Weir.Flow.m3[c(2:14,6)], raw_data_1$Arduino.Flow.m3[c(2:14,6)])

cor.test(raw_data_3$Flow.m3, raw_data_3$Arduino.Flow.m3)
cor.test(raw_data_3$Weir.Flow.m3, raw_data_3$Arduino.Flow.m3)

cor.test(data_3_subset$Flow.m3, data_3_subset$Arduino.Flow.m3)
cor.test(data_3_subset$Weir.Flow.m3, data_3_subset$Arduino.Flow.m3)

# Constant error variance
install.packages("car")
library("car")
require("car")
ncvTest(data1.model1)
ncvTest(data1.model2)
ncvTest(data1.model3)
ncvTest(data2.model1)
ncvTest(data2.model2)
ncvTest(data2.model3)


#Independent Residuals
require(lmtest)
dwtest(data1.model1, alternative = "two.sided")
dwtest(data1.model2, alternative = "two.sided")
dwtest(data1.model3, alternative = "two.sided")
dwtest(data2.model1, alternative = "two.sided")
dwtest(data2.model2, alternative = "two.sided")
dwtest(data2.model3, alternative = "two.sided")

#Normality
shapiro.test(rstandard(data1.model1))
shapiro.test(rstandard(data1.model2))
shapiro.test(rstandard(data1.model3))
shapiro.test(rstandard(data2.model1))
shapiro.test(rstandard(data2.model2))
shapiro.test(rstandard(data2.model3))
