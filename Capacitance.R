rm(list=ls())
setwd("/home/matt/Documents/EEE4022S Project/Stats Analysis")
dir()
#Section A
require(MASS)
require(plyr)
library(plyr)

#Resistivity Plots

resistivity<-read.csv("Comb_data1.csv")
capacitance<-read.csv("comb_data_4.csv")
plot(resistivity$Value, xlab = "Time /15s", ylab = "Resistance /k Ohm", main = "Logged resistivity of 300ml of sampled water", col = "dark green")


#Two sided F-Test power analysis
require(dae)
no.reps(df.num = 1, delta = 1, sigma = ((sd(capacitance$Non.Corroded)+sd(capacitance$Corroded))/2), power = 0.8)
(sd(capacitance$Non.Corroded)+sd(capacitance$Corroded))/2
