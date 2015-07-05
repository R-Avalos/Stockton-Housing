#### Stockton Housing Price Models ###
#####################################

#clear
rm(list = ls())

#libraries
library(foreign)
library(ggplot2)

## Load data ## 
stockton <- read.dta("stockton2.dta")
summary(stockton)
str(stockton)
head(stockton)
plot(stockton$sqft, stockton$price)
plot(stockton$age, stockton$price)
#notes about the data

stockton$agesq <- stockton$age^2
stockton$hundredsqft <- stockton$sqft/100
head(data)

## Models ##

#Ln(price) = B0(sqft/100) + B1(age) + B2(stories) + B3 (vacant) + e
modelOLS.Stockton <- lm(log(price) ~ hundredsqft + stories + vacant, stockton)
summary(modelOLS.Stockton)

#Ln(price) = F(sqft/100, age agesq stories vacant), estimate using OLS


model1 <- lm(log(price) ~ hundredsqft + age + agesq + stories + vacant, data)
summary(model1)
plot(model1)
#2.Plot the squared residuals separately against all RHS variables. 
sqresids <- model1$residuals^2
lnehat <- log(sqresids)
model2 <- lm(lnehat ~ hundredsqft + age + agesq + stories + vacant, data)
summary(model2)
plot(model2)
