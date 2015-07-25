########################################
#### Stockton Housing Price Models  ###
######################################

#clear
rm(list = ls())

#libraries
library(foreign)
library(ggplot2)
library(lmtest)
library(car)

## Load data ## 
##############
stockton <- read.dta("stockton2.dta") #change name to include year
summary(stockton)
str(stockton)
head(stockton)
plot(stockton$sqft, stockton$price)
plot(stockton$age, stockton$price)
#notes about the data ..... #

#load more current data
#constrain currrent data (max/min price or sqft, variables)




stockton$agesq <- stockton$age^2 #age square as nonlinear
stockton$hundredsqft <- stockton$sqft/100 #change to 100 square feet
head(stockton)

#################
###  Models   ##
###############

#Ln(price) = B0(sqft/100) + B1(age) + B2(stories) + B3 (vacant) + e
modelOLS.Stockton <- lm(log(price) ~ hundredsqft + age + stories + vacant, stockton)
summary(modelOLS.Stockton)
plot(modelOLS.Stockton) #quick residual plots
residualPlots(modelOLS.Stockton) # hundresqft p-value of 0.057

avPlots(modelOLS.Stockton, id.n=3) #added variable plots
qqPlot(modelOLS.Stockton, id.n=3) #outlier plots
influencePlot(modelOLS.Stockton, id.n=3) #influence plot

bptest(modelOLS.Stockton) #Breuch-Pagan test for constant variance
whites.htest()#white test for non-linear constant variance

#Ln(price) = F(sqft/100, age agesq stories vacant), estimate using OLS
modelOLS.Stockton2 <- lm(log(price) ~ hundredsqft + age + agesq + stories + vacant, stockton)
summary(modelOLS.Stockton2)
plot(modelOLS.Stockton2)

#2.Plot the squared residuals separately against all RHS variables. 
sqresids <- modelOLS.Stockton$residuals^2
lnehat <- log(sqresids)
model2 <- lm(lnehat ~ hundredsqft + age + agesq + stories + vacant, stockton)
summary(model2)
plot(model2)
