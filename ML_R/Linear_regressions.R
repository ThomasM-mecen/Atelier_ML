rm(list = ls())
# Import packages
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(ISLR)
library(MASS)

# Load dataset
data = Boston
pairs(~ medv + ptratio + black + lstat + dis + rm + crim, data = data, main = "Boston Data")

# Simple linear regression
fit1 = lm(medv ~ lstat, data=data)
summary(fit1)
plot(medv ~ lstat, data)
abline(fit1, col="red")
plot(medv ~ lstat, data)
points(data$lstat, fitted(fit1), col="red",pch=20)

# Multiple linear regression
fit2 = lm(medv ~ ., data=data)
summary(fit2)
fit3 = update(fit2, ~ .-age-indus)
summary(fit3)
plot(medv ~ lstat, data)
points(data$lstat, fitted(fit3), col="red",pch=20)
fit4 = update(fit3, ~ . + I(lstat^2))
summary(fit4)
plot(medv ~ lstat, data)
points(data$lstat, fitted(fit4), col="red",pch=20)
data$predict_price = predict(fit4)
