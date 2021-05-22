#title: Week 10 Logistic regression - Binary Classifier
#author: Vasanthakumar Kalaikkovan
#Date: 05/22/2021

getwd()

setwd("E://Repos/StatisticsR/DSC520-Statistics/week10")

binary_df <- read.csv("binary-classifier-data.csv")

head(binary_df)

binary_model <- glm(label~x+y,data=binary_df,family=binomial())

summary(binary_model)

psuedo_R2 <- 1-(binary_model$deviance/binary_model$null.deviance)
psuedo_R2

#What is the accuracy of the logistic regression classifier?
#Very poor, with very low Pseudo R-squared of 0.01.
#Residual deviance of 2052.1 is only slightly smaller than null deviance of 2075.8, hence a low Pseudo R-squared
#p<0.01 for the intercept and y,x however is too high
#Multicollinearity not addressed, but dont think its an issue with low Pseudo R-squared and only two predictors.
#AIC of 2058.1 can be compared with other models.