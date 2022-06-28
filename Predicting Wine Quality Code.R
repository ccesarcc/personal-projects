######### Final Project #########
rm(list=ls())# remove all variables in memory
library(readr)
library(ISLR)
library(boot)
library(olsrr)
library(caTools)
library(caTools)
library(MASS)


all_wines <- read.csv("wine-quality-white-and-red.csv")
#head(all_wines)
names(all_wines)

################ Model Selection ################
#forward
model <- lm(quality ~., data=all_wines)
ols_step_forward_p(model)
plot(ols_step_forward_p(model))
#backward
ols_step_backward_p(model)  
plot(ols_step_backward_p(model) )
#best subset
ols_step_best_subset(model)
plot(ols_step_best_subset(model))

############################ Linear Regression ############################
################ k-fold Cross Validation ################
set.seed(17)
# fit a model using all variables
glm.fit = glm(quality~.,data=all_wines)
# customize a cost function to find the abs mean deviation
cost <- function(r, pi) mean(abs(r-pi))
cv.err = cv.glm(all_wines, glm.fit, cost, K=10)
# error rate of all varibles
cv.err$delta
#### 0.569924 0.569844

set.seed(17)
# fit a model using best variables
glm.fit2 = glm(quality~ type + fixed.acidity + volatile.acidity + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, data=all_wines)
cv.err = cv.glm(all_wines, glm.fit2, cost, K=10)
# error rate of best varibles
cv.err$delta
#### 0.5699147 0.5698385

################ LOOCV ################
set.seed(17)
# error rate of all varibles using customized cost function
cv.err = cv.glm(all_wines, glm.fit, cost)
cv.err$delta
#### 0.5697885 0.5697883

set.seed(17)
# error rate of best varibles using customized cost function
cv.err = cv.glm(all_wines, glm.fit2, cost)
cv.err$delta
#### 0.5697989 0.5697989


############################ Polynomial Regression ############################
################ k-fold Cross Validation ################



################ LOOCV ################


############################ Ordinal Logistic Regression ############################

##data preparation
all_wines$quality[all_wines$quality<4] <- 1
all_wines$quality[(all_wines$quality>3) & (all_wines$quality<7) ] <- 2
all_wines$quality[all_wines$quality>=7] <- 3
all_wines$quality <- as.factor(all_wines$quality)
summary(all_wines$quality)

set.seed(3000)
spl = sample.split(all_wines$quality, SplitRatio = 0.7)
wine_train = subset(all_wines, spl==TRUE)
wine_test = subset(all_wines, spl==FALSE)

#OLR  with all variables
wine_ord2 <- polr(quality ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, 
                 data=wine_train, Hess=TRUE)
summary(wine_ord2)
predict_qual = predict(wine_ord2, all_wines)
table(all_wines$quality, predict_qual)
mean(as.character(all_wines$quality) != as.character(predict_qual)) #Misclassification error
#0.1897799

# OLR with all but citric.acid since it is not as significant
wine_ord3 <- polr(quality ~ fixed.acidity+volatile.acidity+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, 
                  data=wine_train, Hess=TRUE)
summary(wine_ord3)
predict_qual1 = predict(wine_ord3, all_wines)
table(all_wines$quality, predict_qual1)

mean(as.character(all_wines$quality) != as.character(predict_qual1))#Misclassification error
#0.1891642


################ k-fold Cross Validation ################



################ LOOCV ################




