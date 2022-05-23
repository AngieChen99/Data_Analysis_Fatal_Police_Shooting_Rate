
library("readxl")
library(car)
library(caret)
library(FNN)
library(class)
library(tidyverse)
library(forecast)
library(leaps)
library(gains)

library("Hmisc")
library(ggplot2)

#install data
mydata <- read_excel("/Users/suhail/Desktop/Spring'22/Fata_Data_For_report2a.xlsx")


#Correlation between PI_per_state and Crime_Case  
cor(mydata$PI_per_state, mydata$Fatal_police_shootings)
#Correlation between Crime_Case & Fatal police shootings
cor(mydata$Crime_Case, mydata$Fatal_police_shootings)
#Correlation between  PI per state & police shooting
cor(mydata$PI_per_state, mydata$Crime_Case)

vis11 <- ggplot(mydata, aes(x =PI_per_state, y =Crime_Case)) + geom_point()
xlab = "Average income per person per state",
ylab = "Crime Case per state")
print(vis11)
print(mydata$Crime_Case)


vis1 <- ggplot(mydata, 
               aes(x = Shot, 
                   fill = Armed_D)) + 
  geom_bar(position = "dodge")
print(vis1)

vis2 <- ggplot(mydata, 
               aes(x = Shot, 
                   fill = dummy_Mental_illness)) + 
  geom_bar(position = "dodge")
print(vis2)


#1. index
# 2. UID
# 3. Name
# 4. Age
# 5. Gender
# 6. Race
# 7. Date
# 8. City
# 9. State
# 10. Manner_of_death
# 11.Armed
# 12. Mental_illness
# 13. Flee
# 14. Year
# 15. Armed_D
#16. Dummy_Mental_illness
# 17. PI_per_state
# 18. Fatal_police_shootings
# 19. Crim_case
# 20. Population
# 21. Homeless
# 22. polynomial_CrimeCase
# 23. Shot
# partition data
corr(as.matrix(mydata))

set.seed(2)
dim(mydata)
train.index <- sample(c(1:dim(mydata)[1]), dim(mydata)[1]*0.6)  
train.df <- mydata[train.index, ]
valid.df <- mydata[-train.index, ]


#Create and set aside the remaining 40% of the data, to be used after omitting unhelpful data points and unnecessary variables.

#logit regression with a poloynomial term
logit.reg2 <- glm(as.factor(Shot) ~ Gender + Mental_illness  + Armed_D + polynomial_CrimeCase, data = mydata, family = binomial(link = "logit"))
options(scipen=999)
summary(logit.reg2)
exp(coef(logit.reg2))



#logit regression
logit.reg1 <- glm(Shot ~ Gender + Race  + Mental_illness  + Armed_D, data = mydata, family = binomial(link = "logit"))
options(scipen=999)
summary(logit.reg1)
##Indicators indicating the racial profile, states were removed from the data due to them not being statistically significant
#Lets remove the insignificant variablesLogit regression
logit.reg <- glm(as.factor(Shot) ~ Gender +Mental_illness  + Armed_D, data = mydata, family = binomial(link = "logit"))
options(scipen=999)
summary(logit.reg)
exp(coef(logit.reg))
##Indicators indicating the racial profile, states were removed from the data due to them not being statistically significant
# and do a collinearity check with multiple linear regression (not for actual modeling)


gain <- gains(valid.df$Shot, logit.reg.pred, groups=3)
print(gain)

### Table 10.3

# we can use predict() with type = "response" to compute predicted probabilities of specific cases
logit.reg.pred <- predict(logit.reg, valid.df[, -8], type = "response")

# show a few actual and predicted records
#data.frame(actual = valid.t$shot, predicted = logit.reg.pred)

# let's look at the lift chart to see how well we are classifying
#gain <- gains(valid.df$Shot, logit.reg.pred, groups=10)
# plot lift chart
#plot(c(0,gain$cume.pct.of.total*sum(valid.df$Shot))~c(0,gain$cume.obs),
#xlab="# cases", ylab="Cumulative", main="", type="l")
#lines(c(0,sum(valid.df$Shot))~c(0, dim(valid.df)[1]), lty=2)

# how good is it?
prediction <-as.factor(ifelse(logit.reg$fitted > 0.5, 1, 0))

traindata <- as.factor(train.df$Shot)
prediction <- as.numeric(prediction)
print(length(traindata))
str(traindata)
str(prediction)
print(prediction)
print(traindata)
cm <- confusionMatrix(prediction, traindata)
table(traindata, prediction)
# let's take a look at the target variable again
hist(mydata$Shot)

# use knn() to compute knn. 
#### Figure 7.1


#### Table 7.2

# initialize normalized training, validation data, complete data frames to originals
train.norm.df <- train.df
valid.norm.df <- valid.df
mydata.norm.df <- mydata.df


# use preProcess() from the caret package to normalize Income and Lot_Size.
norm.values <- preProcess(train.df[, 1:2], method=c("center", "scale"))
train.norm.df[, 1:2] <- predict(norm.values, train.df[, 1:2])
valid.norm.df[, 1:2] <- predict(norm.values, valid.df[, 1:2])
mydata.norm.df[, 1:2] <- predict(norm.values, mydata.df[, 1:2])  # whole thing
new.norm.df <- predict(norm.values, new.df)



# use knn() to compute knn. 
# knn() is available in library FNN (provides a list of the nearest neighbors)
# and library class (allows a numerical output variable).

nn <- knn(train = train.norm.df[, 1:2], cl = train.norm.df[, 3], test = new.norm.df, 
          k = 3)   # <<<<<<< set k for number of neighbors considered

row.names(train.df)[attr(nn, "nn.index")]  # owner or non-owner






# Initialize a data frame with two columns: k, and accuracy.
accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))

# compute knn for different numbers of neighbords (k) on validation.
for(i in 1:14) {          # <<<< adjust the bounds to look at particular confusion matrix
  knn.pred <- knn(train = train.norm.df[, 1:2], cl = train.norm.df[, 3], 
                  test = valid.norm.df[, 1:2], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, factor(valid.norm.df[, 3]))$overall[1] 
  
  
}

# which k is the best?
accuracy.df



#
for(i in 3:3) {  # <<<< adjust the bounds to look only at confusion matrix with specific k
  knn.pred <- knn(train = train.norm.df[, 1:2], cl = train.norm.df[, 3], 
                  test = valid.norm.df[, 1:2], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, factor(valid.norm.df[, 3]))$overall[1] 
  
  
}

confusionMatrix(knn.pred, factor(valid.norm.df[, 3]))

mydata2 <- read.csv("/Users/suhail/Downloads/Final_FataData_2b.csv")
#logit regression with a poloynomial term
logit.reg22 <- glm(Shot ~ Gender + Race  + Mental_illness  + Armed_D +Cluster_Var, data = mydata2, family = binomial(link = "logit"))
options(scipen=999)
summary(logit.reg22)


library("readxl")
library(car)
library(caret)
library(FNN)
library(class)
library(tidyverse)
library(forecast)
library(leaps)
#install data

mydata0 <- read_excel("/Users/suhail/Downloads/Fata_Data_For_report2a (1).xlsx")

mydata = na.omit(mydata0)



#1. index
# 2. UID
# 3. Name
# 4. Age
# 5. Gender
# 6. Race
# 7. Date
# 8. City
# 9. State
# 10. Manner_of_death
# 11.Armed
# 12. Mental_illness
# 13. Flee
# 14. Year
# 15. Armed_D
#16. Dummy_Mental_illness
# 17. PI_per_state
# 18. Fatal_police_shootings
# 19. Crim_case
# 20. Population
# 21. Homeless
# 22. polynomial_CrimeCase
# 23. Shot
# partition data
set.seed(200)
dim(mydata)
train.index <- sample(c(1:dim(mydata)[1]), dim(mydata)[1]*0.6)  
train.df <- mydata[train.index, ]
valid.df <- mydata[-train.index, ]


#Create and set aside the remaining 40% of the data, to be used after omitting unhelpful data points and unnecessary variables.


#logit regression
###############################should use the train.df instead of the whole dataset

logit.reg1 <- glm(Shot ~ Gender + Race  + Mental_illness  + Armed_D, data = train.df, family = binomial(link = "logit"))
options(scipen=999)
summary(logit.reg1)
##Indicators indicating the racial profile, states were removed from the data due to them not being statistically significant
#Lets remove the insignificant variablesLogit regression
logit.reg <- glm(as.factor(Shot) ~ Gender +Mental_illness  + Armed_D, data = train.df, family = binomial(link = "logit"))
options(scipen=999)
summary(logit.reg)
exp(coef(logit.reg))
##Indicators indicating the racial profile, states were removed from the data due to them not being statistically significant
# and do a collinearity check with multiple linear regression (not for actual modeling)

### Table 10.3
###############################should delete the target

# we can use predict() with type = "response" to compute predicted probabilities of specific cases
logit.reg.pred <- predict(logit.reg, valid.df[, -24], type = "response")


# how good is it?
prediction <-as.factor(ifelse(logit.reg$fitted > 0.5, 1, 0))

traindata <- as.factor(train.df$Shot)

confusionMatrix(prediction, traindata)
table(traindata, prediction)
# let's take a look at the target variable again
hist(mydata$Shot)

# use knn() to compute knn. 
#### Figure 7.1


#### Table 7.2

# initialize normalized training, validation data, complete data frames to originals
train.norm.df <- train.df
valid.norm.df <- valid.df
mydata.norm.df <- mydata

###############################should normalize the predictors

# use preProcess() from the caret package to normalize Income and Lot_Size.
norm.values <- preProcess(train.df[, 1:23], method=c("center", "scale"))
train.norm.df[, 1:23] <- predict(norm.values, train.df[, 1:23])
valid.norm.df[, 1:23] <- predict(norm.values, valid.df[, 1:23])
mydata.norm.df[, 1:23] <- predict(norm.values, mydata[, 1:23])  # whole thing


# Initialize a data frame with two columns: k, and accuracy.
accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))

train.norm.df <- train.norm.df[-23]
dummy1 <- train.df[23]
train.norm.df <- cbind(train.norm.df, dummy1)

# compute knn for different numbers of neighbords (k) on validation.
for(i in 1:1) {          # <<<< adjust the bounds to look at particular confusion matrix
  knn.pred <- knn(train = train.norm.df[,16:21], cl = train.norm.df[, 24], 
                  test = valid.norm.df[,16:21], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, factor(valid.norm.df[, 24]))$overall[1] 
}

# which k is the best?
accuracy.df
dim(train.norm.df[, 1:23])
dim(train.norm.df[, 24])
sum(is.na(train.norm.df[, 1:23]))
sum(is.na(train.norm.df[, 24]))



confusionMatrix(knn.pred, factor(valid.norm.df[, 3]))
