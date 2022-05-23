library(tidyverse)
library(forecast)
library(leaps)
library(car)
library(readxl)


car.df <- read_excel("R/Fata_Data_For_report2a.xlsx")

car.t <- as_tibble(car.df)    

# use first 1000 rows of data
car.t <- car.t[1:1000, ]


# Column Number) Variable
#
# 
# 4) Age
# 7) Date        
# 14) Year    
# 15) dummy_Mental_illness
# 16) dummy_flee
# 17) PI_per_state
# 18) Fatal_police_shootings
# 19) Crime_Case
# 20) Population
# 21) Homeless
# 22)polynomial_CrimeCase


# select variables for regression
selected.var <- c( 4, 7, 14, 15, 16, 17, 18, 19, 20, 21,22)   


# partition data
set.seed(1)  # set seed for reproducing the partition
train.index <- sample(c(1:1000), 600)  

#Create and set aside the remaining 40% of the data, to be used after omitting unhelpful data points and unnecessary variables.
train.t <- car.t[ train.index, selected.var ]
valid.t <- car.t[ -train.index, selected.var ]

#### Exclude outliers - It is an iterative process! #####
train.t <- train.t[-c(324,662,16,66,26,27,68),]
#
# and exclude them from the valid.t subset
valid.t <- car.t[-train.index, selected.var]

# big matrix plot; make sure the plot window is big enough; then remove unneeded variables; repeat
pairs(~ Fatal_police_shootings + Age + Date + Year + dummy_Mental_illness + dummy_flee +
        Population + PI_per_state + Crime_Case + Homeless + polynomial_CrimeCase, data=train.t, main="Scatterplot Matrix")

# use lm() to run a linear regression of Fata rate on all predictors in the
# training set. 
# use . after ~ to include all the remaining columns in train.df as predictors.
car.lm <- lm(Fatal_police_shootings~ . - Age -dummy_Mental_illness -dummy_flee -Date -Year , data = train.t)
#car.lm <- lm(Fatal_police_shootings~ . , data = train.t)

#  use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999)
summary(car.lm)

#check for Variance Inflation Factor (VIF); must be < 10; should be less than 5
vif(car.lm)

## additional diagnostics to check for outliers/leverage points
par(mfrow=c(2,2))
plot(car.lm)


#### Table 6.4


summary(valid.t$Fuel_Type)

# use predict() to make predictions on a new set. 
car.lm.pred <- predict(car.lm, valid.t)
options(scipen=999, digits = 0)
some.residuals <- valid.t$Fatal_police_shootings[1:20] - car.lm.pred[1:20]
data.frame("Predicted" = car.lm.pred[1:20], "Actual" = valid.t$Fatal_police_shootings[1:20],
           "Residual" = some.residuals)

options(scipen=999, digits = 3)
# use accuracy() to compute common accuracy measures.
accuracy(car.lm.pred, valid.t$Fatal_police_shootings)


#### Figure 6.1

car.lm.pred <- predict(car.lm, valid.t)
all.residuals <- valid.t$Fatal_police_shootings - car.lm.pred
length(all.residuals[which(all.residuals > -1406 & all.residuals < 1406)])/400
hist(all.residuals, breaks = 25, xlab = "Residuals", main = "")



#### Table 6.5

# use regsubsets() in package leaps to run an exhaustive search. 
# unlike with lm, categorical predictors must be turned into dummies manually.

search.train <- regsubsets(Fatal_police_shootings ~ . , data = train.t, nbest = 1, nvmax = dim(train.t)[2],
                           method = "exhaustive")
sum <- summary(search.train)

search.valid <- regsubsets(Fatal_police_shootings ~ . , data = valid.t, nbest = 1, nvmax = dim(valid.t)[2],
                           method = "exhaustive")
sum <- summary(search.valid)

# show models
sum$which

# show metrics
sum$rsq
sum$adjr2
#sum$Cp



#### Table 6.6
# use step() to run stepwise regression.
car.lm.step <- step(car.lm, direction = "backward")
summary(car.lm.step)  # Which variables were dropped?
car.lm.step.pred <- predict(car.lm.step, valid.t)
accuracy(car.lm.step.pred, valid.t$Fatal_police_shootings)

#### Table 6.7
# create model with no predictors
car.lm.null <- lm(Fatal_police_shootings~1, data = valid.t)
# use step() to run forward regression.
car.lm.step <- step(car.lm.null, scope=list(lower=car.lm.null, upper=car.lm), direction = "forward")
summary(car.lm.step)  # Which variables were added?
car.lm.step.pred <- predict(car.lm.step, valid.t)
accuracy(car.lm.step.pred, valid.t$Fatal_police_shootings)
vif(car.lm.step)

#### Table 6.8
# use step() to run stepwise regression.
car.lm.step <- step(car.lm, direction = "both")
summary(car.lm.step)  # Which variables were dropped/added?
car.lm.step.pred <- predict(car.lm.step, valid.t)
accuracy(car.lm.step.pred, valid.t$Fatal_police_shootings)

