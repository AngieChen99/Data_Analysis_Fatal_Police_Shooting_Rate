#install data
mydata <- read_excel("~/Desktop/Fata_Data_For_report2a (1).xlsx")

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
set.seed(2)
dim(mydata)
train.index <- sample(c(1:dim(mydata)[1]), dim(mydata)[1]*0.6)  
train.df <- mydata[train.index, ]
valid.df <- mydata[-train.index, ]


#Create and set aside the remaining 40% of the data, to be used after omitting unhelpful data points and unnecessary variables.


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
VIFcheck <- lm( Shot ~ . -Gender -Mental_illness -Armed_D, data = train.df)
vif(VIFcheck)

### Table 10.3

# we can use predict() with type = "response" to compute predicted probabilities of specific cases
logit.reg.pred <- predict(logit.reg, valid.df[, -8], type = "response")

# show a few actual and predicted records
#data.frame(actual = valid.t$shot, predicted = logit.reg.pred)

# let's look at the lift chart to see how well we are classifying
gain <- gains(valid.df$Shot, logit.reg.pred, groups=10)
# plot lift chart
plot(c(0,gain$cume.pct.of.total*sum(valid.df$Shot))~c(0,gain$cume.obs),
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.df$Shot))~c(0, dim(valid.df)[1]), lty=2)

# how good is it?
prediction <-as.factor(ifelse(logit.reg$fitted > 0.5, 1, 0))

traindata <- as.factor(train.df[24])
print(prediction)
print(traindata)
cm <- confusionMatrix(prediction, traindata)

# let's take a look at the target variable again
hist(mydata$Shot)
