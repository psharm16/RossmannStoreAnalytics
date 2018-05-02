#required packages
library(tidyr)
library(purrr)
library(dplyr)
library(broom)

setwd('C:/Users/Admin/SkyDrive/College/UNCC Spring 2018/ITCS 6162/project')
CalcErrors <- function(actuals.df, preds) {
  #adds column to dataframe containing the square percentage error given 
  #by ((actual - predicted)/actual)^2
  actuals.preds <- data.frame(cbind(actuals=actuals.df$Sales, predicteds=preds))
  actuals.preds <- actuals.preds[actuals.preds$actuals > 0,]
  actuals.preds$SqPctErr <- with(actuals.preds, ((actuals - predicteds)/actuals)^2)
  actuals.preds$SqErr <- with(actuals.preds, (actuals - predicteds)^2)
  print(sqrt(sum(actuals.preds$SqPctErr)/nrow(actuals.preds)))
  print(sqrt(sum(actuals.preds$SqErr)/nrow(actuals.preds)))
  return(actuals.preds)
}
trainRaw <- read.csv('train_cleaned.csv', stringsAsFactors = TRUE)
head(trainRaw)

#transform Date column to date type and DayOfWeek column to factor since it is not ordinal
trainRaw$Date <- as.Date(trainRaw$Date, format = "%Y-%m-%d")
trainRaw$DayOfWeek <- as.factor(trainRaw$DayOfWeek)
trainRaw$DateDay <- as.factor(trainRaw$DateDay)
trainRaw$DateMonth <- as.factor(trainRaw$DateMonth)
trainRaw$DateWeek <- as.factor(trainRaw$DateWeek)
trainRaw$DateYear <- as.factor(trainRaw$DateYear)
#subset into test and train portions. For validation it is essential to have the actuals
#in the dataset to calculate error. The test set provided by Kaggle does not have actuals.
test <- trainRaw[trainRaw$Date >= "2015-07-01",]
train <- trainRaw[trainRaw$Date < "2015-07-01",]

#fitting linear model for each store on training set
#using dplyr to vectorize rather than iterate or create multiple splits of the training set
dfStore = train %>% group_by(Store) %>%
  do(fit = lm(Sales ~ DayOfWeek + Open + Promo + SchoolHoliday, data = .))

#adding predictions to test data
#using dplyr and purrr to again vectorize in the same manner as for training
#each store's test data will be predicted using that store's respective model.
dfPred = test %>% group_by(Store) %>%
  nest() %>%
  inner_join(dfStore, .) %>%
  mutate(pred = list(augment(fit, newdata = data))) %>%
  unnest(pred)

#in the kaggle competition all days when stores are closed and sales are zero are removed prior
#to computing error rates.
dfPredCleaned <- dfPred[dfPred$Sales > 0,]
#kaggle scores the leaderboard using Root Mean Square Percent Error.
#square percent error and standard square error are calculated for each prediction
dfPredCleaned$SqPctErr <- with(dfPredCleaned, ((Sales - .fitted)/Sales)^2)
dfPredCleaned$SqErr <- with(dfPredCleaned, (Sales - .fitted)^2)
#final calculations for RMSPE and RMSE
sqrt(sum(dfPredCleaned$SqPctErr)/nrow(dfPredCleaned))
sqrt(sum(dfPredCleaned$SqErr)/nrow(dfPredCleaned))

dfStoreMore = train %>% group_by(Store) %>%
  do(fit = lm(Sales ~ DayOfWeek + Open + Promo + as.factor(DateYear) + 
            as.factor(DateMonth) + as.factor(DateDay) + as.factor(DateWeek) + 
            SchoolHoliday + StateHoliday, data = .))

dfPredMore = test %>% group_by(Store) %>%
  nest() %>%
  inner_join(dfStoreMore, .) %>%
  mutate(pred = list(augment(fit, newdata = data))) %>%
  unnest(pred)

dfPredMoreCleaned <- dfPredMore[dfPredMore$Sales > 0,]
dfPredMoreCleaned$SqPctErr <- with(dfPredMoreCleaned, ((Sales - .fitted)/Sales)^2)
dfPredMoreCleaned$SqErr <- with(dfPredMoreCleaned, (Sales - .fitted)^2)
#final calculations for RMSPE and RMSE
sqrt(sum(dfPredMoreCleaned$SqPctErr)/nrow(dfPredMoreCleaned))
sqrt(sum(dfPredMoreCleaned$SqErr)/nrow(dfPredMoreCleaned))

#comparing other algorithms
#while the dplyr method for vectorizing to model across all stores worked
#quite well for the standard linear model it did not work well with algorithms
#from other packages. As a result, we will consider a few stores at random 
#and compare the standard linear model to CART and random forest algorithms

library(rpart)
library(randomForest)
#for consistency we generate random numbers only once and we will add store 1 to these values
#sample(1:1115, 3)
#[1] 527 271 821
train527 <- train[train$Store == 527,]
train271 <- train[train$Store == 271,]
train821 <- train[train$Store == 821,]
train1 <- train[train$Store == 1,]
test527 <- test[test$Store == 527,]
test271 <- test[test$Store == 271,]
test821 <- test[test$Store == 821,]
test1 <- test[test$Store == 1,]

cart.527 <- rpart(Sales ~ DayOfWeek + Open + Promo + SchoolHoliday, data = train527)
pred527 <- predict(cart.527, newdata = test527)
eval527 <- CalcErrors(test527, pred527)

rf.527 <- randomForest(Sales ~ DayOfWeek + Open + Promo + SchoolHoliday, data = train527)
pred527rf <- predict(rf.527, newdata = test527)
eval527rf <- CalcErrors(test527, pred527rf)

lm.527 <- lm(Sales ~ DayOfWeek + Open + Promo + SchoolHoliday, data = train527)
pred527lm <- predict(lm.527, newdata = test527)
eval527lm <- CalcErrors(test527, pred527lm)

cart.271 <- rpart(Sales ~ DayOfWeek + Open + Promo + SchoolHoliday, data = train271)
pred271 <- predict(cart.271, newdata = test271)
eval271 <- CalcErrors(test271, pred271)

rf.271 <- randomForest(Sales ~ DayOfWeek + Open + Promo + SchoolHoliday, data = train271)
pred271rf <- predict(rf.271, newdata = test271)
eval271rf <- CalcErrors(test271, pred271rf)

lm.271 <- lm(Sales ~ DayOfWeek + Open + Promo + SchoolHoliday, data = train271)
pred271lm <- predict(lm.271, newdata = test271)
eval271lm <- CalcErrors(test271, pred271lm)

cart.821 <- rpart(Sales ~ DayOfWeek + Open + Promo + SchoolHoliday, data = train821)
pred821 <- predict(cart.821, newdata = test821)
eval821 <- CalcErrors(test821, pred821)

rf.821 <- randomForest(Sales ~ DayOfWeek + Open + Promo + SchoolHoliday, data = train821)
pred821rf <- predict(rf.821, newdata = test821)
eval821rf <- CalcErrors(test821, pred821rf)

lm.821 <- lm(Sales ~ DayOfWeek + Open + Promo + SchoolHoliday, data = train821)
pred821lm <- predict(lm.821, newdata = test821)
eval821lm <- CalcErrors(test821, pred821lm)

cart.1 <- rpart(Sales ~ DayOfWeek + Open + Promo + SchoolHoliday, data = train1)
pred1 <- predict(cart.1, newdata = test1)
eval1 <- CalcErrors(test1, pred1)

rf.1 <- randomForest(Sales ~ DayOfWeek + Open + Promo + SchoolHoliday, data = train1)
pred1rf <- predict(rf.1, newdata = test1)
eval1rf <- CalcErrors(test1, pred1rf)

lm.1 <- lm(Sales ~ DayOfWeek + Open + Promo + SchoolHoliday, data = train1)
pred1lm <- predict(lm.1, newdata = test1)
eval1lm <- CalcErrors(test1, pred1lm)

plot271 <- train271[train271$Date >= "2015-05-01",]
plot271 <- plot271[order(plot271$Date),]
x <- plot271$Date
y <- plot271$Sales
xl <- sort(test271$Date)
yl <- test271$Sales[order(test271$Date)]
plot(x, y, main = "Store 271", xlab = "Date", ylab = "Sales", type = 'l', col = 'black', xlim=as.Date(c("2015-05-01", "2015-07-31")))
lines(xl,yl,col = 'blue')
lines(xl,pred271rf,col = 'red')
lines(xl,pred271lm,col = 'green')
lines(xl,pred271,col = 'orange')
legend("top", inset=c(0.-0.15), legend=c("Training","Actuals","Predicted LM","Predicted RF","Predicted rpart"),
       col=c("black", "blue","red","green","orange"), horiz = TRUE, lty = 1, xpd = TRUE, cex=0.525)

plot821 <- train821[train821$Date >= "2015-05-01",]
plot821 <- plot821[order(plot821$Date),]
x <- plot821$Date
y <- plot821$Sales
xl <- sort(test821$Date)
yl <- test821$Sales[order(test821$Date)]
plot(x, y, main = "Store 821", xlab = "Date", ylab = "Sales", type = 'l', col = 'black', xlim=as.Date(c("2015-05-01", "2015-07-31")))
lines(xl,yl,col = 'blue')
lines(xl,pred821rf,col = 'red')
lines(xl,pred821lm,col = 'green')
lines(xl,pred821,col = 'orange')
legend("top", inset=c(0.-0.15), legend=c("Training","Actuals","Predicted LM","Predicted RF","Predicted rpart"),
       col=c("black", "blue","red","green","orange"), horiz = TRUE, lty = 1, xpd = TRUE, cex=0.525)

plot527 <- train527[train527$Date >= "2015-05-01",]
plot527 <- plot527[order(plot527$Date),]
x <- plot527$Date
y <- plot527$Sales
xl <- sort(test527$Date)
yl <- test527$Sales[order(test527$Date)]
plot(x, y, main = "Store 527", xlab = "Date", ylab = "Sales", type = 'l', col = 'black', xlim=as.Date(c("2015-05-01", "2015-07-31")))
lines(xl,yl,col = 'blue')
lines(xl,pred527rf,col = 'red')
lines(xl,pred527lm,col = 'green')
lines(xl,pred527,col = 'orange')
legend("top", inset=c(0.-0.15), legend=c("Training","Actuals","Predicted LM","Predicted RF","Predicted rpart"),
       col=c("black", "blue","red","green","orange"), horiz = TRUE, lty = 1, xpd = TRUE, cex=0.525)

plot1 <- train1[train1$Date >= "2015-05-01",]
plot1 <- plot1[order(plot1$Date),]
x <- plot1$Date
y <- plot1$Sales
xl <- sort(test1$Date)
yl <- test1$Sales[order(test1$Date)]
plot(x, y, main = "Store 1", xlab = "Date", ylab = "Sales", type = 'l', col = 'black', xlim=as.Date(c("2015-05-01", "2015-07-31")))
lines(xl,yl,col = 'blue')
lines(xl,pred1rf,col = 'red')
lines(xl,pred1lm,col = 'green')
lines(xl,pred1,col = 'orange')
legend("top", inset=c(0.-0.15), legend=c("Training","Actuals","Predicted LM","Predicted RF","Predicted rpart"),
       col=c("black", "blue","red","green","orange"), horiz = TRUE, lty = 1, xpd = TRUE, cex=0.525)
