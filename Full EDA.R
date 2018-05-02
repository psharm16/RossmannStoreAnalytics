
#getwd()
#setwd("C:/Users/prek7/OneDrive/kdd/proj")
#Read the store data
store_data <- read.csv(file="store.csv",stringsAsFactors = TRUE)
store_data
str(store_data)
#Read the train data
train <- read.csv(file="train.csv",stringsAsFactors = TRUE)
#Read the test data
test <- read.csv(file="test.csv",stringsAsFactors = TRUE)
#Install the packages mice, VIM for data imputation and ggplot for plotting
install.packages("mice")
install.packages("ggplot")
library(mice)
library(ggplot)
md.pattern(store_data)
install.packages("VIM")
library(VIM)
#aggr_plot <- aggr(store_data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=0.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
#Predictive Mean Matching is being used as imputation method. Other imputation methods can be used, type methods(mice) for a list of the available imputation methods.
tempData <- mice(store_data,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)
methods(mice)
#tempData$imp$CompetitionOpenSinceMonth
#We have 5 imputed datasets from the mice command and we can take any one from the command below by changing the number we can load the specific imputed dataset
completedData <- complete(tempData,1)
tail(completedData$Promo2SinceWeek)
str(completedData)
completedData
cal_med_test <- subset(completedData, CompetitionOpenSinceMonth > 12)
count_med_test<-nrow(cal_med_test)
count_med_test
xyplot(tempData,Store ~ CompetitionOpenSinceYear+Promo2SinceWeek+Promo2SinceYear,pch=18,cex=1)
stripplot(tempData, pch = 20, cex = 1.2)
modelFit1 <- with(tempData,lm(Store ~ CompetitionOpenSinceYear+Promo2SinceWeek+Promo2SinceYear))
summary(pool(modelFit1))

tempData

#Selecting only the stores which are open from the test data since predicting sales for closed shops is trivial
test[is.na(test$Open), ]
test[is.na(test)] <- 1

#train[is.na(train$Open), ]
#test[is.na(test)] <- 1
#Merge the train dataset and store data to get relation between the two data as we do not have necessary data in store
train <- merge(train,store_data)
train
str(train)
#Merge test and store data
test <- merge(test,store_data)
str(test)
#Add new columns in the test data by separating the Year, Month, Day and Week from the date
d <- as.Date(train$Date, format="%m/%d/%Y")
train$DateYear <- as.numeric(format(d, format = "%Y"))
train$DateMonth <- as.numeric(format(d, format = "%m"))
train$DateDay <- as.numeric(format(d, format = "%d"))
train$DateWeek <- as.numeric(format(d, format = "%W"))
#train$DateYear <- as.numeric(strftime(train$Date, format="%y")) 
#train$DateMonth <- as.numeric(strftime(train$Date, format="%m"))
#train$DateDay <- as.numeric(strftime(train$Date, format="%d"))
#train$DateWeek <- as.numeric(strftime(train$Date, format="%W"))
attributes(train)
summary(train)
#Add new columns in the test data by separating the Year, Month, Day and Week from the date
dtest <- as.Date(test$Date, format="%m/%d/%Y")
test$DateYear <- as.numeric(format(dtest, format = "%Y"))
test$DateMonth <- as.numeric(format(dtest, format = "%m"))
test$DateDay <- as.numeric(format(dtest, format = "%d"))
test$DateWeek <- as.numeric(format(dtest, format = "%W"))
#test$DateYear <- as.numeric(strftime(test$Date, format="%y")) 
#test$DateMonth <- as.numeric(strftime(test$Date, format="%m"))
#test$DateDay <- as.numeric(strftime(test$Date, format="%d"))
#test$DateWeek <- as.numeric(strftime(test$Date, format="%W"))


#hist(train$Sales)
#qqnorm(train$Sales)
#shapiro.test(train$Sales)
attributes(test)
#Get the boxplot for Sales column in train dataset
boxplot(train$Sales)
#Get the standard deviation for Sales column in train dataset
sd(train$Sales)
#Get the InterQuartile Range for Sales column in train dataset
IQR(train$Sales)
quantile(train$Sales)
x=7856+(1.5*4129)
x
#=14049.5
#Get the aggregate of number of records for each store in the dataset
openDays <- aggregate(train$Store,list(train$Store),length)
head(openDays)
summary(openDays)
summary(train[train$Sales > x,])

#The sales should not be considered as ouliers but some stores have high sales

hist(train$Sales,xlab="Sales Amount")
boxplot(train$Sales)
summary(train)
summary(train[train$Sales > 18000,])
#train <- merge(train1,store_data)
openDays <- aggregate(train$Store,list(train$Store),length)
summary(openDays)
#categories variable
#check for normalization
x<-plot(train$StoreType) 
x

# note adjusting breaks can be useful to achieve optimal representation
# 1 e:  hist function can return information
y = hist(x)
y #shows what is in y

# 1 f: use this to plot points relative to y
plot(y$density~y$mids) #relative heights of histogram bars
# 1 g: smooth curve approximating the histogram using density function
lines(density(x))

#1.1 Measures of Location
#1.1 a:  add vertical lines showing mean and median
abline(v=mean(x),col="red")
abline(v=median(x),col="blue")

#check cov
#check outliers
#EDA on categorical
#check assortment,type,holiday-school,state for sales
#find error
z = x^2
hist(z,breaks=10)
abline(v=mean(z),col="red")
abline(v=median(z),col="blue")

#1.1 g mean is average but not typical
# most data lies below the mean
# confirm with following
length(z) #shows length of z
length(z[z<mean(z)]) #shows length less than mean
quantile(z)
abline(v=quantile(z),col="green")
boxplot(z)
#1.2 h z follows chisq dist
plot(ecdf(z))
abline(v=quantile(z),col="red")
lines(pchisq(seq(min(z),max(z),length.out=100)->a,1)~a,col="blue")

m<-plot(train$Assortment) 
cov(m)
quantile(m)
m<-plot(train$Assortment) 

# note adjusting breaks can be useful to achieve optimal representation
# 1 e:  hist function can return information
ym = hist(m)
ym #shows what is in y

# 1 f: use this to plot points relative to y
plot(ym$density~ym$mids) #relative heights of histogram bars
# 1 g: smooth curve approximating the histogram using density function
lines(density(m))

#1.1 Measures of Location
#1.1 a:  add vertical lines showing mean and median
abline(v=mean(m),col="red")
abline(v=median(m),col="blue")
zm = m^2
hist(zm,breaks=10)
abline(v=mean(zm),col="red")
abline(v=median(zm),col="blue")

#1.1 g mean is average but not typical
# most data lies below the mean
# confirm with following
length(zm) #shows length of z
length(zm[zm<mean(zm)]) #shows length less than mean
quantile(zm)
abline(v=quantile(zm),col="green")
boxplot(zm)

m<-plot(train$StateHoliday) 
cov(m)
quantile(m)

# note adjusting breaks can be useful to achieve optimal representation
# 1 e:  hist function can return information
ym = hist(m)
ym #shows what is in y
# 1 f: use this to plot points relative to y
plot(ym$density~ym$mids) #relative heights of histogram bars
# 1 g: smooth curve approximating the histogram using density function
lines(density(m))
#1.1 Measures of Location
#1.1 a:  add vertical lines showing mean and median
abline(v=mean(m),col="red")
abline(v=median(m),col="blue")
zm = m^2
hist(zm,breaks=10)
abline(v=mean(zm),col="red")
abline(v=median(zm),col="blue")
#1.1 g mean is average but not typical
# most data lies below the mean
# confirm with following
length(zm) #shows length of z
length(zm[zm<mean(zm)]) #shows length less than mean
quantile(zm)
abline(vm=quantile(zm),col="red")
boxplot(zm)
#1.2 h z follows chisq dist
plot(ecdf(zm))
abline(vm=quantile(zm),col="red")
lines(pchisq(seq(min(zm),max(zm),length.out=100)->a,1)~a,col="blue")

library(ggplot2)

#Visualize the average sales on different StateHolidays which is categorical variable
salesByState <- aggregate(train$Sales,by = list(train$StateHoliday),function(x){mean(as.numeric(x))})
salesByState <- salesByState[-1,]
names(salesByState) <- c("StateHoliday","Average.Sales")
ggplot(data=salesByState,aes(x=StateHoliday,y=Average.Sales,fill=StateHoliday)) + 
  geom_bar(stat="identity") + ggtitle("Average sales by state holiday")+
  scale_fill_hue(c=45, l=80)

#Visualize the average sales on different SchoolHolidays which is categorical variable
salesBySchool <- aggregate(train$Sales,by = list(train$SchoolHoliday),function(x){mean(as.numeric(x))})
names(salesBySchool) <- c("SchoolHoliday","Average.Sales")
ggplot(data=salesBySchool,aes(x=SchoolHoliday,y=Average.Sales,fill=SchoolHoliday)) + geom_bar(stat="identity") +
  ggtitle("Average sales by school holiday")


#Visualize the average sales for different types of sales which is categorical
salesByStoreType <- aggregate(train$Sales,by = list(train$StoreType),function(x){mean(as.numeric(x))})
names(salesByStoreType) <- c("Store.Type","Average.Sales")
ggplot(data=salesByStoreType,aes(x=Store.Type,y=Average.Sales,fill=Store.Type)) + geom_bar(stat="identity") +
  ggtitle("Average Sales by store type")


#Visualize the average sales for different Assortment Type which is categorical
salesByAssortment <- aggregate(train$Sales,by = list(train$Assortment),function(x){mean(as.numeric(x))})
names(salesByAssortment) <- c("Assortment","Average.Sales")
ggplot(data=salesByAssortment,aes(x=Assortment,y=Average.Sales,fill=Assortment)) + geom_bar(stat="identity") +
  ggtitle("Average Sales by assortment type")


#Plot the Sales according to the Year
#sales increase overs the year
tapply(train$Sales,train$DateYear,mean)
plot(tapply(train$Sales,train$DateYear,mean),xlab="Year",ylab="Mean of Sales")
tapply(train$Sales,train$DateMonth,mean)

#Plot the Sales according to the day of month and check the data
#Increased sales in holiday season nov dec
plot(tapply(train$Sales,train$DateMonth,mean),xlab="Month",ylab="Mean of Sales")

#Plot the Sales according to the day of month and check the data
#Sales are least on sat ans sun may be because they are closed, averagely distributed across week with high sales on monday as stores open after holiday.
tapply(train$Sales,train$DayOfWeek,mean)
plot(tapply(train$Sales,train$DayOfWeek,mean),xlab="Week Number",ylab="Mean of Sales")

#Plot the Sales according to the day of month and check the data
#Sales are high in beginning or end of month as most people get salary
plot(tapply(train$Sales,train$DateDay,mean),xlab="Day of Month",ylab="Mean of Sales")

#Plot the Sales according to the week and check the data
#The sales are very high during the week of Christmas
plot(tapply(train$Sales,train$DateWeek,mean),xlab="Week Number",ylab="Mean of Sales")

#Plot the average os Sales for the years 2013, 2014, 2015 with respect to week number
#The week of Christmas most probably has higher mean of Sales as there is no data for 5 months for 2015
plot(tapply(train[train$DateYear == 13,]$Sales,train[train$DateYear == 13,]$DateWeek,mean),xlab="Week Number",ylab="Mean of Sales")
points(tapply(train[train$DateYear == 14,]$Sales,train[train$DateYear == 14,]$DateWeek,mean),col="red")
points(tapply(train[train$DateYear == 15,]$Sales,train[train$DateYear == 15,]$DateWeek,mean),col="blue")
legend("top",c("2013","2014","2015"),col=c("black","red","blue"),pch=c(1,1,1))

#Promo has direct effect on sales as the p-value is very less
#Get the correlation between Promo and Sales
cor(train$Promo,train$Sales)
t.test(train[train$Promo,]$Sales,train[!train$Promo,]$Sales)

#StateHoliday also directly effects Sales as Sales on the days when it is StateHoliday is high
t.test(train[train$StateHoliday != 0,]$Sales,train[train$StateHoliday == 0,]$Sales)