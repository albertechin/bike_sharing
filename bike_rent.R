# Remove variables
rm(list=ls())

#loading the required libraries
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)

# reading the data file
data = read.csv("day.csv")
str(data)

# Changing the column names
colnames(data) <- c('instant', 'date', 'season', 'year', 'month', 'holiday', 'weekday', 'workingday', 'weather', 'temp', 'atemp', 'humidity', 'windspeed', 'casual', 'registered', 'count')

# getting some information about the data
str(data)
summary(data)

# Exploration
hist(data$season)
hist(data$weather)
hist(data$humidity)
hist(data$holiday)
hist(data$workingday)
hist(data$temp)
hist(data$atemp)
hist(data$windspeed)

# Checking for missing values
data[!complete.cases(data),]

# factoring some variables from numeric
data$season=as.factor(data$season)
data$weather=as.factor(data$weather)
data$holiday=as.factor(data$holiday)
data$workingday=as.factor(data$workingday)


# extracting days of week from datetime
#date=substr(data$datetime,1,10)
days<-weekdays(as.Date(data$date))
data$day=days


# creating boxplots for rentals with different variables to see the variation
boxplot(data$registered~data$weekday,xlab="day", ylab="registered users")
boxplot(data$casual~data$weekday,xlab="day", ylab="casual users")

boxplot(data$registered~data$weather,xlab="weather", ylab="registered users")
boxplot(data$casual~data$weather,xlab="weather", ylab="casual users")

boxplot(data$registered~data$temp,xlab="temp", ylab="registered users")
boxplot(data$casual~data$temp,xlab="temp", ylab="casual users")

# extracting year from data
data$year=substr(data$date,1,4)
data$year=as.factor(data$year)

# again some boxplots with different variables
# these boxplots give important information about the dependent variable with respect to the independent variables
boxplot(data$registered~data$year,xlab="year", ylab="registered users")
boxplot(data$casual~data$year,xlab="year", ylab="casual users")

boxplot(data$registered~data$windspeed,xlab="year", ylab="registered users")
boxplot(data$casual~data$windspeed,xlab="year", ylab="casual users")

boxplot(data$registered~data$humidity,xlab="humidity", ylab="registered users")
boxplot(data$casual~data$humidity,xlab="humidity", ylab="casual users")


#using decision trees for binning some variables, this was a really important step in feature engineering

f=rpart(registered~temp,data=data)
fancyRpartPlot(f)

f=rpart(casual~temp,data=data)
fancyRpartPlot(f)

data$temp_reg=0
data$temp_reg[data$temp<13]=1
data$temp_reg[data$temp>=13 & data$temp<23]=2
data$temp_reg[data$temp>=23 & data$temp<30]=3
data$temp_reg[data$temp>=30]=4

data$temp_cas=0
data$temp_cas[data$temp<15]=1
data$temp_cas[data$temp>=15 & data$temp<23]=2
data$temp_cas[data$temp>=23 & data$temp<30]=3
data$temp_cas[data$temp>=30]=4

data$year_part[data$year=='2011']=1
data$year_part[data$year=='2011' & data$month>3]=2
data$year_part[data$year=='2011' & data$month>6]=3
data$year_part[data$year=='2011' & data$month>9]=4
data$year_part[data$year=='2012']=5
data$year_part[data$year=='2012' & data$month>3]=6
data$year_part[data$year=='2012' & data$month>6]=7
data$year_part[data$year=='2012' & data$month>9]=8
table(data$year_part)

# creating another variable day_type which may affect our accuracy as weekends and weekdays are important in deciding rentals
data$day_type=0

data$day_type[data$holiday==0 & data$workingday==0]="weekend"
data$day_type[data$holiday==1]="holiday"
data$day_type[data$holiday==0 & data$workingday==1]="working day"


plot(data$temp,data$count)

data$month=substr(data$date,6,7)
data$month=as.integer(data$month)

data$weekend=0
data$weekend[data$day_type=="weekend"]=1

str(data)

# converting all relevant categorical variables into factors to feed to our random forest model
data$season=as.factor(data$season)
data$holiday=as.factor(data$holiday)
data$workingday=as.factor(data$workingday)
data$weather=as.factor(data$weather)
data$month=as.factor(data$month)
data$day=as.factor(data$day)
data$day_type=as.factor(data$day_type)
data$temp_cas=as.factor(data$temp_cas)
data$temp_reg=as.factor(data$temp_reg)

# Splitting dataset into test and train
library(caTools)
set.seed(123)   #  set seed to ensure you always have same random numbers generated
sample = sample.split(data,SplitRatio = 0.80) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train =subset(data ,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test=subset(data , sample==FALSE)


# log transformation for some skewed variables, which can be seen from their distribution
train$reg1=train$registered+1
train$cas1=train$casual+1
train$count1=train$count+1 
train$logcas=log(train$cas1)
train$logreg=log(train$reg1)
train$logcount=log(train$count1)
test$logreg=0
test$logcas=0
test$logcount=0

boxplot(train$logreg~train$weather,xlab="weather", ylab="registered users")
boxplot(train$logreg~train$season,xlab="season", ylab="registered users")

# Random forest model

set.seed(415)
fit1 <- randomForest(logreg ~ workingday+holiday+temp_reg+humidity+atemp+windspeed+season+weather+weekend+year+year_part, data=train,importance=TRUE, ntree=250)

pred1=predict(fit1,test)
test$logreg=pred1
 
set.seed(415)
fit2 <- randomForest(logcas ~humidity+atemp+temp_cas+windspeed+season+weather+holiday+workingday+weekend+year+year_part, data=train,importance=TRUE, ntree=250)

pred2=predict(fit2,test)
test$logcas=pred2

set.seed(415)
fit3 <- randomForest(logcount ~humidity+atemp+temp_cas+windspeed+season+weather+holiday+workingday+weekend+year+year_part, data=train,importance=TRUE, ntree=500)

pred3=predict(fit3,test)
test$logcount=pred3


rmse <- function(pred, actual) {
  sqrt(mean((pred - actual)^2))
}
 
#creating the final submission file
test$p_registered=exp(test$logreg)-1
test$p_casual=exp(test$logcas)-1
test$p_count=exp(test$logcount)-1
#test$p_count=test$p_casual+test$p_registered
s<-data.frame(instant=test$instant,count=test$p_count)
write.csv(s,file="submit.csv",row.names=FALSE)


head(s)


#rmse(test$p_count, test$count)



