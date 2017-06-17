#################### Intsall the packages & Load the Libararies ###############

install.packages("descr")
install.packages("gmodels")
install.packages("caret")
install.packages("Rlab")
install.packages("C50")
install.packages("ggthemes")
library(descr)
library(gmodels)
library(Rlab)
library(dplyr)
library(lattice)
library(ggplot2)
library(MASS)
library(DMwR)
library(C50)
library(ggthemes)


###################### Clear Environment Variables ###########################

rm(list=ls(all=TRUE))

######################### Load the CSV files ###############################

#titan_train = read.csv("train.csv", header = T, sep = ",", stringsAsFactors = F)
train <- read.csv("train.csv", header = T, sep = ",", stringsAsFactors = F)
test <- read.csv("test.csv", header = T, sep = ",", stringsAsFactors = F)

######################## Combine Train & Test Data #######################

test$Survived <- NA

titan_comb <- rbind(train,test)
View(titan_comb)

###################### Summary ###########################################

head(titan_comb)
str(titan_comb)
summary(titan_comb)


############## Data Preparation and Exploratory Analysis ################

# Data Cleanup
# Replace the missing values of age with random sample from raw data

age <- titan_comb$Age
n <- length(age)

set.seed(123)
age_na_rows = which(is.na(age))
titan_comb$Age[age_na_rows] = sample(na.omit(titan_comb$Age), length(age_na_rows))
sum(is.na(titan_comb$Age))
# check effect
par(mfrow=c(1,2))

hist(titan_comb$Age, freq=F, main='Before Replacement', 
  col='lightblue', ylim=c(0,0.04),xlab = "age")

hist(age, freq=F, main='After Replacement', 
  col='darkblue', ylim=c(0,0.04))

#the histograms above that there is not much significant change of age distribution, 
#which means the replacement is appropriate.

#  create a new Cabin column to indicate how many cabins the passenger has.

# Process Cabin Column to show number of cabins passenger has
cabin_counts = strsplit(titan_comb$Cabin," ")
cabin = sapply(cabin_counts, length)
table(cabin)

# process fare column

# check missing
titan_comb$PassengerId[is.na(titan_comb$Fare)]

titan_comb[1044,]

table(titan_comb$Embarked)

ggplot(titan_comb[titan_comb$Pclass == '3' & titan_comb$Embarked == 'S', ], 
       aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='red', linetype='dashed', lwd=1)

# we can see that fare is clustered around mode. we just repace the missing value with 
# median fare of according Pclass and Embarked

titan_comb$Fare[1044] <- median(titan_comb[titan_comb$Pclass == '3' & titan_comb$Embarked == 'S', ]$Fare, na.rm = TRUE)

# process embarked column

embarked <- titan_comb$Embarked
embarked[which(embarked == "")] = "S"
table(embarked)

##*****************************
# Categorical Varibale
#Crosstabs for categorical variables

# Age vs Survival

ggplot(titan_comb[1:891,], aes(Age, fill = factor(Survived))) + geom_histogram()+labs(title="Impact of Age on Survival",x ="Age", y = "Frequency")

#kids with very young age have a respectively higher survival rate, and elder people 
#have a respectively lower survival rate.

#Sex v.s Survival

CrossTable(titan_comb$Survived,titan_comb$Sex)

# create histgram to show effect of Sex on survival
ggplot(titan_comb[1:891,], aes(Sex,fill = factor(Survived))) + geom_histogram(stat = "count")

# female's survival rate is greater than male's.

# calculate survival rate
tapply(titan_comb[1:891,]$Survived,titan_comb[1:891,]$Sex,mean)

#female      male 
#0.7420382 0.1889081
#The survival rate of female is 0.74, while the survival rate of male is 0.19.

# Pclass vs Survival
CrossTable(titan_comb$Survived,titan_comb$Pclass)

# make a histogram
ggplot(titan_comb[1:891,], aes(Pclass,fill = factor(Survived))) + geom_histogram(stat = "count")


#Looking at this crosstab, we can see that "Pclass" could be a useful predictor of "Survived." Why? The first column of the crosstab 
#shows that of the passengers in Class 1, 136 survived and 80 died (ie. 63% of first class passengers survived). On the other hand, in Class 2, 
#87 survived and 97 died (ie. only 47% of second class passengers survived). Finally, in Class 3, 119 survived and 372 died (ie. only 24% of third 
#class passengers survived).


# calculate survival rate
tapply(titan_comb[1:891,]$Survived,titan_comb[1:891,]$Pclass,mean)

#Pclass = 1 group has the highest survival rate, then is Pclass = 2 group, and Pclass = 3 group has the lowest survival rate 
#within these three groups.

#Family Size v.s. Survival

# histogram of Parch
ggplot(titan_comb[1:891,], aes(Parch,fill = factor(Survived))) +
  geom_histogram(stat = "count")

# histogram of SibSp
ggplot(titan_comb[1:891,], aes(SibSp,fill = factor(Survived))) +
  geom_histogram(stat = "count")

# histogram of SibSp
ggplot(titan_comb[1:891,], aes(SibSp,fill = factor(Survived))) +
  geom_histogram(stat = "count")

#We can see that they have similar trend, then we decide to combine them together to 
#construct a column named family.

# combine SibSp and Parch 
family <- titan_comb$SibSp + titan_comb$Parch
d <- data.frame(family = family[1:891],Survived = titan_comb[1:891,]$Survived)
ggplot(d, aes(family,fill = factor(Survived))) +
  geom_histogram(stat = "count")

tapply(d$Survived,d$family,mean)

#the survival rate increases as the family size increases from 0 to 3. When family 
#size becomes greater than 3, survival rate decrease dramatically.

# create histogram
d <- data.frame(Cabin = cabin[1:891],Survived = titan_comb[1:891,]$Survived)
ggplot(d, aes(Cabin,fill = factor(Survived))) +
  geom_histogram(stat = "count")

# calculate survival rate
tapply(d$Survived,d$Cabin,mean)

#passenger who has no cabin has a lower survival rate, and passenger 
#who has one or more cabins has higher survival rate.

#Fare v.s. Survival

# make a histogram
ggplot(titan_comb[1:891,], aes(Fare,fill = factor(Survived))) +
  geom_histogram()

#Passengers who's fare is lower than 50 has a relatively lower survival rate. Passengers who's fare is
#extremely high (500-550) have very high survival rate.

#Embarked v.s. Survival

# make histogram
d <- data.frame(Embarked = embarked[1:891], Survived = train$Survived)
ggplot(d, aes(Embarked,fill = factor(Survived))) +
  geom_histogram(stat = "count")

# make table
tapply(train$Survived,train$Embarked,mean)

#Embarked C group has a relatively higher survival rate than other 2 groups.

#Name v.s. Survival

# extract title from Name 
# (here I process full data set but only plot title vs survival in train 
#    data set because there is no survival value for test data set)
get_titles = function() {
  names = as.character(titan_comb$Name) # Names as character so we can split the string
  split_before_title = strsplit(names, ",")
  part_with_title = sapply(split_before_title, "[[", -1) # Take elements after comma - there is title that we want
  split_after_title = strsplit(part_with_title, "[.]")
  part_with_title = sapply(split_after_title, "[[", 1) # Take only title
  titles = sapply(part_with_title, trimws)
  return(titles)
}
title = get_titles()

# make a histogram of title v.s survival
d <- data.frame(title = title[1:891],Survived = titan_comb[1:891,]$Survived)
ggplot(d, aes(title,fill = factor(Survived))) +
  geom_histogram(stat = "count")

# count of title
table(title)

# survival rate
tapply(d$Survived,d$title,mean)

#survival rates of females with Miss and Mrs title are close to the 
#average survival rate for female group 
#Survival rates of males with Master are higher than the average male group. Titles like Col, 
#Rev, Dr etc. also have influence on the survival.

# replace rare titles to 'Rare'
title[title != 'Mr' & title != 'Miss' & title != 'Mrs' & title != 'Master'] <- 'Rare'
table(title)

# Process Age again, but now take Title into account

age <- titan_comb$Age
age_title = as.data.frame(list(Age=age, Title=title))
set.seed(123)
get_age = function(title_to_get) {
  ages = na.omit(age_title[which(age_title$Title == title_to_get), "Age"])
  cat(title_to_get, "age range:", min(ages), "-", max(ages), "\n")
  #hist(ages, freq=F, main=cat(title_to_get, ' age distribution'), 
  #  col='lightblue', ylim=c(0,0.04),xlab = "age")
  age_na_rows = which(is.na(age_title$Age) & age_title$Title == title_to_get)
  age[age_na_rows] = sample(ages, length(age_na_rows))
  return(age)
}

age = get_age("Master")
age = get_age("Miss")
age = get_age("Mr")
age = get_age("Mrs")
age = get_age("Rare")

# check effect
par(mfrow=c(1,2))
hist(titan_comb$Age, freq=F, main='Before Replacement', 
     col='lightblue', ylim=c(0,0.04),xlab = "age")
hist(age, freq=F, main='After Replacement', 
     col='darkblue', ylim=c(0,0.04))

# Let's see survivals count across different ages again, but with new age distribution
d <- data.frame(Age = age[1:891], Survived = titan_comb[1:891,]$Survived)
ggplot(d, aes(Age,fill = factor(Survived))) +
  geom_histogram()

#### Modeling
# Feature Engineering

# response variable
f.survived = titan_comb[1:891,]$Survived
# feature
# 1. age
f.age = age[1:891]    # for training
t.age = age[892:1309]  # for testing

# 2. fare
f.fare = titan_comb$Fare[1:891]
t.fare = titan_comb$Fare[892:1309]

# 3. cabin
f.cabin = cabin[1:891]
t.cabin = cabin[892:1309]

# 4. title
f.title = title[1:891]
t.title = title[892:1309]

# 5. family
family <- titan_comb$SibSp + titan_comb$Parch
f.family = family[1:891]
t.family = family[892:1309]

# 6. plcass
f.pclass = titan_comb[1:891,]$Pclass
t.pclass = titan_comb[892:1309,]$Pclass

# 7. sex
f.sex = titan_comb[1:891,]$Sex
t.sex = titan_comb[892:1309,]$Sex

# 8. embarked
f.embarked = embarked[1:891]
t.embarked = embarked[892:1309]

#Model Training

# construct training data frame
new_train = data.frame(age = f.age, fare = f.fare , sex = f.sex, 
                       embarked = f.embarked ,family = f.family ,title = f.title ,cabin =  f.cabin, pclass= f.pclass, survived = f.survived)


# logistic regression
fit_logit <- glm(factor(survived) ~ age + fare + sex + embarked + family 
                 + title + cabin + pclass,data = new_train,family = binomial)

# predicted result of regression

predict_train_logit = predict(fit_logit, type="response", newdata=new_train)

# Confusion matrix with threshold of 0.5
table(new_train$survived, predict_train_logit > 0.5)

ans_logit = rep(NA,891)
for(i in 1:891){
  ans_logit[i] = round(fit_logit$fitted.values[[i]],0)
}
# check result
mean(ans_logit == train$Survived)
table(ans_logit)

# random forest
library('randomForest')

set.seed(123)
fit_rf <- randomForest(factor(survived) ~ age + fare + sex + embarked + family 
                       + title + cabin + pclass,data = new_train)

predict_train_rf = predict(fit_rf , type="response", newdata=new_train)

table(predict_train_rf)

# predicted result of regression
rf.fitted = predict(fit_rf)
ans_rf = rep(NA,891)
for(i in 1:891){
  ans_rf[i] = as.integer(rf.fitted[[i]]) - 1
}
# check result
mean(ans_rf == train$Survived)
table(ans_rf)

# decision tree
library(rpart)

fit_dt <- rpart(factor(survived) ~ age + fare + sex + embarked + family 
                + title + cabin + pclass,data = new_train)

predict_train_dt = predict(fit_dt ,newdata=new_train, type="class")

table(predict_train_dt)

# predicted result of regression
dt.fitted = predict(fit_dt)
ans_dt = rep(NA,891)
for(i in 1:891){
  if(dt.fitted[i,1] >= dt.fitted[i,2] ){
    ans_dt[i] = 0
  } else{
    ans_dt[i] = 1
  }
}
# check result
mean(ans_dt == train$Survived)
table(ans_dt)

# svm
library(e1071)

fit_svm <- svm(factor(survived) ~ age + fare + sex + embarked + family 
               + title + cabin + pclass,data = new_train)

predict_train_svm = predict(fit_svm ,newdata=new_train, type="response")

table(predict_train_svm)

# predicted result of regression
svm.fitted = predict(fit_svm)
ans_svm = rep(NA,891)
for(i in 1:891){
  ans_svm[i] = as.integer(svm.fitted[[i]]) - 1
}
# check result
mean(ans_svm == train$Survived)
table(ans_svm)

################ Model Evaluation ####################

#evaluate model accuracy using Confusion Matrix.

CrossTable(new_train$survived, predict_train_logit > 0.5,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual', 'predicted'))

CrossTable(new_train$survived, predict_train_rf,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual', 'predicted'))


CrossTable(new_train$survived, predict_train_dt,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual', 'predicted'))

CrossTable(new_train$survived, predict_train_svm,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual', 'predicted'))

#From matrix above, we can see that all models predict non-survival better than survival. And both logistic regression and SVM work well for training data set. Here, logistic 
# regression has accuracy = 0.837, SVM has accuracy = 0.836.

##Prediction

new_test = data.frame(age = t.age, fare = t.fare , sex = t.sex, 
                      embarked = t.embarked ,family = t.family ,title = t.title ,cabin =  t.cabin, pclass= t.pclass)


# make prediction
predict_svm = predict(fit_svm, newdata = new_test)
predict_svm = as.integer(predict_svm) - 1
table(predict_svm)

predict_logit = predict(fit_logit, newdata = new_test)
predict_logit = as.numeric(predict_logit > 0)
table(predict_logit)

predict_rf = predict(fit_rf, newdata = new_test)
predict_rf = as.integer(predict_rf) - 1
table(predict_rf)

predict_dt = predict(fit_dt, newdata = new_test)
predict_dt = ifelse(predict_dt[,1] >= predict_dt[,2], 0, 1)
table(predict_dt)


# create a csv file for submittion
d<-data.frame(PassengerId = test$PassengerId, Survived = predict_svm)
write.csv(d,file = "TitanicResultSvm.csv",row.names = F)

d<-data.frame(PassengerId = test$PassengerId, Survived = predict_logit)
write.csv(d,file = "TitanicResultLogit.csv",row.names = F)

d<-data.frame(PassengerId = test$PassengerId, Survived = predict_rf)
write.csv(d,file = "TitanicResultRF.csv",row.names = F)

d<-data.frame(PassengerId = test$PassengerId, Survived = predict_dt)
write.csv(d,file = "TitanicResultDT.csv",row.names = F)

