#The data has been spilt into two groups:
#training set and testing set
df.train <- read.csv("/Users/satyamaddali/Desktop/R for data science/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Machine Learning with R/titanic_train.csv")
head(df.train)
#Exploratory Data Analysis
install.packages("Amelia")
library(Amelia)
missmap(df.train,main = "Missing Data", col = c('yellow','black'), legend = FALSE)
#Data Visualization
library(ggplot2)
print(ggplot(df.train, aes(Survived))+ geom_bar())
print(ggplot(df.train, aes(Pclass)) + geom_bar(aes(fill = factor(Pclass))))
print(ggplot(df.train, aes(Sex)) + geom_bar(aes(fill = factor(Sex)), alpha= 0.4))
print(ggplot(df.train, aes(Age)) + geom_histogram(aes(fill = Age), alpha= 0.4, color = 'blue', fill = 'pink'))
ggplot(df.train,aes(SibSp)) + geom_bar(fill='red',alpha=0.5)
ggplot(df.train,aes(Fare)) + geom_histogram(fill='green',color='black',alpha=0.5)
#Data Cleaning
pl <- ggplot(df.train,aes(Pclass,Age)) + geom_boxplot(aes(group=Pclass,fill=factor(Pclass),alpha=0.4))
print(pl + scale_y_continuous(breaks = seq(min(0), max(80), by = 2)))
#We have found out that first class passengers are older than second class, who in turn are older than the third class
impute_age <- function(age,class){
  out <- age
  for (i in 1:length(age)){
    
    if (is.na(age[i])){
      
      if (class[i] == 1){
        out[i] <- 37
        
      }else if (class[i] == 2){
        out[i] <- 29
        
      }else{
        out[i] <- 24
      }
    }else{
      out[i]<-age[i]
    }
  }
  return(out)
}
fixed.ages <- impute_age(df.train$Age,df.train$Pclass)
df.train$Age <- fixed.ages
missmap(df.train, main="Titanic Training Data - Missings Map",col=c("yellow", "black"), legend=FALSE)
#We notice that there is no missing data now
#Building a logistic regression model
str(df.train)
library(dplyr)
#Lets remove columns we wont use
df.train <- select(df.train,-PassengerId,-Name,-Ticket,-Cabin,-Parch)
head(df.train,3)
str(df.train)
df.train$Survived <- factor(df.train$Survived)
df.train$Pclass <- factor(df.train$Pclass)
df.train$SibSp <- factor(df.train$SibSp)
#Train the Model
log.model <- glm(Survived ~ ., family = binomial(link = 'logit'), data = df.train)
summary(log.model)
#Cleaning the test data
df.test <- read.csv("/Users/satyamaddali/Desktop/R for data science/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Machine Learning with R/titanic_test.csv")
head(df.test)
df.test <- select(df.test,-PassengerId,-Name,-Ticket,-Cabin,-Parch)
head(df.test,3)
str(df.test)
df.test$Pclass <- factor(df.test$Pclass)
df.test$SibSp <- factor(df.test$SibSp)
head(df.test)
str(df.test)
str(df.train)
#Predicting using Test cases
fitted.probabilities <- predict(log.model, newdata = df.test, type = 'response')
head(fitted.probabilities)
fitted.results <- ifelse(fitted.probabilities > 0.5,1,0)
head(fitted.results)
#Calculating the Accuracy
misClasificError <- mean(fitted.results != df.test$Survived)
print(paste('Accuracy', 1 -misClasificError))
#Creating the confusion table
table(df.test$Survived, fitted.probabilities > 0.5)