---
title: "NBA"
author: "Aditya Padal"
date: "28 November 2015"
output: html_document
---

Setting the working directory

```{r}
getwd()
setwd("~path")
```
reading the raw data into R dataframe

```{r}
NBA<-read.csv("DataClean_v1.csv")
str(NBA)
head(NBA)
```

Changing the datatypes of some variables
```{r}
NBA[!complete.cases(NBA)] #no missing values
lapply(NBA[8:ncol(NBA)], function(x) boxplot(x)) #to check for outliers
NBA$Contract<- factor(NBA$Contract)
str(NBA)
```

scaling the variables
```{r}
for (i in 8:ncol(NBA))
{
  NBA[,i] <- scale(NBA[,i])
}

head(NBA)
summary(NBA)
```

checking for correlation
```{r}
tmp<-round(cor(NBA[,8:ncol(NBA)],y=NULL),2)
tmp[upper.tri(tmp)] <- NA
diag(tmp)<- NA
tmp
NBA_cor<- NBA[,c(-2,-5,-6,-12,-15,-17,-18,-22,-26,-32)]
head(NBA_cor)
names(NBA_cor)[names(NBA_cor)=="Annual.Salary..."] <- "Salary"
```

partitioning the training set into training and testing:

```{r}
set.seed(1)
train <- sample(1:nrow(NBA_cor), nrow(NBA_cor)*0.7)
test = -train
training<- NBA_cor[train,]
testing<- NBA_cor[test,]
nrow(training)
nrow(testing)

test_Salary <- testing$Salary
```
running the full linear model
```{r}
model_nba <- lm(Salary~., data = training)
summary(model_nba) #0.67
pred_model_nba <- predict(model_nba,testing)
RMSE = sqrt(sum((test_Salary - pred_model_nba)^2))/nrow(testing)
RMSE
ss_total <- sum((test_Salary - mean(testing$Salary))^2)
ss_residuals <- sum((test_Salary - pred_model_nba)^2)
testing_Rsquare <- 1- (ss_residuals/ss_total)
testing_Rsquare #0.62



###with selected variables
model_nba1 <- lm(Salary~Contract+Pos+Age+GS+MP+FG+FGC+X3PC+X2PC+EFG+FT+ORB+
                  AST+STL, data = training)
summary(model_nba1) #0.66
pred_model_nba1 <- predict(model_nba1,testing)
RMSE1 = sqrt(sum((test_Salary - pred_model_nba1)^2))/nrow(testing)
RMSE1
ss_total1 <- sum((test_Salary - mean(testing$Salary))^2)
ss_residuals1 <- sum((test_Salary - pred_model_nba1)^2)
testing_Rsquare1 <- 1- (ss_residuals1/ss_total1)
testing_Rsquare1 #0.61

```

Cross-validation:

```{r}

#choosing k = 10

k<-10
a<-k
l<- floor(nrow(NBA_cor)/k)
l

for (i in 1:l)
{
  p <- k-9
  test <- NBA_cor[(p:k),]
  train <- NBA_cor[-(p:k),]
  model_cv <- lm(Salary~Contract+Pos+Age+GS+MP+FG+FGC+X3PC+X2PC+EFG+FT+ORB+
                  AST+STL, data = train)
  summary(model_cv) 
  pred_model_cv <- predict(model_cv,test)
  RMSE1 = sqrt(sum((test$Salary - pred_model_cv)^2))/nrow(test)
  RMSE1
  ss_total1 <- sum((test$Salary - mean(test$Salary))^2)
  ss_residuals1 <- sum((test$Salary - pred_model_cv)^2)
  testing_Rsquare1 <- 1- (ss_residuals1/ss_total1)
  y[i] <- testing_Rsquare1
  x[i] <- summary(model_cv)$r.squared
  
  print(c(i,p,k,summary(model_cv)$r.squared, testing_Rsquare1, summary(model_cv)$r.squared -  testing_Rsquare1))
  
  k <- k + a
}

#choosing the best model from cross validation

model_cv_best <- lm(Salary~Contract+Pos+Age+GS+MP+FG+FGC+X3PC+X2PC+EFG+FT+ORB+
                  AST+STL, data = train[-(91:100),])
summary(model_cv_best) 











