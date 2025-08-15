library(dplyr)
library(ggplot2)
library(caret)
library(e1071) # For SVM
library(nnet) # For multinomial logistic regression
library(rpart) # For decision trees
library(randomForest)
library(ROSE) # For SMOTE and Near Miss
library(pROC) # For AUC and ROC
library(naivebayes)
library(rpart)
library(rpart.plot)
df <- read.csv("C:/Users/susha/OneDrive/Desktop/online_shoppers_intention.csv")

str(df)

sapply(df, function(x) sum(is.na(x)))

ggplot(data=df, aes(x=Revenue)) + geom_bar(fill=c("#FF9999", "#66B3FF")) + labs(title="Revenue Generated")


df$Month <- as.factor(df$Month)
df$VisitorType <- as.factor(df$VisitorType) # factor data types 
df$Weekend <- as.integer(df$Weekend)
df$Revenue <- as.integer(df$Revenue)

str(df)

summary(df)

set.seed(530)
trainIndex <- createDataPartition(df$Revenue, p=0.8, list=FALSE)
#trainIndex <- createDataPartition(df$Revenue, p=0.5, list=FALSE)
train <- df[trainIndex,]
test <- df[-trainIndex,]
class_counts <- table(train$Revenue)
class_counts
train_smote <- ovun.sample(Revenue ~ ., data=train, method="over")$data

class_counts <- table(train_smote$Revenue)
class_counts

train$Revenue <- as.factor(train$Revenue)
train_smote$Revenue <- as.factor(train_smote$Revenue)
train_nearmiss <- ovun.sample(Revenue ~ ., data=train, method="under")$data

class_counts <- table(train_nearmiss$Revenue)
class_counts

# Naive Bayes
model_nb <- naive_bayes(Revenue ~ ., data=train)
#model_nb <- naive_bayes(Revenue ~ ., data=train_smote)
#model_nb <- naive_bayes(Revenue ~ ., data=train_nearmiss)
pred_nb <- predict(model_nb, test)
pred_nb <- factor(pred_nb, levels = levels(factor(df$Revenue)))
test$Revenue <- factor(test$Revenue, levels = levels(factor(df$Revenue)))
confusionMatrix(pred_nb, test$Revenue)



# Logistic Regression
model_lr <- multinom(Revenue ~ ., data=train)
#model_lr <- multinom(Revenue ~ ., data=train_smote)
#model_lr <- multinom(Revenue ~ ., data=train_nearmiss)
pred_lr <- predict(model_lr, test)
pred_lr <- factor(pred_lr, levels = levels(factor(df$Revenue)))
test$Revenue <- factor(test$Revenue, levels = levels(factor(df$Revenue)))
confusionMatrix(pred_lr, test$Revenue)


# Decision Tree
model_dt <- rpart(Revenue ~ ., data=train)
#model_dt <- rpart(Revenue ~ ., data=train_smote)
#model_dt <- rpart(Revenue ~ ., data=train_nearmiss)
pred_dt <- predict(model_dt, test, type="class")
pred_dt <- factor(pred_dt, levels = levels(factor(df$Revenue)))
test$Revenue <- factor(test$Revenue, levels = levels(factor(df$Revenue)))
confusionMatrix(pred_dt, test$Revenue)

# Random Forest
model_rf <- randomForest(Revenue ~ ., data=train)
#model_rf <- randomForest(Revenue ~ ., data=train_smote)
#model_rf <- randomForest(Revenue ~ ., data=train_nearmiss)
pred_rf <- predict(model_rf, test)
pred_rf <- factor(pred_rf, levels = levels(factor(df$Revenue)))
test$Revenue <- factor(test$Revenue, levels = levels(factor(df$Revenue)))
confusionMatrix(pred_rf, test$Revenue)

# K-Nearest Neighbors
model_knn <- knn3(Revenue ~ ., data=train, k=5)
#model_knn <- knn3(Revenue ~ ., data=train_smote,k=5)
#model_knn <- knn3(Revenue ~ ., data=train_nearmiss,k=5)
pred_knn <- predict(model_knn, test, type="class")
pred_knn <- factor(pred_knn, levels = levels(factor(df$Revenue)))
test$Revenue <- factor(test$Revenue, levels = levels(factor(df$Revenue)))
confusionMatrix(pred_knn, test$Revenue)

# Support Vector Machine
model_svm <- svm(Revenue ~ ., data=train, probability=TRUE)
#model_svm <- svm(Revenue ~ ., data=train_smote, probability=TRUE)
#model_svm <- svm(Revenue ~ ., data=train_nearmiss, probability=TRUE)
pred_svm <- predict(model_svm, test, probability=TRUE)
pred_svm <- factor(pred_svm, levels = levels(factor(df$Revenue)))
test$Revenue <- factor(test$Revenue, levels = levels(factor(df$Revenue)))
confusionMatrix(pred_svm, test$Revenue)





