library(caret)
library(e1071) # For SVM
library(nnet) # For multinomial logistic regression
library(rpart) # For decision trees
library(randomForest)
library(ROSE) # For SMOTE and Near Miss
library(pROC) # For AUC and ROC
library(naivebayes)
library(class) # For KNN

df <- read.csv("C:/Users/susha/OneDrive/Desktop/online_shoppers_intention.csv")

df$Month <- as.factor(df$Month)
df$VisitorType <- as.factor(df$VisitorType)
df$Weekend <- as.integer(df$Weekend)
df$Revenue <- as.factor(df$Revenue)

set.seed(530)
train_control <- trainControl(method="cv", number=10, savePredictions = "final")

# Naive Bayes
model_nb <- train(Revenue ~ ., data=df, method="naive_bayes", trControl=train_control)

# Logistic Regression
model_lr <- train(Revenue ~ ., data=df, method="multinom", trControl=train_control, trace=FALSE)

# Decision Tree
model_dt <- train(Revenue ~ ., data=df, method="rpart", trControl=train_control)

# Random Forest
model_rf <- train(Revenue ~ ., data=df, method="rf", trControl=train_control)

# K-Nearest Neighbors
model_knn <- train(Revenue ~ ., data=df, method="knn", trControl=train_control, tuneGrid=data.frame(k=5))

# Support Vector Machine
model_svm <- train(Revenue ~ ., data=df, method="svmRadial", trControl=train_control, probability=TRUE)

# Summary of models
results <- resamples(list(naive_bayes=model_nb, log_reg=model_lr, dec_tree=model_dt, rand_forest=model_rf, knn=model_knn, svm=model_svm))
summary(results)

