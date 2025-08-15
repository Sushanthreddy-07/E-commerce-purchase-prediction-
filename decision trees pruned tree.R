library(rpart)
library(rpart.plot)
library(caret)
data <- read.csv("C:/Users/susha/OneDrive/Desktop/online_shoppers_intention.csv")

set.seed(123)  # for reproducibility
trainIndex <- createDataPartition(data$Revenue, p = .8, list = FALSE, times = 1)
#trainIndex <- createDataPartition(data$Revenue, p = .5, list = FALSE, times = 1)
trainData <- data[trainIndex,]
testData <- data[-trainIndex,]


initial_tree <- rpart(Revenue ~ ., data = data, method = "class")

printcp(initial_tree)

optimal_cp <- initial_tree$cptable[which.min(initial_tree$cptable[, "xerror"]), "CP"]
pruned_tree <- prune(initial_tree, cp = optimal_cp)

rpart.plot(pruned_tree)

summary(pruned_tree)

count_terminal_nodes <- function(tree) {
  sum(tree$frame$var == "<leaf>")
}

optimal_size <- count_terminal_nodes(pruned_tree)
cat("The optimal size of the pruned tree, measured as the number of terminal nodes, is:", optimal_size, "\n")
count_terminal_nodes <- function(tree) {
  sum(tree$frame$var == "<leaf>")
}

optimal_size <- count_terminal_nodes(pruned_tree)
cat("The optimal size of the pruned tree, measured as the number of terminal nodes, is:", optimal_size, "\n")

pred <- predict(pruned_tree, testData, type = "class")
pred <- factor(pred, levels = levels(factor(data$Revenue)))  # Corrected from df to data
testData$Revenue <- factor(testData$Revenue, levels = levels(factor(data$Revenue)))  # Corrected from test to testData
confusionMatrix(pred, testData$Revenue)

