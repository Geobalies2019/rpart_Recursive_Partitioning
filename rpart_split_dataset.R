library(plyr)
library(readr)
library(dplyr)
library(rpart,quietly = TRUE)
library(caret,quietly = TRUE)
library(rpart.plot,quietly = TRUE)
library(ggplot2)     # for awesome plotting
# Model interpretability packages
library(rpart.plot)  # for plotting decision trees
library(vip,quietly = TRUE)         # for feature importance
library(pdp,quietly = TRUE)         # for feature effects


# set working directory
setwd("C:/Users/xxxxx/")

# Load your CSV data
#data <- read.csv("dataname.csv")

#Splitting the data into training and testing sets
set.seed(12345) # for reproducibility
train <- sample(1:nrow(data),size = ceiling(0.80*nrow(data)),replace = FALSE)
# training set
data_train <- data[train,]
# test set
data_test <- data[-train,]

#Creating a Classification Tree
# panalty matrix: 

penalty.matrix <- matrix(c(0,1,10,0), byrow=TRUE, nrow=2)
#cost_matrix <- matrix(c(0, 10, 5, 0), nrow = 2, byrow = TRUE)
# building the classification tree with rpart
tree1 <- rpart(label~.,
              data=data_train,
              parms = list(loss = penalty.matrix),
              method = "class")
# Assuming 'tree1' is your decision tree model
rpart.plot(tree1)

printcp(tree1) # display the results
plotcp(tree1) # visualize cross-validation results
summary(tree1) # detailed summary of splits


#Prunning the tree using the best complexity parameter
#Note to self: there is noneed t prune the tree, because the penalty matrix contrained the tree and the pruning give same outcome.
# choosing the best complexity parameter "cp" to prune the tree
cp.optim <- tree1$cptable[which.min(tree1$cptable[,"xerror"]),"CP"]
# tree prunning using the best complexity parameter. For more in
tree2 <- prune(tree1, cp=cp.optim)

rpart.plot(tree2)
printcp(tree2) # display the results
plotcp(tree2) # visualize cross-validation results
summary(tree2) # detailed summary of splits


# plot the pruned tree
plot(tree2, uniform=TRUE,
     main="Pruned Classification Tree")
text(tree2, use.n=TRUE, all=TRUE, cex=.8)


#Predictions on test set
#Finally we proceed to test the model on the test dataset.
pred <- predict(object=tree2,data_test,type="class")
t <- table(data_test$label,pred)
confusionMatrix(t)

