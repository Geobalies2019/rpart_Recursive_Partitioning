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

# deleting useless variable 
data$runoffclass <- NULL
data$comment <- NULL
data$SN <- NULL


#Remove any NA (null) values
data=na.omit(data)


# Count the occurrences of each label
label_counts <- table(data$label)
# Convert the table to a data frame
label_counts_df <- as.data.frame(label_counts)
names(label_counts_df) <- c("Label", "Count")
# Create a bar plot using ggplot2
bar_plot <- ggplot(label_counts_df, aes(x = Label, y = Count, fill = Label)) +
  geom_bar(stat = "identity") +
  labs(title = "Label Counts",
       x = "Label",
       y = "Count") +
  theme_minimal()
# Show the plot
print(bar_plot)



# Get the column names from the data frame (excluding the target column)
predictor_columns <- setdiff(names(data), "label")
# Create the formula for the decision tree dynamically
formula <- as.formula(paste("label ~", paste(predictor_columns, collapse = " + ")))

# Create a simple decision tree model without tunning parameters
tree_model <- rpart(formula, data = data,method="class")

# regular plots do not look good so lets use rpart.plot()
#rpart.plot(tree_model,type=0,extra=3,box.palette=0) # plot the tree

printcp(tree_model) # display the results
plotcp(tree_model) # visualize cross-validation results
summary(tree_model) # detailed summary of splits
rpart.plot(tree_model)

#Prunning the tree using the best complexity parameter
# choosing the best complexity parameter "cp" to prune the tree
cp.optim <- tree_model$cptable[which.min(tree_model$cptable[,"xerror"]),"CP"]
# tree prunning using the best complexity parameter. For more in
tree_model2 <- prune(tree_model, cp=cp.optim)

printcp(tree_model2) # display the results
plotcp(tree_model2) # visualize cross-validation results
summary(tree_model2) # detailed summary of splits
rpart.plot(tree_model2)

#------------------------------------------------------------------------------------

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



# Assuming 'data_test' is your testing dataset
# Predict the class labels using the trained decision tree model 'tree1'
predicted_labels <- predict(tree_model, newdata = data, type = "class")
# Compare predicted labels with actual labels and calculate accuracy
accuracy <- sum(predicted_labels == data$label) / nrow(data)
# Print the accuracy
print(paste("Accuracy:", accuracy))



# Prune the decision tree
pruned_tree <- prune(tree1, cp = 0.01442308)  # Specify the desired CP value for pruning
# Plot the pruned tree
plotcp(pruned_tree)





#Second method of prunning the tree by optimizing the cp value
library(rpart)

# Assuming you have 'data_train' and 'data_test' datasets

# Create the penalty matrix
penalty.matrix <- matrix(c(0, 1, 10, 0), byrow = TRUE, nrow = 2)

# Create a range of CP values to consider
cp_values <- seq(0.01, 0.5, by = 0.01)

# Initialize variables to store best accuracy and corresponding CP value
best_accuracy <- 0
best_cp <- 0

# Loop through each CP value and evaluate accuracy
for (cp_value in cp_values) {
  # Build the classification tree with the current CP value
  tree <- rpart(label ~ .,
                data = data_train,
                parms = list(loss = penalty.matrix),
                method = "class",
                control = rpart.control(cp = cp_value))
  
  # Make predictions on the test set
  predictions <- predict(tree, newdata = data_test, type = "class")
  
  # Calculate accuracy
  accuracy <- sum(predictions == data_test$label) / nrow(data_test)
  
  # Check if this CP value gives a higher accuracy
  if (accuracy > best_accuracy) {
    best_accuracy <- accuracy
    best_cp <- cp_value
  }
}

# Print the best CP value and accuracy
cat("Best CP value:", best_cp, "\n")
cat("Best accuracy:", best_accuracy, "\n")


#-------------------------------------------
library(rpart)

# Assuming you have 'data_train' and 'data_test' datasets

# Create the penalty matrix
penalty.matrix <- matrix(c(0, 1, 10, 0), byrow = TRUE, nrow = 2)

# Create a range of CP values to consider
cp_values <- seq(0.01, 0.5, by = 0.01)

# Initialize a data frame to store results
results <- data.frame(CP = numeric(0), Accuracy = numeric(0))

# Loop through each CP value and evaluate accuracy
for (cp_value in cp_values) {
  # Build the classification tree with the current CP value
  tree <- rpart(label ~ .,
                data = data_train,
                parms = list(loss = penalty.matrix),
                method = "class",
                control = rpart.control(cp = cp_value))
  
  # Make predictions on the test set
  predictions <- predict(tree, newdata = data_test, type = "class")
  
  # Calculate accuracy
  accuracy <- sum(predictions == data_test$label) / nrow(data_test)
  
  # Store CP value and accuracy in the results data frame
  results <- rbind(results, data.frame(CP = cp_value, Accuracy = accuracy))
}

# Find the CP value with the highest accuracy
best_cp <- results$CP[which.max(results$Accuracy)]
best_accuracy <- max(results$Accuracy)

# Print the best CP value and accuracy
cat("Best CP value:", best_cp, "\n")
cat("Best accuracy:", best_accuracy, "\n")


#--------------------------------------------------------------------------------

# Build the decision tree with different CP values
tree_cp <- rpart(label ~ ., data = data_train, method = "class", parms = list(loss = cost_matrix))
# Create the pruning plot
plotcp(tree_cp)

library(e1071)
library(ggcorrplot)
library(caret)
library(randomForest)
library(gam)

predictor<- data[, !(names(data) %in% c("label"))]

#Support Vector Machines (SVM) for classification 
svmfit = svm(x= predictor,y= data$label, kernel = "linear", scale = TRUE)
roc_imp2 <- varImp(svmfit, scale = FALSE)
# Assuming 'svmfit' is your trained SVM model
vip(svmfit)
#Coefficients for Linear Kernel
coefficients(svmfit)


#Recursive Feature Elimination (RFE): Another approach is to perform Recursive Feature 
#Elimination (RFE) using cross-validation. 
# Assuming 'svmfit' is your trained SVM model
ctrl <- rfeControl(functions=rfFuncs, method="cv", number=10)
rfe_result <- rfe(predictor, data$label, sizes=c(1:ncol(predictor)), rfeControl=ctrl)
print(rfe_result)


#train random forest model and calculate feature importance
rf = randomForest(x= predictor,y= data$label)
var_imp <- varImp(rf, scale = FALSE)


#sort the score in decreasing order
var_imp_df <- data.frame(cbind(variable = rownames(var_imp), score = var_imp[,1]))
var_imp_df$score <- as.double(var_imp_df$score)
var_imp_df[order(var_imp_df$score,decreasing = TRUE),]


ggplot(var_imp_df, aes(x=reorder(variable, score), y=score)) + 
  geom_point() +
  geom_segment(aes(x=variable,xend=variable,y=0,yend=score)) +
  ylab("IncNodePurity") +
  xlab("Variable Name") +
  coord_flip()



#Recursive Feature elimination:RFE is used for feature selection, which involves identifying and 
#selecting the most important features from a dataset.
predictor <- data %>% 
  select(-label)

filterCtrl <- rfeControl(functions=rfFuncs, method="cv", number=3)
results <- rfe(x= predictor,y= data$label, sizes=c(1:11), rfeControl=filterCtrl)
results

# plot the results
plot(results, type=c("g", "o"))

#------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
binary.model <- rpart(label ~ ., data = data, method = "class", cp = 0.02)
rpart.plot(binary.model)






fit1 <- rpart(formula, data = data_train, parms = list(split = 'gini'))
fit2 <- rpart(label~., data = data_train, parms = list(split = 'information'))
par(mfrow = c(1,2), mar = rep(0.1, 4))
plot(fit1, margin = 0.05); text(fit1, use.n = TRUE, cex = 0.8)
plot(fit2, margin = 0.05); text(fit2, use.n = TRUE, cex = 0.8)
summary(fit1, cp = 0.06)




rpart(label~., data, parms=list(split=c("information","gini")),
      cp = 0.01, minsplit=20, minbucket=7, maxdepth=30)


default.model <- rpart(label~., data = data_train)
info.model <- rpart(label~., data = data_train, parms=list(split="information"))


overfit.model <- rpart(label~., data = data_train,
                       maxdepth= 5, minsplit=2,
                       minbucket = 1)


#Single Rule Model
one.rule.model <- rpart(label~., data=data_train, maxdepth = 1)
rpart.plot(one.rule.model, main="Single Rule Model")



super.overfit.model <- rpart(label~., data = data_train, minsplit=2,
                             minbucket = 1, cp = 0.0001)
rpart.plot(super.overfit.model, main = "Really Overfit")



cost.driven.model <- rpart(label~.,data=data_train,
                           parms=list(
                             loss=matrix(c(0,1,5,0),
                                         byrow=TRUE,
                                         nrow=2))
)



print(cost.driven.model)
printcp(cost.driven.model)
summary(cost.driven.model)




model_dt1 <- rpart(
  formula = label ~ .,
  data    = data_train,
  method  = "class"
)


model_dt1
rpart.plot(model_dt1)
plotcp(model_dt1)




ames_dt2 <- rpart(
  formula = label ~ .,
  data    = data_train,
  method  = "class", 
  control = list(cp = 0, xval = 10)
)
plotcp(ames_dt2)
abline(v = 11, lty = "dashed")



model_dt1$cptable

# caret cross validation results
ames_dt3 <- train(
  label ~ .,
  data = data_train,
  method = "rpart",
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 20
)

ggplot(ames_dt3)


vip(ames_dt3, num_features = 40, bar = FALSE)


# Construct partial dependence plots
p1 <- partial(ames_dt3, pred.var = "Elevation") %>% autoplot()
p2 <- partial(ames_dt3, pred.var = "claypct") %>% autoplot()
p3 <- partial(ames_dt3, pred.var = c("Elevation", "claypct")) %>% 
  plotPartial(levelplot = FALSE, zlab = "yhat", drape = TRUE, 
              colorkey = TRUE, screen = list(z = -20, x = -60))

#--------------------------------------------------------------------------
library(caret)

# Create a cost matrix (penalty matrix) for binary classification
cost_matrix <- matrix(c(0, 10, 5, 0), nrow = 2, byrow = TRUE)

# Define a custom summary function that calculates costs based on the class probabilities
custom_summary <- function(data, lev = NULL, model = NULL) {
  probs <- data$prob
  predicted_class <- ifelse(probs >= 0.5, 1, 0)  # Assuming threshold is 0.5
  confusion <- confusionMatrix(predicted_class, data$obs, positive = "1")
  costs <- sum(confusion$byClass * cost_matrix)
  c(accuracy = confusion$overall['Accuracy'], cost = costs)
}

# Define train control with classProbs and the custom summary function
ctrl <- trainControl(method = "cv", classProbs = TRUE, summaryFunction = custom_summary)

#
# Train your model using train function and ctrl
model <- train(label ~ ., data = data, method = "glm", trControl = ctrl)
#------------------------------------------------------------------------------


library(rpart.plot)
prp(tree_model,extra=1) #Initial tree with 16 splits
prp(prune(tree_model,cp=0.042),extra=1) #Subtree with 10 splits
prp(prune(tree_model,cp=0.068),extra=1) #Subtree with 5 splits
prp(prune(tree_model,cp=0.14),extra=1) #Subtree with 1 split




#Classification tree using rpart (100% Accuracy)
number.perfect.splits <- apply(X=data, MARGIN = 2, FUN = function(col){
  t <- table(data$label,col)
  # we will use how many perfect splits as a proxy for how good is a variable
  # in splitting the data into classes edible and poisonous
  sum(t == 0)
})
# order by number of prefect splits decreasing
order <- order(number.perfect.splits,decreasing = TRUE)
number.perfect.splits <- number.perfect.splits[order]
# plot results
par(mar=c(10,2,2,2))
barplot(number.perfect.splits,
        main="Number of perfect splits vs feature",
        xlab="",ylab="Feature",las=2,col="wheat")
#-----------------------------------------------------------
# Confusion matrix values
TP <- 56
TN <- 25
FP <- 83
FN <- 8

# Confusion matrix values
TP <- 15
TN <- 89
FP <- 57
FN <- 15


# Confusion matrix values
TP <- 0   # True Positives
TN <- 148 # True Negatives
FP <- 0   # False Positives
FN <- 17  # False Negatives


# Compute metrics
accuracy <- (TP + TN) / (TP + TN + FP + FN)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
specificity <- TN / (TN + FP)
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print the computed metrics
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("Specificity:", specificity, "\n")
cat("F1 Score:", f1_score, "\n")




library(rattle)
rattle()
