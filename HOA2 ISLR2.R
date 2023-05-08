#Student Name & Number

# View the current working directory
getwd()

# Change the working directory to a folder on your computer
setwd("C:/Users/amin/OneDrive/IEMBA MSC NEOMA BS/10 Financial Data and Machine Learning/A1 Hands On Assignment Group A")

# View the new working directory
getwd()

# Step 0 - Objective and Context -----------------------------------------

# Import Default data
library(ISLR2)
data(Default)


# Step 1 - Getting Modelling Started In R Studio --------------------------
data.full <- Default


# Step 2 - Data Exploration: Getting to Know Your Data --------------------

summary(data.full)

var(data.full)

cor(data.full[,3:4])

pairs(data.full, main="Default data Scatter Plots")


# Step 3 - Splitting the Data into a Training and Test Set ----------------

# Set seed for reproducibility
set.seed(123)

# Get indices of rows to include in training set
train_idx <- sample(nrow(data.full), size = round(0.8 * nrow(data.full)), replace = FALSE)

# Create training set
data.train <- data.full[train_idx, ]

# Create test set
data.test <- data.full[-train_idx, ]


# Step 4 - Implementing a Logistic Regression -----------------------------
# Fit logistic regression model
logistic.fitted <- glm(default ~ student + balance + income, data = data.train, family = "binomial")
summary(logistic.fitted)
coef(logistic.fitted)
summary(logistic.fitted)$coefficients[, "Std. Error"]

#4.1 Training Set Performance of the Logistic Regression Model

# Fit a GLM model
model <- glm(default ~ student + balance + income, data = data.train, family = binomial)

# Create vector of predictions for training set
glm.pred.train <- predict(model, newdata = data.train, type="response")
glm.pred.train <- ifelse(glm.pred.train > 0.5, 1, 0)
# Create confusion matrix
confusion_matrix <- table(glm.pred.train, data.train[,1])
print(confusion_matrix)

# Compute training set prediction accuracy
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
print(accuracy)

#Question
#1. What is the prediction accuracy? 
#2. How does it compare to a random guess?
  
#4.2 Test Set Performance of the Logistic Regression Model 
# Create vector of predictions for training set
glm.pred.test <- predict(model, newdata = data.test, type="response")
glm.pred.test <- ifelse(glm.pred.test > 0.5, 1, 0)

# Create confusion matrix
confusion_matrix <- table(glm.pred.test, data.test[,1])
print(confusion_matrix)

#Compute test set prediction accuracy
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
print(accuracy)

#Questions
#1. What is the prediction accuracy on the test set? 
#2. How does it compare to the prediction accuracy that you computed for the training set?

#4.3 Test Set MSE of the Logistic Regression Model
test_default_binary <- data.test[,1] #convert qualitative data to binary
test_default_binary <- ifelse(test_default_binary == "Yes", 1, 0)
test_mse_log <- mean((glm.pred.test - test_default_binary)^2)
print(test_mse_log)

#Questions: 
#1. What is the test set MSE for the logistic regression? 


# Step 5 - Implementing a Linear Discriminant Analysis  -------------------

# Fit the LDA model using student, balance, and income as predictors
library(MASS)
lda.fitted <- lda(default ~ student + balance + income, data = data.train)
summary(lda.fitted)
coef(lda.fitted)

#Questions
#1. What are the coefficients of the model? 

#5.1 Test Set Performance of the LDA Model 
lda.pred.test <- predict(lda.fitted, newdata = data.test)$class
print(lda.pred.test)

# Create confusion matrix
confusion_matrix <- table(lda.pred.test, data.test[,1])
print(confusion_matrix)

# Compute test set prediction accuracy
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
print(accuracy)

#Questions: 
#1. What is the prediction accuracy of the LDA model on the test set? 
#2. How does it compare to the prediction accuracy that you computed for the training set? 
#3. How does it compare to the test set prediction accuracy of the logistic regression?

#5.2 Test Set MSE of the LDA Model

# Calculate the test set MSE
test_mse_lda <- mean((as.numeric(lda.pred.test) - as.numeric(data.test$default))^2)
test_mse_lda

#Questions: 
#1. What is the test set MSE of the LDA model? 
#2. How does it compare to the test set MSE of the logistic regression?


# Step 6 - Implementing a QDA Model  --------------------------------------
# Fit the QDA model using student, balance, and income as predictors
qda.fitted <- qda(default ~ student + balance + income, data = data.train)
summary(qda.fitted)
coef(qda.fitted)

#Questions
#1. What are the coefficients of the model?

#6.1 Test Set Performance of the QDA Model

# Create vector of predictions for the test set
qda.pred.test <- predict(qda.fitted, newdata = data.test)$class
print(qda.pred.test)

# Create confusion matrix
confusion_matrix <- table(as.numeric(qda.pred.test), as.numeric(data.test[,1]))
print(confusion_matrix)

# Compute test set prediction accuracy
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
print(accuracy)

#Question
#1. What is the prediction accuracy of the QDA model on the test set? 
#2. How does it compare to the prediction accuracy that you computed for the training set? 
#3. How does it compare to the test set prediction accuracy of the logistic regression and of the LDA model?

#6.2 Test Set MSE of the QDA Model
test_mse_qda <- mean((as.numeric(qda.pred.test) - as.numeric(data.test$default))^2)
test_mse_qda

#Questions: 
#1. What is the test set MSE of the QDA model? 
#2. How does it compare to the test set MSE of the logistic regression and LDA models?


# Step 7 - Implementing a Classification Tree -----------------------------

library(tree)
tree.fitted <- tree(default ~ student + balance + income, data = data.train)
summary(tree.fitted)
plot(tree.fitted)
text(tree.fitted, pretty = 0)

#compute the test set MSE for the fitted tree
# Predict the default status for the test set using the tree model
tree.pred.test <- predict(tree.fitted, newdata = data.test)

# Calculate the test set MSE
test_mse_tree <- mean((as.numeric(tree.pred.test)-as.numeric(data.test$default))^2)
test_mse_tree

#Question:
#1. Explain the tree using the plot? 
#2. What is the test set MSE of the QDA model? 
#3. How does it compare to the test set MSE of the logistic regression and LDA models?

# Step 8 (Optional) - Implementing a -Nearest  ----------------------------

library(class)

data.train$student <- as.numeric(data.train$student)
data.test$student <- as.numeric(data.test$student)

# Fit five KNN models with k=1,3,5,7,9
k_values <- c(1, 3, 5, 7, 9)
knn.fitted <- lapply(k_values, function(k) {
  knn.fit <- knn(train = data.train[,c("student", "balance", "income")],
                 test = data.test[,c("student", "balance", "income")],
                 cl = data.train$default,
                 k = k)
  return(knn.fit)
})

# Compute confusion matrix and prediction accuracy for each model
for (i in seq_along(k_values)) {
  knn.pred.test <- knn.fitted[[i]]
  cm <- table(knn.pred.test, data.test$default)
  acc <- sum(diag(cm))/sum(cm)
  cat(sprintf("KNN Model with k=%d:\n", k_values[i]))
  cat(sprintf("Confusion Matrix:\n%s\n", cm))
  cat(sprintf("Test Set Prediction Accuracy: %f\n\n", acc))
}

# Compute test set MSE for each model
for (i in seq_along(k_values)) {
  knn.pred.test <- knn.fitted[[i]]
  mse <- mean((as.numeric(knn.pred.test) - as.numeric(data.test$default))^2)
  cat(sprintf("KNN Model with k=%d:\n", k_values[i]))
  cat(sprintf("Test Set MSE: %f\n\n", mse))
}

#Question
#1. What is the test set prediction accuracy of the KNN models for a choice of number of neighbours ? 
#2. What is the test set MSE for the KNN models for a choice of number of neighbours? 
#3. Which of the KNN model works best? Justify your answer? 
#4. How do the prediction accuracy and MSE of this model compare to the test set prediction accuracy and MSE of the logistic regression, LDA, QDA, and classification tree models? 

#Step 9 - Conclusion 

#Question
#1. Which model would you recommend, if any? 
#2. Justify your answer


