library(randomForest)
library(ggplot2)
library(caret)
library(Matrix)
library(pROC)
data1 <- read.csv("/Users/youniao/Desktop/FinalProjectData/Study_A.csv")
data2 <- read.csv("/Users/youniao/Desktop/FinalProjectData/Study_B.csv")
data3 <- read.csv("/Users/youniao/Desktop/FinalProjectData/Study_C.csv")
data4 <- read.csv("/Users/youniao/Desktop/FinalProjectData/Study_D.csv")
data5 <- read.csv("/Users/youniao/Desktop/FinalProjectData/Study_E.csv")

data_list <- list(data1, data2, data3, data4)
#Calculate Percentage of Passed
for (i in 1:length(data_list)) {
  percentage_passed <- (sum(data_list[[i]]$LeadStatus == "Passed") / nrow(data_list[[i]])) * 100
  print(paste("Percentage of LeadStatus Passed in data", i, ":", round(percentage_passed, 2), "%"))
}


data4$LeadStatus <- ifelse(data4$LeadStatus == "Passed", 0, 1)

set.seed(123)
trainIndex <- createDataPartition(data4$LeadStatus, p = 0.7, list = FALSE)
data_train <- data4[trainIndex,]
data_test <- data4[-trainIndex,]


rf_model <- randomForest(LeadStatus ~ VisitDay + PANSS_Total + P1 + P2 + P3 + P4 + P5 + P6 + P7 + N1 + N2 + N3 + N4 + N5 + N6 + N7 + G1 + G2 + G3 + G4 + G5 + G6 + G7 + G8 + G9 + G10 + G11 + G12 + G13 + G14 + G15 + G16, data = data_train, ntree = 150, mtry = 14, nodesize = 14)

importance(rf_model)

predictions <- predict(rf_model, data_test)
predictions_data5 <- predict(rf_model, data5)
data5$LeadStatus <- predictions_data5

# Create a result DataFrame with AssessmentID and Predicted_LeadStatus
result_data <- data.frame(
  AssessmentID = data5$AssessmentID,
  LeadStatus = data5$LeadStatus
)


write.csv(result_data, "/Users/youniao/Desktop/FinalProjectData/Study_E_Predictions.csv", row.names = FALSE)


#Plot Result
plot <- ggplot(data5, aes(x = VisitDay, y = PANSS_Total, color = LeadStatus)) + 
  geom_point(alpha = 0.6) +
  labs(title = "PANSS_Total vs VisitDay",
       x = "VisitDay",
       y = "PANSS_Total",
       color = "Lead Status") +
  theme_minimal() +
  scale_color_gradient(low = "lightblue", high = "darkred")

print(plot)



library(randomForest)
library(ggplot2)
library(caret)
library(Matrix)
library(pROC)
data1 <- read.csv("/Users/youniao/Desktop/FinalProjectData/Study_A.csv")
data2 <- read.csv("/Users/youniao/Desktop/FinalProjectData/Study_B.csv")
data3 <- read.csv("/Users/youniao/Desktop/FinalProjectData/Study_C.csv")
data4 <- read.csv("/Users/youniao/Desktop/FinalProjectData/Study_D.csv")
data5 <- read.csv("/Users/youniao/Desktop/FinalProjectData/Study_E.csv")




data4$LeadStatus <- ifelse(data4$LeadStatus == "Passed", 0, 1)
data4$LeadStatus <- as.numeric(data4$LeadStatus)

#Set Train and Test Set
set.seed(123)
trainIndex <- createDataPartition(data4$LeadStatus, p = 0.7, list = FALSE)
data_train <- data4[trainIndex,]
data_test <- data4[-trainIndex,]

# Hyper parameters
ntrees <- c(50, 100, 150, 200)
mtrys <- c(5, 10, 15, 20)
nodesizes <- c(1, 5, 10, 20, 25)

# Negative Log Likelihood loss function
nll_loss <- function(y_true, y_prob) {
  -mean(y_true * log(y_prob) + (1 - y_true) * log(1 - y_prob))
}
results <- data.frame()

# Tune
for (tree in ntrees) {
  for (m in mtrys) {
    for (n in nodesizes) {
      set.seed(123)
      rf_model <- randomForest(LeadStatus ~ ., data = data_train, ntree = tree, mtry = m, nodesize = n)
      predictions_prob <- predict(rf_model, data_test)
      loss <- nll_loss(data_test$LeadStatus, predictions_prob)
      results <- rbind(results, data.frame(ntree = tree, mtry = m, nodesize = n, loss = loss))
    }
  }
}
results <- results[order(results$loss),]

top5_models <- head(results, 5)
data_train$LeadStatus <- as.factor(data_train$LeadStatus)

#Cross validation
control <- trainControl(method = "cv", number = 5, savePredictions = "final", classProbs = TRUE)


top5_models <- head(results, 5)
folds <- createFolds(data_train$LeadStatus, k = 5)
cv_results <- list()

for (i in 1:nrow(top5_models)) {
  set.seed(123)

  fold_ROC <- numeric(length(folds))
  
  for (j in 1:length(folds)) {
    validation_data <- data_train[folds[[j]], ]
    training_data <- data_train[-folds[[j]], ]
    rf <- randomForest(LeadStatus ~ ., data = training_data, ntree = top5_models$ntree[i], mtry = top5_models$mtry[i], nodesize = top5_models$nodesize[i])
    predictions <- predict(rf, validation_data, type = "prob")[, 2]
    fold_ROC[j] <- roc(validation_data$LeadStatus, predictions)$auc
  }
  cv_results[[i]] <- mean(fold_ROC)
}

best_model_index <- which.max(unlist(cv_results))
best_parameters <- top5_models[best_model_index, ]
print(best_parameters)





