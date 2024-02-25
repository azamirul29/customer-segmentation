install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
selected_columns <- customer[, c("Age", "Work_Experience", "Family_Size")]
target_variable <- customer$Segmentation
decision_tree_model <- rpart(target_variable ~ ., data = selected_columns, method = "class")
rpart.plot(decision_tree_model, main = "Decision Tree for Customer Segmentation")
predictions <- predict(decision_tree_model, newdata = selected_columns, type = "class")
conf_matrix <- table(Actual = target_variable, Predicted = predictions)
print("Confusion Matrix:")
print(conf_matrix)
accuracy <- sum(diag(conf_matrix)) /  sum(conf_matrix)
print(paste("Accuracy:", round(accuracy, 4)))