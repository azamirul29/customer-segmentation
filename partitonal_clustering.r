library(readxl)
customer <- read_excel("customer.xlsx")
View(customer)
str(customer)
missing_values <- sum(is.na(customer))
cat("Number of missing values in the dataset:", missing_values, "\n")
if (missing_values > 0) {
cat("\nColumns with missing values:\n")
print(colnames(customer)[colSums(is.na(customer)) > 0])
customer <- customer[complete.cases(customer),]
cat("\nRows with missing values removed.\n")
} else {
cat("\nNo missing values found in the dataset.\n")
}
View(customer)
str(customer)
numeric_columns <- sapply(customer, is.numeric)
customer[numeric_columns] <- scale(customer[numeric_columns])
View(customer)
str(customer)
install.packages("ggplot2")
install.packages("factoextra")
selected_columns <- customer[, c("Age", "Work_Experience", "Family_Size")]
wss <- numeric(10)
for (i in 1:10) {
kmeans_model <- kmeans(selected_columns, centers = i)
wss[i] <- sum(kmeans_model$withinss)
}
plot(1:10, wss, type = "b", pch = 19, frame = FALSE, main = "Elbow Method",
xlab = "Number of Clusters (k)", ylab = "Within-cluster Sum of Squares")
optimal_k <- 3
kmeans_model <- kmeans(selected_columns, centers = optimal_k)
customer$Cluster <- as.factor(kmeans_model$cluster)
library(factoextra)
fviz_cluster(kmeans_model, data = selected_columns, geom = "point",
ellipse.type = "norm", ellipse.level = 0.95,
main = "K-Means Clustering")
kmeans_centers <- as.data.frame(kmeans_model$centers)
print(kmeans_centers)