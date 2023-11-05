# install.packages("caret")
# install.packages("randomForest")
# install.packages("FactoMineR")

library(caret)
library(randomForest)
library(FactoMineR)
data <- read.csv('loan_approval_dataset.csv')

# checking missing values
total_missing_values <- sum(is.na(data))

# drop missing values (even though there are not any)
data <- na.omit(data)

# transformation
data_ready <- data
data_ready$education <- as.integer(factor(data_ready$education))
data_ready$self_employed <- as.integer(factor(data_ready$self_employed))
data_ready$loan_status <- as.integer(factor(data_ready$loan_status))
data_ready$loan_status <- as.factor(data_ready$loan_status)
data_ready$loan_id <- NULL
data_ready

#handling outliers
boxplot(data_ready$income_annum, main = "Boxplot for Income Annum", ylab = "Values")
boxplot(data_ready$loan_amount, main = "Boxplot for Loan Amount", ylab = "Values")
boxplot(data_ready$loan_term, main = "Boxplot for Loan term", ylab = "Values")
boxplot(data_ready$cibil_score, main = "Boxplot for Cibil Score", ylab = "Values")
boxplot(data_ready$residential_assets_value, main = "Boxplot for Residential Asset Value", ylab = "Values") #this
boxplot(data_ready$commercial_assets_value, main = "Boxplot for Commercial Asset Value", ylab = "Values") #this
boxplot(data_ready$luxury_assets_value, main = "Boxplot for Luxury Assets Value", ylab = "Values")
boxplot(data_ready$bank_asset_value, main = "Boxplot for Bank Asset Value", ylab = "Values") #this

outliers_RAV <- boxplot.stats(data_ready$residential_assets_value)$out
outliers_CAV <- boxplot.stats(data_ready$commercial_assets_value)$out
outliers_BAV <- boxplot.stats(data_ready$bank_asset_value)$out

data_ready <- data_ready[!data_ready$residential_assets_value %in% outliers_RAV, ]
data_ready <- data_ready[!data_ready$commercial_assets_value %in% outliers_CAV, ]
data_ready <- data_ready[!data_ready$bank_asset_value %in% outliers_BAV, ]

boxplot(data_ready$residential_assets_value, main = "Boxplot for Residential Asset Value", ylab = "Values") #this
boxplot(data_ready$commercial_assets_value, main = "Boxplot for Commercial Asset Value", ylab = "Values") #this
boxplot(data_ready$bank_asset_value, main = "Boxplot for Bank Asset Value", ylab = "Values") #this

# # PCA
# independent_variables <- data_ready[, sapply(data_ready, is.numeric)]
# independent_variables <- independent_variables[ , !names(independent_variables) %in% c("loan_status")]
# 
# pca_results <- PCA(independent_variables, graph = FALSE, scale.unit = TRUE)
# 
# plot(pca_results$eig[,1], type = "b", main = "Scree Plot",
#      xlab = "Principal Components", ylab = "Eigenvalues",
#      col.main = "blue", pch = 19)
# 
# n_comp <- 2
# 
# pca_data <- as.data.frame(pca_results$ind$coord[, 1:n_comp])
# 
# pca_data$loan_status <- data_ready$loan_status

#Data split
set.seed(123)

trainIndex <- createDataPartition(data_ready$loan_status, p = 0.7, list = FALSE)
trainData <- data_ready[trainIndex, ]
testData <- data_ready[-trainIndex, ]

# creating model
rf_model <- randomForest(loan_status ~ ., data = trainData, ntree = 500)
rf_predictions <- predict(rf_model, testData)

# plotting the confusion matrix
cm <- confusionMatrix(rf_predictions, testData$loan_status)
# plot(as.table(cm$table), col = cm$byClass, main = paste("Confusion Matrix"))
cm_matrix <- as.data.frame(cm$table)
library(ggplot2)
ggplot(cm_matrix, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Confusion Matrix", x = "Actual Class", y = "Predicted Class") +
  theme_minimal()
