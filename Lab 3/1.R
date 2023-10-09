library(e1071)
library(psych)

data_train <- read.table("Lab 3/svmdata1.txt", sep = "\t", stringsAsFactors = TRUE)
data_test <- read.table("Lab 3/svmdata1test.txt", sep = "\t", stringsAsFactors = TRUE)

SVM <- svm(Color ~ ., data = data_train, type = "C-classification", cost = 1, kerner = "linear")
SVM

plot(SVM, data_train, grid = 1000, symbolPallete = c("Green", "Red"))

n_row <- dim(data_test)[1]

res1_train <- predict(SVM, data_train)
result <- table(data_train$Color, res1_train)
accuracy <- tr(result) / n_row
accuracy

res1_test <- predict(SVM, data_test)
result <- table(data_test$Color, res1_test)
accuracy <- tr(result) / n_row
accuracy


