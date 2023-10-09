library(e1071)
library(psych)

data_train <- read.table("Lab 3/svmdata1.txt", sep = "\t", stringsAsFactors = TRUE)
data_test <- read.table("Lab 3/svmdata1test.txt", sep = "\t", stringsAsFactors = TRUE)

SVM <- svm(Color ~ ., data = data_train, type = "C-classification", cost = 1, kerner = "linear")
SVM

n_row <- dim(data_test)[1]

result <- table(data_train$Color, predict(SVM, data_train))
accuracy <- tr(result) / n_row
accuracy

result <- table(data_test$Color, predict(SVM, data_test))
accuracy <- tr(result) / n_row
accuracy

plot(SVM, data_train, grid = 500, symbolPallete = c("Green", "Red"))

