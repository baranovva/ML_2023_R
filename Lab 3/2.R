library(e1071)

data_train <- read.table("Lab 3/svmdata2.txt", sep = "\t", stringsAsFactors = TRUE)
data_test <- read.table("Lab 3/svmdata2test.txt", sep = "\t", stringsAsFactors = TRUE)

cost_list <- seq(0.01, 9, 0.01)

for (cost in cost_list) {
  model2 <- svm(Colors ~ ., data = data_train, type = "C-classification", cost = cost, kerner = "linear")

  train_table <- table(data_train$Color, predict(model2, data_train))
  test_table <- table(data_test$Color, predict(model2, data_test))

  accuracy_train <- (train_table[1] + train_table[4]) / sum(train_table)
  accuracy_test <- (test_table[1] + test_table[4]) / sum(test_table)

  if (accuracy_train == 1 | accuracy_test == 1) {
    print(c(cost, accuracy_train, accuracy_test))
  }
}

# 7.92 - 7.94   1.0, 1.0
