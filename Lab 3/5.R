library(e1071)

data_train <- read.table("Lab 3/svmdata5.txt", sep = "\t", stringsAsFactors = TRUE)
data_test <- read.table("Lab 3/svmdata5test.txt", sep = "\t", stringsAsFactors = TRUE)

# radial
gamma_list <- seq(1, 101, 100)

for (gamma in gamma_list) {
  model <- svm(Colors ~ ., data = data_train, type = "C-classification",
               cost = 1, kerner = "radial", gamma = gamma)

  result <- table(data_test$Color, predict(model, data_test))
  accuracy <- (result[1] + result[4]) / sum(result)
  print(accuracy)

  plot(model, data_train, grid = 500, symbolPalette = c("Green", "Red"))
}

# polynomial and sibmoid
print('polynomial and sibmoid')
kernel_list <- c("polynomial", "sigmoid")
for (kernel in kernel_list) {
  model <- svm(Colors ~ ., data = data_train, type = "C-classification", cost = 1,
               kernel = kernel, gamma = 1, degree = 2)

  result <- table(data_test$Color, predict(model, data_test))
  accuracy <- (result[1] + result[4]) / sum(result)
  print(accuracy)
}