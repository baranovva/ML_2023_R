library(e1071)
set.seed(12345)


data_train <- read.table("Lab 3/svmdata4.txt", sep = "\t", stringsAsFactors = TRUE)
data_test <- read.table("Lab 3/svmdata4test.txt", sep = "\t", stringsAsFactors = TRUE)

# part 1
print('part 1')
degree_list <- seq(1, 10, 1)

for (degree in degree_list) {
  model <- svm(Colors ~ ., data = data_train, type = "C-classification", cost = 1,
               kernel = "polynomial", gamma = 1, degree = degree)

  result <- table(data_test$Color, predict(model, data_test))
  accuracy <- (result[1] + result[4]) / sum(result)
  print(accuracy)
}

# part 2
print('part 2')
kernel_list <- c("radial", "sigmoid")
for (kernel in kernel_list) {
  model <- svm(Colors ~ ., data = data_train, type = "C-classification", cost = 1,
               kernel = kernel, gamma = 1)

  result <- table(data_test$Color, predict(model, data_test))
  accuracy <- (result[1] + result[4]) / sum(result)
  print(accuracy)
}

