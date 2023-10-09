library(e1071)
set.seed(12345)


segragete <- function(n_row, train_size, data) {
  data <- data[order(runif(n_row)),]

  data_train <- data[1:train_size,]
  data_test <- data[(train_size + 1):n_row,]

  return(list(data_train, data_test))
}

data <- read.table("Lab 3/svmdata3.txt", sep = "\t", stringsAsFactors = TRUE)
1
n_row <- dim(data)[1]
size <- 0.5
train_size <- as.integer(n_row * size)

data_prepared <- segragete(n_row, train_size, data)
data_train <- data_prepared[[1]]
data_test <- data_prepared[[2]]

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

