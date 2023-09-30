library(e1071)
set.seed(12345)


segragete <- function(n_row, train_size, data) {
  data <- data[order(runif(n_row)),]

  data_train <- data[1:train_size,]
  data_test <- data[(train_size + 1):n_row,]

  return(list(data_train, data_test))
}


x1 <- c(rnorm(50, mean = 10, sd = 4), rnorm(50, mean = 20, sd = 3))
x2 <- c(rnorm(50, mean = 14, sd = 4), rnorm(50, mean = 18, sd = 3))

data <- data.frame(X1 = x1, X2 = x2, Class = c(rep("-1", 50), rep("1", 50)))
data$color[data$Class == "-1"] <- "blue"
data$color[data$Class == "1"] <- "red"

plot(x1, x2, col = data$color,
     xlab = "X1", ylab = "X2",
     pch = 20, cex = 2)

data <- data[-4]

n_column <- dim(data)[2]
train_size_list <- seq(0.1, 0.9, 0.1)
accuracy_list <- list()

for (size in train_size_list) {
  n_row <- dim(data)[1]
  train_size <- as.integer(n_row * size)

  data_prepared <- segragete(n_row, train_size, data)
  data_train <- data_prepared[[1]]
  data_test <- data_prepared[[2]]

  NBclassifier <- naiveBayes(data_train[, -n_column], data_train[, n_column])
  NBpredicted <- predict(NBclassifier, data_test)

  result <- table(NBpredicted, data_test$Class)
  test_size <- n_row - train_size
  # (TP + TN)/test_size
  accuracy <- (result[1] + result[4]) / test_size
  accuracy_list[[length(accuracy_list) + 1]] <- accuracy
}

plot(train_size_list, accuracy_list,
     type = "l", col = "red",
     xlab = "train size", ylab = "accuracy", lwd = 3)
grid(nx = NULL, ny = NULL,
     lty = 2, col = "gray", lwd = 2)
