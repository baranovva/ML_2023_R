library(e1071)
library(ModelMetrics)

segragete <- function(n_row, train_size, data) {
  data <- data[order(runif(n_row)),]

  data_train <- data[1:train_size,]
  data_test <- data[(train_size + 1):n_row,]

  return(list(data_train, data_test))
}

data <- read.table("Lab 3/svmdata6.txt", sep = "\t", stringsAsFactors = TRUE)

n_row <- dim(data)[1]
size <- 0.8
train_size <- as.integer(n_row * size)

data_prepared <- segragete(n_row, train_size, data)
data_train <- data_prepared[[1]]
data_test <- data_prepared[[2]]

err_list <- list()
epsilon_list <- seq(0, 1, 0.05)

for (epsilon in epsilon_list) {
  model <- svm(Y ~ ., data = data_train, type = "eps-regression",
               cost = 1, kernel = "radial", epsilon = epsilon)

  err_list[[length(err_list) + 1]] <- mse(data_test$Y, predict(model, data_test))
}

plot(epsilon_list, err_list, col = "red", lwd = 3, type = 'b',
     xlab = "epsilon", ylab = "Среднеквадратичная ошибка")
grid(nx = NULL, ny = NULL,
     lty = 2, col = "gray", lwd = 2)