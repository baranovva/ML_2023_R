library(kknn)
set.seed(12345)


segragete <- function(n_row, train_size, data) {
  data <- data[order(runif(n_row)),]

  data_train <- data[1:train_size,]
  data_test <- data[(train_size + 1):n_row,]

  return(list(data_train, data_test))
}


data <- read.table("Lab 1/Tic_tac_toe.txt", sep = ",", stringsAsFactors = TRUE)
train_size_list <- seq(0.05, 0.95, 0.05)
accuracy_list <- list()

for (size in train_size_list) {
  n_row <- dim(data)[1]
  train_size <- as.integer(n_row * size)

  data_prepared <- segragete(n_row, train_size, data)
  data_train <- data_prepared[[1]]
  data_test <- data_prepared[[2]]

  KNN_classifier <- kknn(V10 ~ ., data_train,data_test,distance=1,kernel="triangular")
  KNN_predicted <- fitted(KNN_classifier)

  result <- table(KNN_predicted, data_test$V10)
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
