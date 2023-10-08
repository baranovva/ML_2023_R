library(kernlab)
library(kknn)
library(e1071)
library(psych)
library(dplyr)

set.seed(12345)


segragete <- function(n_row, train_size, data) {
  data <- data[order(runif(n_row)),]

  data_train <- data[1:train_size,]
  data_test <- data[(train_size + 1):n_row,]

  return(list(data_train, data_test))
}

data(glass)
data <- glass[, -1]

n_row <- dim(data)[1]
size <- 0.8
train_size <- as.integer(n_row * size)

data_prepared <- segragete(n_row, train_size, data)
data_train <- data_prepared[[1]]
data_test <- data_prepared[[2]]

# part 1
kernel_list <- c("triangular", "rectangular", "epanechnikov",
                 "biweight", "triweight", "cos", "inv",
                 "guassian", "rank", "optimal")


KNN_classifier <- train.kknn(Type ~ ., data_train, kmax = 25, kernel = kernel_list, distance = 1)
plot(KNN_classifier, lwd = 3, type = "b")
grid(nx = NULL, ny = NULL,
     lty = 2, col = "gray", lwd = 2)

# part 2
accuracy_list <- list()
distance_list <- seq(1, 10, 1)
test_size <- n_row - train_size

for (distance in distance_list) {
  KNN_classifier <- kknn(Type ~ ., k = 7, data_train, data_test, distance = distance, kernel = "optimal")
  KNN_predicted <- fitted(KNN_classifier)

  result <- table(KNN_predicted, data_test$Type)
  # (True classified)/test_size
  accuracy <- tr(result) / test_size
  accuracy_list[[length(accuracy_list) + 1]] <- accuracy
}

plot(distance_list, accuracy_list,
     type = "l", col = "red",
     xlab = "distance", ylab = "accuracy", lwd = 3)
grid(nx = NULL, ny = NULL,
     lty = 2, col = "gray", lwd = 2)

# part 3
sample <- data.frame(RI = 1.516, Na = 11.7, Mg = 1.01, Al = 1.19, Si = 72.59, K = 0.43,
                     Ca = 11.44, Ba = 0.02, Fe = 0.1)

KNN_classifier <- kknn(Type ~ ., data_train, sample, distance = 1, kernel = "biweight")
KNN_classifier$prob

# part 4
accuracy_list <- list()
test_size <- n_row - train_size
drop_list <- seq(1, 8, 1)

for (drop_index in drop_list) {
  data_train_dropped <- data_train[,-drop_index]
  data_test_dropped <- data_test[,-drop_index]

  KNN_classifier <- kknn(Type ~ ., k = 7, data_train_dropped, data_test_dropped, distance = 1, kernel = "biweight")
  KNN_predicted <- fitted(KNN_classifier)

  result <- table(KNN_predicted, data_test_dropped$Type)
  # (True classified)/test_size
  accuracy <- tr(result) / test_size
  accuracy_list[[length(accuracy_list) + 1]] <- accuracy
}

plot(drop_list, accuracy_list,
     type = "l", col = "red",
     xlab = "dropped kind", ylab = "accuracy", lwd = 3)
grid(nx = NULL, ny = NULL,
     lty = 2, col = "gray", lwd = 2)