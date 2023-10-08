library(kknn)

data_train <- read.table("Lab 2/svmdata4.txt", sep = "\t", stringsAsFactors = TRUE)
data_test <- read.table("Lab 2/svmdata4test.txt", sep = "\t", stringsAsFactors = TRUE)

plot(data_train$X1, data_train$X2, pch = 21, bg = c("red", "blue")
[unclass(data_train$Colors)], main = "My train data", xlab = "X1", ylab = "X2")
grid(nx = NULL, ny = NULL,
     lty = 2, col = "gray", lwd = 2)

KNN_classifier <- train.kknn(Colors ~ ., data_train, k_max = 25, distance = 1, kernel = "biweight")

KNN_classifier$best.parameters

mistake <- min(KNN_classifier$MISCLASS)
mistake
