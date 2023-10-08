library(kknn)

data_train <- read.csv("Lab 2/train.csv", stringsAsFactors = FALSE)
data_test <- read.csv("Lab 2/test.csv", stringsAsFactors = FALSE)
y_test <- read.csv("Lab 2/gender_submission.csv", stringsAsFactors = FALSE)
data_test["Survived"] <- y_test["Survived"]

mean_age <- summary(data_test$Age)[4]
for (i in 1:dim(data_test)[1]) {
  if (is.na(data_test$Age[i])) {
    data_test$Age[i] <- mean_age
  }
}

n_row_train <- dim(data_train)[1]

kernel_list <- c("triangular", "rectangular", "epanechnikov",
                 "biweight", "triweight", "cos", "inv",
                 "guassian", "rank", "optimal")

valid_classifier <- train.kknn(Survived ~ Pclass + Sex + Age + SibSp + Parch,
                               data_train, k_max = 25, distance = 1, kernel = kernel_list)
parameters <- valid_classifier$best.parameters

KNN_classifier <- kknn(Survived ~ Pclass + Sex + Age + SibSp + Parch,
                       data_train, data_test, kernel = parameters$kernel,
                       k = parameters$k)
KNN_predicted <- fitted(KNN_classifier)

result <- table(KNN_predicted, data_test$Survived)

n_row_test <- dim(data_test)[1]
# (TP + TN)/test_size
accuraccy <- sum(result[, 1]) / n_row_test
print(accuraccy)
