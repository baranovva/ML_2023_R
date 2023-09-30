library(e1071)

data_train <- read.csv("Lab 1/Titanic_train.csv")

data_test <- read.csv("Lab 1/Titanic_test.csv")
y_test <- read.csv("Lab 1/gender_submission.csv")
data_test["Survived"] <- y_test["Survived"]

n_row_train <- dim(data_train)[1]

NB <- naiveBayes(data_train[, -2], data_train$Survived)
predicted <- predict(NB, data_test)

result <- table(predicted, data_test$Survived)

n_row_test <- dim(data_test)[1]
# (TP + TN)/test_size
accuracy <- (result[1] + result[4]) / n_row_test
print(accuracy)
