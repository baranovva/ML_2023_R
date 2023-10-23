library(rpart)
library(rpart)
library(maptree)

data_train <- read.csv("Lab 2/train.csv", stringsAsFactors = FALSE)
data_test <- read.csv("Lab 2/test.csv", stringsAsFactors = FALSE)
y_test <- read.csv("Lab 2/gender_submission.csv", stringsAsFactors = FALSE)
data_test["Survived"] <- y_test["Survived"]

age_mean <- function(data) {
  mean_age <- summary(data$Age)[4]
  for (i in 1:dim(data)[1]) {
    if (is.na(data$Age[i])) {
      data$Age[i] <- mean_age
    }
  }
  return(data)
}

data_test <- age_mean(data_test)
data_train <- age_mean(data_train)

tree_model <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch, data = data_train, method = 'class')

draw.tree(tree_model)

predicted_classes <- predict(tree_model, newdata = data_test, type = 'class')

confusion_matrix <- table(predicted_classes, data_test$Survived)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy
