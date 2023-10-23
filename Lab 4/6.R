library(tree)
library(maptree)

train_data <- read.table("Lab 3/svmdata4.txt", sep = "\t", stringsAsFactors = TRUE)
test_data <- read.table("Lab 4/svmdata4test.txt", sep = "\t", stringsAsFactors = TRUE)

tree_model <- tree(Colors ~ ., data = train_data)
draw.tree(tree_model)

predicted_classes <- predict(tree_model, newdata = test_data, type = "class")
confusion_matrix <- table(predicted_classes, test_data$Colors)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy