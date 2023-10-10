library(tree)
library(maptree)
# загрузка данных
train_data <- read.table("Lab 3/svmdata4.txt", sep = "\t", stringsAsFactors = TRUE)
test_data <- read.table("Lab 4/svmdata4test.txt", sep = "\t", stringsAsFactors = TRUE)
# построение дерева решений
tree_model <- tree(Colors ~ ., data = train_data)
# визуализация дерева
draw.tree(tree_model)
# предсказание классов для тестовой выборки
predicted_classes <- predict(tree_model, newdata = test_data, type = "class")

# оценка точности классификации
confusion_matrix <- table(predicted_classes, test_data$Colors)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy