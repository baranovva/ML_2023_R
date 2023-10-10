library(DAAG)
library(rpart)
library(e1071)
library(caret)
library(ModelMetrics)

data(nsw74psid1)

# Заменяем пропущенные значения
nsw74psid1[is.na(nsw74psid1)] <- 0

# Разделение данных на обучающий и тестовый наборы
set.seed(12345)  # Установка случайного зерна для воспроизводимости результатов
train_index <- createDataPartition(nsw74psid1$re78, p = 0.8, list = FALSE)
train_data <- nsw74psid1[train_index,]
test_data <- nsw74psid1[-train_index,]

# Построение линейной регрессионной модели
lm_model <- lm(re78 ~ ., data = train_data)

# Построение регрессионного дерева
tree_model <- rpart(re78 ~ ., data = train_data)

# Построение SVM-регрессии
svm_model <- svm(re78 ~ ., data = train_data)

# Прогнозирование на тестовых данных
lm_pred <- predict(lm_model, newdata = test_data)
tree_pred <- predict(tree_model, newdata = test_data)
svm_pred <- predict(svm_model, newdata = test_data)

lm_mae <- mae(test_data$re78, lm_pred)
lm_mae
tree_mae <- mae(test_data$re78, tree_pred)
tree_mae
svm_mae <- mae(test_data$re78, svm_pred)
svm_mae
