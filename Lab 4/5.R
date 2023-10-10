library(tree)
library(maptree)
library(kknn)

# загрузка данных
data(glass)
# удаление первого столбца
glass <- glass[, -1]
# построение дерева решений
glass_tree <- tree(Type ~ ., data = glass, mindev = 0.01, minsize=25)
# визуализация дерева
draw.tree(glass_tree)
# определение типа стекла для экземпляра с характеристиками RI =1.516 Na =11.7 Mg =1.01 Al =1.19 Si =72.59 K=0.43 Ca =11.44 Ba =0.02 Fe =0.1
new_glass <- data.frame(RI = 1.516, Na = 11.7, Mg = 1.01, Al = 1.19, Si = 72.59, K = 0.43, Ca = 11.44, Ba = 0.02, Fe = 0.1)
predict(glass_tree, newdata = new_glass, type = "class")
