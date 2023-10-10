library(DAAG)
library(tree)
library(maptree)

# Загрузите набор данных spam7
data(spam7)

# Постройте дерево классификации
tree_model <- tree(yesno ~ ., data = spam7, mindev = 0.01, minsize = 1)
draw.tree(tree_model)

# Процедура "cost-complexity pruning" с параметром k по умолчанию и методом 'misclass'
for (i in seq(1, 10, 5)) {
  prune_seq <- prune.misclass(tree_model, k = i)
  draw.tree(prune_seq)
}

