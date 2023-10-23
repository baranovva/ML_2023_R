library(DAAG)
library(tree)
library(maptree)

data(spam7)

tree_model <- tree(yesno ~ ., data = spam7, mindev = 0.01, minsize = 1)
draw.tree(tree_model)

for (i in seq(1, 10, 5)) {
  prune_seq <- prune.misclass(tree_model, k = i)
  draw.tree(prune_seq)
}

