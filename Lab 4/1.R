library(mlbench)
library(tree)
library(maptree)

data(Glass)

bc.tr <- tree(Type ~ ., data = Glass)
draw.tree(bc.tr)

bc.tr.optimized <- tree(Type ~ ., data = Glass, mindev = 0.01, minsize = 30)
draw.tree(bc.tr.optimized)

bc.tr.optimized <- prune.tree(bc.tr.optimized, k = 10)
draw.tree(bc.tr.optimized)