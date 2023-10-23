library(tree)
library(maptree)
library(rpart)

lenses <- read.table("Lab 4/Lenses.txt", header = FALSE, sep = "")
lenses <- lenses[, -1]
colnames(lenses) <- c("Age", "Vision", "Astigmatism", "Tears", "Type")

lenses_tree <- rpart(Type ~ ., data = lenses, method = 'class', control = rpart.control(minsplit = 5, maxdepth = 10))
draw.tree(lenses_tree)

new_patient <- data.frame(Age = 2, Vision = 1, Astigmatism = 2, Tears = 1)
pred <- predict(lenses_tree, new_patient, type = "class")
pred
