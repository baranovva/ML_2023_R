library(cluster)

data(animals)

animals_std <- scale(animals)
dist_matrix <- dist(animals_std)
plot(hclust(dist_matrix), main = "Dendrogram for animals dataset")