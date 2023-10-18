library(cluster)

data(animals)

write.csv(animals, "animals.csv")

animals_std <- scale(animals)
dist_matrix <- dist(animals_std)
plot(hclust(dist_matrix), main = "Dendrogram for votes.repub dataset")