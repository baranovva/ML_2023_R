library(cluster)

data(votes.repub)

votes.repub_std <- scale(votes.repub)
dist_matrix <- dist(votes.repub_std)

plot(hclust(dist_matrix), main = "Dendrogram for votes.repub dataset")


