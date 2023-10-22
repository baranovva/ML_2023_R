library(cluster)
data(pluton)

set.seed(12345)

for (i in seq(2, 16, 2)) {
  kmeans3_1iter <- kmeans(pluton, centers = 3, iter.max = i)
  print(mean(silhouette(kmeans3_1iter$cluster, dist(pluton))[, 3]))
}
