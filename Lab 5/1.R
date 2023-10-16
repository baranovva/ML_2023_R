library(cluster)
data(pluton)

kmeans3 <- kmeans(pluton, centers = 3)
silhouette(kmeans3$cluster, dist(pluton))

kmeans3_10iter <- kmeans(pluton, centers = 3, max.iter = 10)
silhouette(kmeans3_10iter$cluster, dist(pluton))

kmeans3_1iter <- kmeans(pluton, centers = 3, max.iter = 1)
silhouette(kmeans3_1iter$cluster, dist(pluton))