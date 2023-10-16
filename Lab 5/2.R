library(cluster)

set.seed(123)
cluster1 <- cbind(rnorm(50, mean = 0, sd = 0.5), rnorm(50, mean = 0, sd = 2))
cluster2 <- cbind(rnorm(50, mean = 2, sd = 0.5), rnorm(50, mean = 2, sd = 2))
cluster3 <- cbind(rnorm(50, mean = -2, sd = 0.5), rnorm(50, mean = 2, sd = 2))
data <- rbind(cluster1, cluster2, cluster3)
plot(data, pch = 19)

clara_std_euclid <- clara(scale(data), 3, metric = "euclidean")
clara_std_manhattan <- clara(scale(data), 3, metric = "manhattan")
cat("Silhouette width for standardized data with Euclidean metric:", mean(silhouette(clara_std_euclid)))
cat("Silhouette width for standardized data with Manhattan metric:", mean(silhouette(clara_std_manhattan)))


clara_euclid <- clara(data, 3, metric = "euclidean")
clara_manhattan <- clara(data, 3, metric = "manhattan")
cat("Silhouette width for non-standardized data with Euclidean metric:", mean(silhouette(clara_euclid)))
cat("Silhouette width for non-standardized data with Manhattan metric:", mean(silhouette(clara_manhattan)))