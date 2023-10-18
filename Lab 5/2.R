library(cluster)
library(flexclust)

set.seed(12345)

n <- 300
cluster1 <- cbind(rnorm(n, mean = 0, sd = 0.5), rnorm(n, mean = 0, sd = 2))
cluster2 <- cbind(rnorm(n, mean = 5, sd = 0.5), rnorm(n, mean = 0, sd = 2))
cluster3 <- cbind(rnorm(n, mean = -5, sd = 0.5), rnorm(n, mean = 0, sd = 2))
data <- rbind(cluster1, cluster2, cluster3)
plot(data, pch = 19)

clara_euclid <- clara(data, 3, metric = "euclidean")
clara_manhattan <- clara(data, 3, metric = "manhattan")
print(randIndex(clara_euclid$clustering, rep(1:3, each=n)), type="RI")
print(randIndex(clara_manhattan$clustering, rep(1:3, each=n)), type="RI")

clara_std_euclid <- clara(scale(data), 3, metric = "euclidean")
clara_std_manhattan <- clara(scale(data), 3, metric = "manhattan")
print(randIndex(clara_std_euclid$clustering, rep(1:3, each=n)), type="RI")
print(randIndex(clara_std_manhattan$clustering, rep(1:3, each=n)), type="RI")