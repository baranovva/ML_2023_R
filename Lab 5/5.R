seeds <- read.table("Lab 5/seeds_dataset.txt", header=FALSE)

dist_matrix <- dist(seeds[,1:7], method="euclidean")
hc <- hclust(dist_matrix, method="ward.D2")
plot(hc, main="Dendrogram for seeds dataset")