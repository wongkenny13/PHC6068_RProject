avg_sil <- function(k, matrix) {
    km.res <- kmeans(matrix, centers = k, nstart = 25)
    ss <- cluster::silhouette(km.res$cluster, dist(matrix))
    mean(ss[, 3])
}
