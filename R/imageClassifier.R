imageClassifier <- function(url = "https://res.cloudinary.com/twenty20/private_images/t_low-crop/photosp/f985a04a-3f6a-4407-b3d8-722aac5eb6d8/f985a04a-3f6a-4407-b3d8-722aac5eb6d8.jpg", 
    numclust = 1) {
    dfile <- download.file(url, "ImagetoClust.jpg", mode = "wb")
    img <- jpeg::readJPEG("ImagetoClust.jpg")
    imgDm <- dim(img)
    imgRGB <- data.frame(x = rep(1:imgDm[2], each = imgDm[1]), y = rep(imgDm[1]:1, 
        imgDm[2]), R = as.vector(img[, , 1]), G = as.vector(img[, , 2]), B = as.vector(img[, 
        , 3]))
    # Plot the image
    themevals <- ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), 
        panel.border = ggplot2::element_blank(), panel.grid.major = ggplot2::element_blank(), 
        panel.grid.minor = ggplot2::element_blank(), axis.line = ggplot2::element_blank(), 
        axis.text.x = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(), 
        axis.ticks = ggplot2::element_blank(), axis.title.x = ggplot2::element_blank(), 
        axis.title.y = ggplot2::element_blank())
    plot1 <- ggplotshort(rgb(imgRGB[c("R", "G", "B")]), imgRGB, "Original", themevals)
    if (numclust%%1 != 0 || numclust < 2) {
        s <- sample(nrow(imgRGB), size = 5000, replace = FALSE)
        optimMat <- imgRGB[s, ]
        # Compute and plot wss for k = 2 to k = 15
        k.values <- 2:15
        # extract avg silhouette for 2-15 clusters
        avg_sil2 <- function(k.values) {
            avg_sil(k.values, optimMat)
        }
        avg_sil_values <- purrr::map_dbl(k.values, avg_sil2)
        numclust <- as.numeric(which.max(avg_sil_values) + 1)
    }
    kMeans <- kmeans(imgRGB[, c("R", "G", "B")], centers = numclust, nstart = 20)
    kColors <- rgb(kMeans$centers[kMeans$cluster, ])
    plot2 <- ggplotshort(kColors, imgRGB, paste("K-Means Clustering of", numclust, 
        "Colors"), themevals)
    ggplot2::ggsave("kmeanscomparison.jpg", gridExtra::arrangeGrob(plot1, plot2, 
        ncol = 2, nrow = 1))
    wd <- getwd()
    jpegpath <- paste0(wd, "/kmeanscomparison.jpg")
}
