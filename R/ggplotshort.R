ggplotshort <- function(colors, matrix, inputtitle, themevals) {
    ggplot2::ggplot(data = matrix, ggplot2::aes(x = x, y = y)) + ggplot2::geom_point(color = colors) + 
        ggplot2::theme_bw() + themevals + ggplot2::labs(title = inputtitle) + 
        ggplot2::xlab("x") + ggplot2::ylab("y")
}
