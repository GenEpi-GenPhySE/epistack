#' mean_color
#'
#' @param colors vector of colors
#'
#' @return a color
#'
#' @examples
#' mean_color(c("#000000FF", "#FFFFFF00", "#FFFF00FF", "#FF0000FF"))
#' 
#' # works with color names
#' mean_color(c("blue", "red"))
#' 
#' Mix color names and HTML codes
#' mean_color(c("blue", "red", "#FFFF00FF"))
#' 
#' # works without alpha channel in inputs (but outputs an alpha channel):
#' mean_color(c("#000000", "#FFFFFF", "#FFFF00", "#FF0000"))

mean_color <- function(colors) {
    do.call(
        rgb,
        c(
            as.list(rowMeans(col2rgb(colors, alpha = TRUE))),
            maxColorValue = 255
        )
    )
}


